#!/usr/bin/env python3

import subprocess
import selectors
import logging
import shutil
import sys
import csv
import os
import io
import time

logging.basicConfig(format='%(asctime)s %(message)s', level=logging.INFO)

if len(sys.argv) <= 2:
    print("Usage: %s <non-root-user> <list of files to check>" % sys.argv[0])
    sys.exit(0)

spec_found = False
build_user = sys.argv[1]
dnf_based = False
dist = "9999.ci.ohpc"
version_id = ''

# Check which base OS we are using
reader = csv.DictReader(open('/etc/os-release'), delimiter="=")

skip_ci_specs = []
skip_ci_specs_env = os.getenv('SKIP_CI_SPECS')
if skip_ci_specs_env:
    if os.getenv('CIRRUS_CI'):
        # CIRRUS CI splits multi-line evinronment variables
        # with a '\n'.
        skip_ci_specs = skip_ci_specs_env.rstrip().split('\n')
    else:
        # GitHub Actions with a space.
        skip_ci_specs = skip_ci_specs_env.rstrip().split(' ')

for row in reader:
    key = row.pop('NAME')
    if key in ['ID_LIKE', 'ID']:
        for item in list(row.items())[0]:
            if 'fedora' in item or 'openEuler' in item:
                dnf_based = True
    if key == 'VERSION_ID':
        version_id = list(row.items())[0][1]


def run_command(command):
    logging.info("About to run command %s" % ' '.join(command))
    process = subprocess.Popen(
        command,
        bufsize=1,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        universal_newlines=True,
    )

    buf = io.StringIO()

    def handle_output(stream, mask):
        line = stream.readline()
        buf.write(line)
        sys.stdout.write(line)

    selector = selectors.DefaultSelector()
    selector.register(
        process.stdout,
        selectors.EVENT_READ,
        handle_output,
    )

    while process.poll() is None:
        events = selector.select()
        for key, mask in events:
            callback = key.data
            callback(key.fileobj, mask)

    return_code = process.wait()
    selector.close()

    output = buf.getvalue()
    buf.close()

    return ((return_code == 0), output)


def loop_command(command, max_attempts=5):
    output = None
    attempt_counter = 0

    while True:
        attempt_counter += 1

        try:
            (success, output) = run_command(command)
            if success:
                return (True, output)
        except Exception as e:
            logging.error("Exception: %s" % e)

        if attempt_counter >= abs(max_attempts):
            return (False, output)

        logging.info("Retrying attempt '%i'" % attempt_counter)
        time.sleep(attempt_counter)


def build_srpm_and_rpm(command, family=None):
    success, output = run_command(command)
    if not success:
        # First check if the architecture is not supported
        if output is not None:
            for line in output.split('\n'):
                if 'No compatible architectures found for build' in line:
                    logging.info("Skipping unsupported architecture RPM")
                    return True

        logging.error("Running misc/build_srpm.sh failed")
        return False

    src_rpm = ""
    for line in output.split('\n'):
        if line.endswith('.src.rpm'):
            src_rpm = line
            break

    if src_rpm == "":
        logging.error("SRPM generation failed")
        return False

    logging.info(src_rpm)

    if dnf_based:
        builddep_command = [
            'dnf',
            '-y',
            'builddep',
            src_rpm,
        ]
    else:
        builddep_command = [
            'zypper',
            '-n',
            '--no-gpg-checks',
            'source-install',
            src_rpm,
        ]

    success, _ = loop_command(builddep_command)
    if not success:
        logging.error("Running '%s' failed" % ' '.join(builddep_command))
        return False

    tmp_src_rpm = os.path.join('/tmp', os.path.basename(src_rpm))

    try:
        os.unlink(tmp_src_rpm)
    except FileNotFoundError:
        pass
    shutil.move(src_rpm, '/tmp/')
    src_rpm = tmp_src_rpm

    rebuild_command = [
        'su',
        build_user,
        '-l',
        '-c',
        'rpmbuild --define "dist %s" --rebuild %s' % (dist, src_rpm),
    ]

    if family is not None:
        rebuild_command[-1] += " --define 'mpi_family %s'" % family

    # Disable parallel builds for boost on aarch64 to avoid OOM
    if "boost-" in src_rpm and os.uname().machine == "aarch64":
        rebuild_command[-1] += " --define '_smp_mflags -j1'"

    success, _ = run_command(rebuild_command)
    if not success:
        logging.error("Running 'rpmbuild --rebuild' failed")
        return False

    return True


skipped = []
failed = []
total = 0

for spec in sys.argv[1:]:
    if not spec.endswith('.spec'):
        continue
    just_spec = os.path.basename(spec)
    total += 1
    if spec in skip_ci_specs:
        logging.info("--> Skipping spec file %s" % spec)
        skipped.append(just_spec)
        continue

    spec_found = True
    logging.info("--> Building RPM from spec file %s" % spec)

    command = [
        'misc/get_source.sh',
        just_spec,
    ]

    success, _ = run_command(command)
    if not success:
        logging.error("Running misc/get_source.sh failed")
        failed.append(just_spec)
        continue

    # cache spec file contents
    infile = open(spec)
    contents = infile.read()
    infile.close()

    if 'ohpc_mpi_dependent' in contents:
        families = [
            'openmpi4',
            'mpich',
            'mvapich2',
        ]

        for family in families:
            if (family == 'mvapich2' and
                    (os.uname().machine == 'aarch64' or
                        version_id.startswith('9'))):
                continue
            # Build SRPM
            command = [
                'misc/build_srpm.sh',
                spec,
                family,
            ]
            if not build_srpm_and_rpm(command, family=family):
                failed.append(just_spec)
    else:
        # Build SRPM
        command = [
            'misc/build_srpm.sh',
            spec,
        ]
        if not build_srpm_and_rpm(command):
            failed.append(just_spec)


if not spec_found:
    logging.info("SKIP. Commit without changes to a SPEC file.")

logging.info("Found %d spec file(s)" % total)
logging.info("--> %d rebuild successfully" %
             (total - len(failed) - len(skipped)))
logging.info("--> %d rebuilds skipped" % len(skipped))

for skip in skipped:
    logging.info("----> %s skipped" % skip)

if len(failed) > 0:
    logging.error("--> %d rebuilds failed" % len(failed))
    for fail in failed:
        logging.info("----> %s failed" % fail)

    logging.error("ERROR")
    sys.exit(1)

logging.info("No errors found. Exiting.")
sys.exit(0)
