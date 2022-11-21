#!/usr/bin/env python3

import csv
import subprocess
import logging
import shutil
import sys
import os

logging.basicConfig(format='%(asctime)s %(message)s', level=logging.INFO)

if len(sys.argv) <= 2:
    print("Usage: %s <non-root-user> <list of files to check>" % sys.argv[0])
    sys.exit(0)

spec_found = False
build_user = sys.argv[1]
dnf_based = False
dist = "9999.ci.ohpc"

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
    if key == 'ID_LIKE':
        for item in list(row.items())[0]:
            if 'fedora' in item:
                dnf_based = True


def build_srpm_and_rpm(command, family=None):
    logging.info("About to run command %s" % ' '.join(command))
    result = subprocess.run(
        command,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    if result.returncode != 0:
        logging.error("Running misc/build_srpm.sh failed")
        logging.error(result.stdout.decode('utf-8'))
        logging.error(result.stderr.decode('utf-8'))
        return False

    if result.stderr is not None:
        for line in result.stderr.decode('utf-8').split('\n'):
            if 'No compatible architectures found for build' in line:
                logging.info("Skipping unsupported architecture RPM")
                return True

    src_rpm = ""
    for line in result.stdout.decode('utf-8').split('\n'):
        if line.endswith('.src.rpm'):
            src_rpm = line
            break

    if src_rpm == "":
        logging.error("SRPM generation failed")
        logging.error(result.stdout.decode('utf-8'))
        logging.error(result.stderr.decode('utf-8'))
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

    logging.info("About to run command %s" % ' '.join(builddep_command))
    result = subprocess.run(
        builddep_command,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    if result.returncode != 0:
        logging.error("Running 'dnf builddep' failed")
        logging.error(result.stdout.decode('utf-8'))
        logging.error(result.stderr.decode('utf-8'))
        return False

    logging.info(result.stdout.decode('utf-8'))

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

    logging.info("About to run command %s" % ' '.join(rebuild_command))
    result = subprocess.run(
        rebuild_command,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    if result.returncode != 0:
        logging.info("Running 'rpmbuild --rebuild' failed")
        logging.error(result.stdout.decode('utf-8'))
        logging.error(result.stderr.decode('utf-8'))
        return False

    logging.info(result.stdout.decode('utf-8'))

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

    logging.info("About to run command %s" % ' '.join(command))
    result = subprocess.run(
        command,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    if result.returncode != 0:
        logging.error("Running misc/get_source.sh failed")
        logging.error(result.stdout.decode('utf-8'))
        logging.error(result.stderr.decode('utf-8'))
        failed += 1
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
            if family == 'mvapich2' and os.uname().machine == 'aarch64':
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
