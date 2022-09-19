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

error = False
spec_found = False
build_user = sys.argv[1]
dnf_based = False

# Check which base OS we are using
reader = csv.DictReader(open('/etc/os-release'), delimiter="=")

for row in reader:
    key = row.pop('NAME')
    if key in ['ID_LIKE', 'ID']:
        for item in list(row.items())[0]:
            if 'fedora' in item or 'openEuler' in item:
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
        'rpmbuild --rebuild %s' % src_rpm,
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


for spec in sys.argv[1:]:
    if not spec.endswith('.spec'):
        continue
    spec_found = True
    logging.info("\n--> Building RPM from spec file %s" % spec)

    just_spec = os.path.basename(spec)

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
        error = True
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
                error = True
    else:
        # Build SRPM
        command = [
            'misc/build_srpm.sh',
            spec,
        ]
        if not build_srpm_and_rpm(command):
            error = True


if not spec_found:
    logging.info("\nSKIP. Commit without changes to a SPEC file.")

if error:
    logging.error("\nERROR: Failure during RPM rebuild.")
    sys.exit(1)

logging.info("\nNo errors found. Exiting.")
sys.exit(0)
