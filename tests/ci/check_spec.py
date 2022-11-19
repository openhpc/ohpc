#!/usr/bin/env python3

import os
import re
import sys

regex_wrong = [
    # make sure there is no Source42
    '^Source42.*$',
    # make sure OHPC_macros is not listed
    '^Source.*OHPC_macros$',
    # make sure there is no %changelog
    '^%changelog.*',
    # make sure there is no DocDir
    '^DocDir.*',
    # make sure there is no BuildRoot
    '^BuildRoot.*',
    # make sure there is no %defattr(-,root,root)
    '^%defattr(-,root,root).*',
    # make sure there is no global definition of PROJ_DELIM
    '^.*%global.*PROJ_DELIM.*$',
    ]

regex_required = [
    # Group designation should include %{PROJ_NAME} delimiter and
    # known component area
    '^Group:.*%{PROJ_NAME}/(admin|compiler-families|dev-tools|distro-packages|'
    'io-libs|lustre|meta-package|mpi-families|parallel-libs|perf-tools|'
    'provisioning|rms|runtimes|serial-libs)$',
    # Need a URL
    '(^URL:.*$|Url:.*$)',
    ]

if len(sys.argv) <= 1:
    print("SKIP. Needs a list of files to check")
    sys.exit(0)

regex_wrong_string = '(' + '|'.join(regex_wrong) + ')'

print("Checking that %s does not exist" % regex_wrong_string)

pattern = re.compile(regex_wrong_string)

error = False
spec_found = False

skip_ci_specs = []
skip_ci_specs_env = os.getenv('SKIP_CI_SPECS')
if skip_ci_specs_env:
    skip_ci_specs = skip_ci_specs_env.rstrip().split(' ')

for spec in sys.argv[1:]:
    if not spec.endswith('.spec'):
        continue
    if spec in skip_ci_specs:
        print("--> Skipping spec file %s" % spec)
        continue
    spec_found = True
    print("--> Scanning spec file %s" % spec)

    # cache spec file contents
    infile = open(spec)
    contents = infile.read()
    infile.close()

    # first, verify patterns which should *not* be present
    for line in contents.split('\n'):
        if pattern.match(line):
            print("    [+] Found %s" % (line.rstrip()))
            error = True

    # next, verify items which should be present
    for requirement in regex_required:
        if not re.findall(requirement, contents, re.MULTILINE):
            print("    [-] Missing %s" % requirement)
            error = True

if not spec_found:
    print("\nSKIP. Commit without changes to a SPEC file.")

if error:
    print("\nERROR: Found an inconsistency in one or more SPEC files.")
    sys.exit(1)

print("\nNo consistency errors found. Exiting.")
sys.exit(0)
