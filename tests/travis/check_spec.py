#!/usr/bin/python3

# this script needs at least python 3.5 for
# subprocess.run()

import re
import subprocess
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
    '^%defattr\(-,root,root\).*',
    ]

if len(sys.argv) != 2:
    print("SKIP. Needs a git range as parameter")
    sys.exit(0)

command = ['git', 'diff', '--name-only', sys.argv[1]]

print("About to run command %s" % ' '.join(command))

result = subprocess.run(command, stdout=subprocess.PIPE)

regex_wrong_string = '(' + '|'.join(regex_wrong) + ')'

print("Checking that %s does not exist" % regex_wrong_string)

pattern = re.compile(regex_wrong_string)

if result.returncode != 0:
    sys.exit(1)

found = False
spec_found = False

for spec in result.stdout.decode('utf-8').split('\n'):
    if not spec.endswith('.spec'):
        continue
    spec_found = True
    with open(spec) as infile:
        for line in infile:
            if pattern.match(line):
                print("Found %s in %s" % (line.rstrip(), spec))
                found = True

if not spec_found:
    print("SKIP. Commit without changes to a SPEC file.")

if found:
    print("ERROR:Found an error in one of the SPEC files.")
    sys.exit(1)

print("No errors found. Exiting.")
sys.exit(0)
