#!/usr/bin/env python3
#
# This script tries to create a mapping between spec files
# and which tests to enable in the test suite.
# This script will return two shell arrays (TESTS and PKGS).

import sys

# This dictionary defines the mapping
# 'path/to/file.spec': ['test-option', 'required packages for test']
test_map = {
    # 'path/to/file.spec': ['test-option', 'required packages test']
    'components/rms/slurm/SPECS/slurm.spec': [
        'munge',
        'magpie-ohpc pdsh-mod-slurm-ohpc openmpi4-gnu12-ohpc pdsh-ohpc'
    ],
    'components/dev-tools/hwloc/SPECS/hwloc.spec': [
        'hwloc',
        'gnu12-compilers-ohpc',
    ],
    'components/rms/magpie/SPECS/magpie.spec': [
        'munge',
        'pdsh-mod-slurm-ohpc openmpi4-gnu12-ohpc pdsh-ohpc'
    ],
    'components/dev-tools/easybuild/SPECS/easybuild.spec': [
        'easybuild',
        'gcc-c++',
    ],
    'components/io-libs/adios/SPECS/adios.spec': [
        'adios',
        ''
    ],
    'components/io-libs/hdf5/SPECS/hdf5.spec': [
        'hdf5',
        'zlib-devel'
    ],
    'components/parallel-libs/ptscotch/SPECS/ptscotch.spec': [
        'ptscotch',
        'zlib-devel'
    ],
    'components/serial-libs/scotch/SPECS/scotch.spec': [
        'scotch',
        'zlib-devel'
    ],
}


if len(sys.argv) <= 1:
    print('TESTS=() PKGS=()')
    sys.exit(0)

tests = ''
pkgs = ''

for i in sys.argv[1:]:
    if i in test_map.keys():
        if len(tests) > 0:
            tests += ' '
        if len(pkgs) > 0:
            pkgs += ' '

        tests += f'--enable-{test_map[i][0]}'
        pkgs += test_map[i][1]

print(
    'TESTS=(%s) PKGS=(%s)' % (
        tests,
        pkgs,
    )
)
