#!/usr/bin/env python3
#
# This script tries to create a mapping between spec files
# and which tests to enable in the test suite.
# This script will return three shell arrays (TESTS, ADMIN_TESTS and PKGS).

import sys

# This dictionary defines the mapping
# 'path/to/file.spec': [
#     'test-option',
#     'admin-test-option',
#     'required packages for test',
# ]
test_map = {
    'components/rms/slurm/SPECS/slurm.spec': [
        'munge',
        '',
        'magpie-ohpc pdsh-mod-slurm-ohpc openmpi4-gnu12-ohpc pdsh-ohpc'
    ],
    'components/dev-tools/hwloc/SPECS/hwloc.spec': [
        'hwloc',
        '',
        'gnu12-compilers-ohpc',
    ],
    'components/rms/magpie/SPECS/magpie.spec': [
        'munge',
        '',
        'pdsh-mod-slurm-ohpc openmpi4-gnu12-ohpc pdsh-ohpc'
    ],
    'components/dev-tools/easybuild/SPECS/easybuild.spec': [
        'easybuild',
        '',
        'gcc-c++',
    ],
    'components/io-libs/adios/SPECS/adios.spec': [
        'adios',
        '',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/io-libs/hdf5/SPECS/hdf5.spec': [
        'hdf5',
        '',
        'zlib-devel'
    ],
    'components/parallel-libs/ptscotch/SPECS/ptscotch.spec': [
        'ptscotch',
        '',
        'zlib-devel'
    ],
    'components/serial-libs/scotch/SPECS/scotch.spec': [
        'scotch',
        '',
        'zlib-devel'
    ],
    'components/parallel-libs/fftw/SPECS/fftw.spec': [
        'fftw',
        '',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/parallel-libs/hypre/SPECS/hypre.spec': [
        'hypre',
        '',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/parallel-libs/mfem/SPECS/mfem.spec': [
        'mfem',
        '',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/parallel-libs/mumps/SPECS/mumps.spec': [
        'mumps',
        '',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/parallel-libs/opencoarrays/SPECS/opencoarrays.spec': [
        'opencoarrays',
        '',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/parallel-libs/petsc/SPECS/petsc.spec': [
        'petsc',
        '',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/io-libs/phdf5/SPECS/hdf5.spec': [
        'phdf5',
        '',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/io-libs/pnetcdf/SPECS/pnetcdf.spec': [
        'pnetcdf',
        '',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/parallel-libs/scalapack/SPECS/scalapack.spec': [
        'scalapack',
        '',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/parallel-libs/slepc/SPECS/slepc.spec': [
        'slepc',
        '',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/serial-libs/superlu/SPECS/superlu.spec': [
        'superlu',
        '',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/parallel-libs/superlu_dist/SPECS/superlu_dist.spec': [
        'superlu_dist',
        '',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/parallel-libs/trilinos/SPECS/trilinos.spec': [
        'trilinos',
        '',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/perf-tools/extrae/SPECS/extrae.spec': [
        'extrae',
        '',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/perf-tools/geopm/SPECS/geopm.spec': [
        'geopm',
        '',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/perf-tools/likwid/SPECS/likwid.spec': [
        'likwid',
        '',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/perf-tools/papi/SPECS/papi.spec': [
        'papi',
        '',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/perf-tools/scalasca/SPECS/scalasca.spec': [
        'scalasca',
        '',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/perf-tools/tau/SPECS/tau.spec': [
        'tau',
        '',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/mpi-families/openmpi/SPECS/openmpi.spec': [
        'openmpi4',
        '',
        'openmpi4-gnu12-ohpc'
    ],
    'components/mpi-families/mpich/SPECS/mpich.spec': [
        'mpich',
        '',
        'mpich-gnu12-ohpc'
    ],
    'components/dev-tools/spack/SPECS/spack.spec': [
        '',
        'spack',
        ''
    ],
    'components/admin/conman/SPECS/conman.spec': [
        '',
        'oob',
        ''
    ],
    'components/dev-tools/autoconf/SPECS/autoconf.spec': [
        'autotools',
        '',
        'autoconf-ohpc automake-ohpc libtool-ohpc'
    ],
    'components/dev-tools/cmake/SPECS/cmake.spec': [
        'cmake',
        '',
        'cmake-ohpc'
    ],
    'components/parallel-libs/boost/SPECS/boost.spec': [
        'boost',
        '',
        ''
    ],
}


if len(sys.argv) <= 1:
    print('TESTS=() ADMIN_TESTS=() PKGS=()')
    sys.exit(0)

tests = ''
admin_tests = ''
pkgs = ''

for i in sys.argv[1:]:
    if i in test_map.keys():
        if len(tests) > 0:
            tests += ' '
        if len(admin_tests) > 0:
            admin_tests += ' '
        if len(pkgs) > 0:
            pkgs += ' '

        if len(test_map[i][0]) > 0:
            tests += f'--enable-{test_map[i][0]}'
        if len(test_map[i][1]) > 0:
            admin_tests += f'--enable-{test_map[i][1]}'
        pkgs += test_map[i][2]

print(
    'TESTS=(%s) ADMIN_TESTS=(%s) PKGS=(%s)' % (
        tests,
        admin_tests,
        pkgs,
    )
)
