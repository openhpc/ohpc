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
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
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
    'components/parallel-libs/fftw/SPECS/fftw.spec': [
        'fftw',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/parallel-libs/hypre/SPECS/hypre.spec': [
        'hypre',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/parallel-libs/mfem/SPECS/mfem.spec': [
        'mfem',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/parallel-libs/mumps/SPECS/mumps.spec': [
        'mumps',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/parallel-libs/opencoarrays/SPECS/opencoarrays.spec': [
        'opencoarrays',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/parallel-libs/petsc/SPECS/petsc.spec': [
        'petsc',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/io-libs/phdf5/SPECS/hdf5.spec': [
        'phdf5',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/io-libs/pnetcdf/SPECS/pnetcdf.spec': [
        'pnetcdf',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/parallel-libs/scalapack/SPECS/scalapack.spec': [
        'scalapack',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/parallel-libs/slepc/SPECS/slepc.spec': [
        'slepc',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/serial-libs/superlu/SPECS/superlu.spec': [
        'superlu',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/parallel-libs/superlu_dist/SPECS/superlu_dist.spec': [
        'superlu_dist',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/parallel-libs/trilinos/SPECS/trilinos.spec': [
        'trilinos',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/perf-tools/extrae/SPECS/extrae.spec': [
        'extrae',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/perf-tools/geopm/SPECS/geopm.spec': [
        'geopm',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/perf-tools/likwid/SPECS/likwid.spec': [
        'likwid',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/perf-tools/papi/SPECS/papi.spec': [
        'papi',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/perf-tools/scalasca/SPECS/scalasca.spec': [
        'scalasca',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
    ],
    'components/perf-tools/tau/SPECS/tau.spec': [
        'tau',
        'openmpi4-gnu12-ohpc mpich-gnu12-ohpc'
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
