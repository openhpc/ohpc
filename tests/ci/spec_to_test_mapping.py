#!/usr/bin/env python3
#
# This script tries to create a mapping between spec files
# and which tests to enable in the test suite.
# This script will return three shell arrays (TESTS, ADMIN_TESTS and PKGS).

import sys
import csv
import os
import argparse

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
        'magpie-ohpc pdsh-mod-slurm-ohpc pdsh-ohpc'
    ],
    'components/dev-tools/hwloc/SPECS/hwloc.spec': [
        'hwloc',
        '',
        '',
    ],
    'components/rms/magpie/SPECS/magpie.spec': [
        'munge',
        '',
        'pdsh-mod-slurm-ohpc pdsh-ohpc'
    ],
    'components/admin/pdsh/SPECS/pdsh.spec': [
        'munge',
        '',
        'magpie-ohpc'
    ],
    'components/dev-tools/easybuild/SPECS/easybuild.spec': [
        'easybuild',
        '',
        'gcc-c++',
    ],
    'components/io-libs/adios2/SPECS/adios2.spec': [
        'adios2',
        '',
        ('openmpi5-COMPILER_FAMILY-ohpc '
         'mpich-COMPILER_FAMILY-ohpc '
         'python3-numpy-COMPILER_FAMILY-ohpc '
         'python3-mpi4py-COMPILER_FAMILY-mpich-ohpc '
         'python3-mpi4py-COMPILER_FAMILY-openmpi5-ohpc')
    ],
    'components/io-libs/hdf5/SPECS/hdf5.spec': [
        'hdf5',
        '',
        'zlib-devel automake-ohpc libtool-ohpc autoconf-ohpc'
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
        ''
    ],
    'components/parallel-libs/hypre/SPECS/hypre.spec': [
        'hypre',
        '',
        ''
    ],
    'components/parallel-libs/mfem/SPECS/mfem.spec': [
        'mfem',
        '',
        ''
    ],
    'components/parallel-libs/mumps/SPECS/mumps.spec': [
        'mumps',
        '',
        ''
    ],
    'components/parallel-libs/opencoarrays/SPECS/opencoarrays.spec': [
        'opencoarrays',
        '',
        ''
    ],
    'components/parallel-libs/petsc/SPECS/petsc.spec': [
        'petsc',
        '',
        ''
    ],
    'components/io-libs/phdf5/SPECS/hdf5.spec': [
        'phdf5',
        '',
        'zlib-devel automake-ohpc libtool-ohpc autoconf-ohpc'
    ],
    'components/io-libs/pnetcdf/SPECS/pnetcdf.spec': [
        'pnetcdf',
        '',
        ''
    ],
    'components/io-libs/netcdf/SPECS/netcdf.spec': [
        'netcdf',
        '',
        ''
    ],
    'components/parallel-libs/scalapack/SPECS/scalapack.spec': [
        'scalapack',
        '',
        ''
    ],
    'components/parallel-libs/slepc/SPECS/slepc.spec': [
        'slepc',
        '',
        ''
    ],
    'components/serial-libs/superlu/SPECS/superlu.spec': [
        'superlu',
        '',
        ''
    ],
    'components/parallel-libs/superlu_dist/SPECS/superlu_dist.spec': [
        'superlu_dist',
        '',
        ('scalapack-COMPILER_FAMILY-openmpi5-ohpc '
         'scalapack-COMPILER_FAMILY-mpich-ohpc')
    ],
    'components/parallel-libs/trilinos/SPECS/trilinos.spec': [
        'trilinos',
        '',
        ''
    ],
    'components/perf-tools/extrae/SPECS/extrae.spec': [
        'extrae',
        '',
        'lmod-defaults-COMPILER_FAMILY-openmpi5-ohpc'
    ],
    'components/perf-tools/geopm/SPECS/geopm.spec': [
        'geopm',
        '',
        'lmod-defaults-COMPILER_FAMILY-openmpi5-ohpc'
    ],
    'components/perf-tools/likwid/SPECS/likwid.spec': [
        'likwid',
        '',
        'lmod-defaults-COMPILER_FAMILY-openmpi5-ohpc'
    ],
    'components/perf-tools/papi/SPECS/papi.spec': [
        'papi',
        '',
        ''
    ],
    'components/perf-tools/scorep/SPECS/scorep.spec': [
        'scorep',
        '',
        'lmod-defaults-COMPILER_FAMILY-openmpi5-ohpc'
    ],
    'components/perf-tools/scalasca/SPECS/scalasca.spec': [
        'scalasca',
        '',
        'lmod-defaults-COMPILER_FAMILY-openmpi5-ohpc'
    ],
    'components/perf-tools/tau/SPECS/tau.spec': [
        'tau',
        '',
        ('lmod-defaults-COMPILER_FAMILY-openmpi5-ohpc '
         'man bc')
    ],
    'components/mpi-families/openmpi/SPECS/openmpi5.spec': [
        'slurm',
        '',
        ''
    ],
    'components/mpi-families/mpich/SPECS/mpich.spec': [
        'slurm',
        '',
        ''
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
        'automake-ohpc libtool-ohpc'
    ],
    'components/dev-tools/cmake/SPECS/cmake.spec': [
        'cmake',
        '',
        ''
    ],
    'components/parallel-libs/boost/SPECS/boost.spec': [
        'boost boost-mpi',
        '',
        ''
    ],
    'components/serial-libs/openblas/SPECS/openblas.spec': [
        'openblas',
        '',
        ''
    ],
    'components/serial-libs/R/SPECS/R.spec': [
        'R',
        '',
        ''
    ],
    'components/perf-tools/dimemas/SPECS/dimemas.spec': [
        'dimemas',
        '',
        'lmod-defaults-COMPILER_FAMILY-openmpi5-ohpc'
    ],
    'components/runtimes/charliecloud/SPECS/charliecloud.spec': [
        'charliecloud',
        '',
        'singularity-ce'
     ],
    'components/io-libs/netcdf-fortran/SPECS/netcdf-fortran.spec': [
        'netcdf',
        '',
        ('netcdf-cxx-COMPILER_FAMILY-openmpi5-ohpc '
         'netcdf-cxx-COMPILER_FAMILY-mpich-ohpc')
    ],
    'components/io-libs/netcdf-cxx/SPECS/netcdf-cxx.spec': [
        'netcdf',
        '',
        ('netcdf-fortran-COMPILER_FAMILY-openmpi5-ohpc '
         'netcdf-fortran-COMPILER_FAMILY-mpich-ohpc')
    ],
    'components/perf-tools/imb/SPECS/imb.spec': [
        'imb',
        '',
        ''
    ],
    'components/perf-tools/omb/SPECS/omb.spec': [
        'omb',
        '',
        ''
    ],
    'components/dev-tools/mpi4py/SPECS/python-mpi4py.spec': [
        'mpi4py',
        '',
        ''
    ],
    'components/serial-libs/gsl/SPECS/gsl.spec': [
        'gsl',
        '',
        ''
    ],
    'components/serial-libs/plasma/SPECS/plasma.spec': [
        'plasma',
        '',
        ''
    ],
    'components/dev-tools/valgrind/SPECS/valgrind.spec': [
        'valgrind',
        '',
        '',
    ],
    'components/serial-libs/metis/SPECS/metis.spec': [
        'metis',
        '',
        '',
    ],
    'components/admin/lmod/SPECS/lmod.spec': [
        '',
        'lmod',
        ''
    ],
}

# Check which base OS we are using
reader = csv.DictReader(open('/etc/os-release'), delimiter="=")

python_prefix = 'python3'

for row in reader:
    key = row.pop('NAME')
    if key in ['ID_LIKE', 'ID']:
        for item in list(row.items())[0]:
            if 'rhel' in item:
                python_prefix = 'python3.11'
                break
            if 'suse' in item:
                python_prefix = 'python311'
                break

skip_ci_specs = []
skip_ci_specs_env = os.getenv('SKIP_CI_SPECS')
if skip_ci_specs_env:
    skip_ci_specs = skip_ci_specs_env.rstrip().split()
for spec in skip_ci_specs:
    if spec in test_map:
        test_map.pop(spec)

parser = argparse.ArgumentParser()
parser.add_argument('--compiler-family', required=True, help='Compiler family')
parser.add_argument('specs', nargs='*')
args = parser.parse_args()

if not args.specs:
    print('TESTS=() ADMIN_TESTS=() PKGS=()')
    sys.exit(0)

tests = ''
admin_tests = ''
pkgs = ''

for i in args.specs:
    if i in test_map.keys():
        if len(tests) > 0:
            tests += ' '
        if len(admin_tests) > 0:
            admin_tests += ' '
        if len(pkgs) > 0:
            pkgs += ' '

        if len(test_map[i][0]) > 0:
            for test in test_map[i][0].split():
                tests += f'--enable-{test} '
        if len(test_map[i][1]) > 0:
            admin_tests += f'--enable-{test_map[i][1]}'
        pkgs += test_map[i][2]

pkgs = pkgs.replace('python3', python_prefix)

pkgs = pkgs.replace('COMPILER_FAMILY', args.compiler_family)

print(
    'TESTS=(%s) ADMIN_TESTS=(%s) PKGS=(%s)' % (
        tests,
        admin_tests,
        pkgs,
    )
)
