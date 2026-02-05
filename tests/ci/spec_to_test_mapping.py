#!/usr/bin/env python3
#
# This script tries to create a mapping between spec files
# and which tests to enable in the test suite.
# This script will return three shell arrays (TESTS, ADMIN_TESTS and PKGS).

import sys
import csv
import os
import argparse
import subprocess

# This dictionary defines the mapping
# 'path/to/file.spec': [
#     'test-option',
#     'admin-test-option',
#     'required packages for test',
# ]
test_map = {
    "components/rms/slurm/SPECS/slurm.spec": [
        "",
        "",
        "",
    ],
    "components/dev-tools/hwloc/SPECS/hwloc.spec": [
        "hwloc",
        "",
        "",
    ],
    "components/rms/magpie/SPECS/magpie.spec": [
        "munge",
        "",
        "pdsh-mod-slurm-ohpc pdsh-ohpc",
    ],
    "components/admin/pdsh/SPECS/pdsh.spec": ["munge pdsh", "", "magpie-ohpc"],
    "components/dev-tools/easybuild/SPECS/easybuild.spec": [
        "easybuild",
        "",
        ("gcc-c++", "openssl-devel"),
    ],
    "components/io-libs/adios2/SPECS/adios2.spec": [
        "adios2",
        "",
        (
            "openmpi5-COMPILER_FAMILY-ohpc "
            "mpich-COMPILER_FAMILY-ohpc "
            "python3-numpy-COMPILER_FAMILY-ohpc "
            "python3-mpi4py-COMPILER_FAMILY-mpich-ohpc "
            "python3-mpi4py-COMPILER_FAMILY-openmpi5-ohpc"
        ),
    ],
    "components/io-libs/hdf5/SPECS/hdf5.spec": [
        "hdf5 phdf5",
        "",
        "zlib-devel",
    ],
    "components/serial-libs/scotch/SPECS/scotch.spec": [
        "scotch ptscotch",
        "",
        "zlib-devel",
    ],
    "components/parallel-libs/fftw/SPECS/fftw.spec": ["fftw", "", ""],
    "components/parallel-libs/hypre/SPECS/hypre.spec": ["hypre", "", ""],
    "components/parallel-libs/mfem/SPECS/mfem.spec": ["mfem", "", ""],
    "components/parallel-libs/mumps/SPECS/mumps.spec": ["mumps", "", ""],
    "components/parallel-libs/petsc/SPECS/petsc.spec": ["petsc", "", ""],
    "components/io-libs/pnetcdf/SPECS/pnetcdf.spec": ["pnetcdf", "", ""],
    "components/io-libs/netcdf/SPECS/netcdf.spec": ["netcdf", "", ""],
    "components/parallel-libs/scalapack/SPECS/scalapack.spec": ["scalapack", "", ""],
    "components/parallel-libs/slepc/SPECS/slepc.spec": ["slepc", "", ""],
    "components/serial-libs/superlu/SPECS/superlu.spec": ["superlu", "", ""],
    "components/parallel-libs/superlu_dist/SPECS/superlu_dist.spec": [
        "superlu_dist",
        "",
        (
            "scalapack-COMPILER_FAMILY-openmpi5-ohpc "
            "scalapack-COMPILER_FAMILY-mpich-ohpc"
        ),
    ],
    "components/parallel-libs/trilinos/SPECS/trilinos.spec": ["trilinos", "", ""],
    "components/perf-tools/extrae/SPECS/extrae.spec": [
        "extrae",
        "",
        "lmod-defaults-COMPILER_FAMILY-openmpi5-ohpc",
    ],
    "components/perf-tools/likwid/SPECS/likwid.spec": [
        "likwid",
        "",
        "lmod-defaults-COMPILER_FAMILY-openmpi5-ohpc",
    ],
    "components/perf-tools/papi/SPECS/papi.spec": ["papi", "", ""],
    "components/perf-tools/scorep/SPECS/scorep.spec": [
        "scorep",
        "",
        "lmod-defaults-COMPILER_FAMILY-openmpi5-ohpc",
    ],
    "components/perf-tools/scalasca/SPECS/scalasca.spec": [
        "scalasca",
        "",
        "lmod-defaults-COMPILER_FAMILY-openmpi5-ohpc",
    ],
    "components/perf-tools/tau/SPECS/tau.spec": [
        "tau",
        "",
        ("lmod-defaults-COMPILER_FAMILY-openmpi5-ohpc man bc"),
    ],
    "components/mpi-families/openmpi/SPECS/openmpi5.spec": ["mpi", "", ""],
    "components/mpi-families/mpich/SPECS/mpich.spec": ["mpi", "", ""],
    "components/dev-tools/spack/SPECS/spack.spec": ["", "spack", ""],
    "components/admin/conman/SPECS/conman.spec": ["", "oob", ""],
    "components/dev-tools/autoconf/SPECS/autoconf.spec": [
        "autotools",
        "",
        "automake-ohpc libtool-ohpc",
    ],
    "components/dev-tools/cmake/SPECS/cmake.spec": ["cmake", "", ""],
    "components/parallel-libs/boost/SPECS/boost.spec": ["boost boost-mpi", "", ""],
    "components/serial-libs/openblas/SPECS/openblas.spec": ["openblas", "", ""],
    "components/serial-libs/R/SPECS/R.spec": ["R", "", ""],
    "components/perf-tools/dimemas/SPECS/dimemas.spec": [
        "dimemas",
        "",
        "lmod-defaults-COMPILER_FAMILY-openmpi5-ohpc",
    ],
    "components/runtimes/charliecloud/SPECS/charliecloud.spec": [
        "charliecloud",
        "",
        "singularity-ce",
    ],
    "components/io-libs/netcdf-fortran/SPECS/netcdf-fortran.spec": [
        "netcdf",
        "",
        (
            "netcdf-cxx-COMPILER_FAMILY-openmpi5-ohpc "
            "netcdf-cxx-COMPILER_FAMILY-mpich-ohpc"
        ),
    ],
    "components/io-libs/netcdf-cxx/SPECS/netcdf-cxx.spec": [
        "netcdf",
        "",
        (
            "netcdf-fortran-COMPILER_FAMILY-openmpi5-ohpc "
            "netcdf-fortran-COMPILER_FAMILY-mpich-ohpc"
        ),
    ],
    "components/perf-tools/imb/SPECS/imb.spec": ["imb", "", ""],
    "components/perf-tools/omb/SPECS/omb.spec": ["omb", "", ""],
    "components/dev-tools/mpi4py/SPECS/python-mpi4py.spec": ["mpi4py", "", ""],
    "components/serial-libs/gsl/SPECS/gsl.spec": ["gsl", "", ""],
    "components/serial-libs/plasma/SPECS/plasma.spec": ["plasma", "", ""],
    "components/dev-tools/valgrind/SPECS/valgrind.spec": [
        "valgrind",
        "",
        "",
    ],
    "components/serial-libs/metis/SPECS/metis.spec": [
        "metis",
        "",
        "",
    ],
    "components/admin/lmod/SPECS/lmod.spec": ["", "lmod", ""],
}

# Check which base OS we are using
reader = csv.DictReader(open("/etc/os-release"), delimiter="=")

python_prefix = "python3"

for row in reader:
    key = row.pop("NAME")
    if key in ["ID_LIKE", "ID"]:
        for item in list(row.items())[0]:
            if "rhel" in item:
                python_prefix = "python3.11"
                break
            if "suse" in item:
                python_prefix = "python311"
                break

skip_ci_specs = []
skip_ci_specs_env = os.getenv("SKIP_CI_SPECS")
if skip_ci_specs_env:
    skip_ci_specs = skip_ci_specs_env.rstrip().split()
for spec in skip_ci_specs:
    if spec in test_map:
        test_map.pop(spec)

parser = argparse.ArgumentParser()
parser.add_argument("--compiler-family", required=True, help="Compiler family")
parser.add_argument("specs", nargs="*")
args = parser.parse_args()

if not args.specs:
    print("TESTS=() ADMIN_TESTS=() PKGS=()")
    sys.exit(0)

# Packages to install when running ALL tests (not built during CI)
# Add additional packages to this list as needed
all_test_packages = [
    "charliecloud-ohpc",
    "cmake-ohpc",
    "conman-ohpc",
    "EasyBuild-ohpc",
    "losf-ohpc",
    "papi-ohpc",
    "pdsh-ohpc",
    "pdsh-mod-slurm-ohpc",
    "spack-ohpc",
    "valgrind-ohpc",
    # Compiler-only packages
    "gsl-COMPILER_FAMILY-ohpc",
    "hdf5-COMPILER_FAMILY-ohpc",
    "likwid-COMPILER_FAMILY-ohpc",
    "metis-COMPILER_FAMILY-ohpc",
    "netcdf-COMPILER_FAMILY-ohpc",
    "plasma-COMPILER_FAMILY-ohpc",
    "R-COMPILER_FAMILY-ohpc",
    "scotch-COMPILER_FAMILY-ohpc",
    "superlu-COMPILER_FAMILY-ohpc",
    # MPI packages (openmpi5 and mpich variants)
    "adios2-COMPILER_FAMILY-openmpi5-ohpc",
    "adios2-COMPILER_FAMILY-mpich-ohpc",
    "boost-COMPILER_FAMILY-openmpi5-ohpc",
    "boost-COMPILER_FAMILY-mpich-ohpc",
    "dimemas-COMPILER_FAMILY-openmpi5-ohpc",
    "dimemas-COMPILER_FAMILY-mpich-ohpc",
    "extrae-COMPILER_FAMILY-openmpi5-ohpc",
    "extrae-COMPILER_FAMILY-mpich-ohpc",
    "fftw-COMPILER_FAMILY-openmpi5-ohpc",
    "fftw-COMPILER_FAMILY-mpich-ohpc",
    "hypre-COMPILER_FAMILY-openmpi5-ohpc",
    "hypre-COMPILER_FAMILY-mpich-ohpc",
    "imb-COMPILER_FAMILY-openmpi5-ohpc",
    "imb-COMPILER_FAMILY-mpich-ohpc",
    "mfem-COMPILER_FAMILY-openmpi5-ohpc",
    "mfem-COMPILER_FAMILY-mpich-ohpc",
    "mumps-COMPILER_FAMILY-openmpi5-ohpc",
    "mumps-COMPILER_FAMILY-mpich-ohpc",
    "netcdf-COMPILER_FAMILY-openmpi5-ohpc",
    "netcdf-COMPILER_FAMILY-mpich-ohpc",
    "omb-COMPILER_FAMILY-openmpi5-ohpc",
    "omb-COMPILER_FAMILY-mpich-ohpc",
    "petsc-COMPILER_FAMILY-openmpi5-ohpc",
    "petsc-COMPILER_FAMILY-mpich-ohpc",
    "pnetcdf-COMPILER_FAMILY-openmpi5-ohpc",
    "pnetcdf-COMPILER_FAMILY-mpich-ohpc",
    "ptscotch-COMPILER_FAMILY-openmpi5-ohpc",
    "ptscotch-COMPILER_FAMILY-mpich-ohpc",
    "scalasca-COMPILER_FAMILY-openmpi5-ohpc",
    "scalasca-COMPILER_FAMILY-mpich-ohpc",
    "scorep-COMPILER_FAMILY-openmpi5-ohpc",
    "scorep-COMPILER_FAMILY-mpich-ohpc",
    "slepc-COMPILER_FAMILY-openmpi5-ohpc",
    "slepc-COMPILER_FAMILY-mpich-ohpc",
    "superlu_dist-COMPILER_FAMILY-openmpi5-ohpc",
    "superlu_dist-COMPILER_FAMILY-mpich-ohpc",
    "tau-COMPILER_FAMILY-openmpi5-ohpc",
    "tau-COMPILER_FAMILY-mpich-ohpc",
    "trilinos-COMPILER_FAMILY-openmpi5-ohpc",
    "trilinos-COMPILER_FAMILY-mpich-ohpc",
]

# If "ALL" is specified, use all known specs
if args.specs == ["ALL"]:
    specs_to_process = list(test_map.keys())
else:
    specs_to_process = args.specs

tests = ""
admin_tests = ""
pkgs = ""

for i in specs_to_process:
    if i in test_map.keys():
        if len(tests) > 0:
            tests += " "
        if len(admin_tests) > 0:
            admin_tests += " "
        if len(pkgs) > 0:
            pkgs += " "

        if len(test_map[i][0]) > 0:
            for test in test_map[i][0].split():
                tests += f"--enable-{test} "
        if len(test_map[i][1]) > 0:
            admin_tests += f"--enable-{test_map[i][1]}"
        pkg_entry = test_map[i][2]
        if isinstance(pkg_entry, tuple):
            pkgs += " ".join(pkg_entry)
        else:
            pkgs += pkg_entry

# Add additional packages when running ALL tests
if args.specs == ["ALL"] and all_test_packages:
    if pkgs:
        pkgs += " "
    pkgs += " ".join(all_test_packages)

# Get OpenHPC version - only replace python3 prefix for versions < 4
ohpc_major_version = 0
try:
    result = subprocess.run(
        ["rpm", "-q", "--queryformat=%{VERSION}", "ohpc-release"],
        capture_output=True,
        text=True,
        check=True,
    )
    version_str = result.stdout.strip()
    ohpc_major_version = int(version_str.split(".")[0])
except (subprocess.CalledProcessError, ValueError, IndexError):
    # If we can't determine the version, assume we need the replacement
    ohpc_major_version = 0

if ohpc_major_version < 4:
    pkgs = pkgs.replace("python3", python_prefix)

pkgs = pkgs.replace("COMPILER_FAMILY", args.compiler_family)

print(
    "TESTS=(%s) ADMIN_TESTS=(%s) PKGS=(%s)"
    % (
        tests,
        admin_tests,
        pkgs,
    )
)
