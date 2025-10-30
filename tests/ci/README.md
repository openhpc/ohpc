# OpenHPC Continuous Integration (CI) Tools

This directory contains scripts, tools, and configurations used for OpenHPC's continuous integration and testing infrastructure. These tools automate building, testing, and validation of OpenHPC components across multiple distributions and architectures.

## Core CI Scripts

### `run_build.py`
**Purpose**: Main CI build script that handles OpenHPC package building and testing.

**Features**:
- Builds source RPMs (SRPMs) and binary RPMs from spec files
- Supports multiple compiler families (GNU, Intel, ARM, LLVM)
- Handles multiple MPI implementations (OpenMPI, MPICH, MVAPICH2, Intel MPI)
- Automatic dependency resolution using `dnf builddep` or `zypper source-install`
- Detects MPI-dependent and compiler-dependent packages automatically
- Retry logic for network operations
- Special handling for architecture-specific builds

**Usage**:
```bash
# Build with default gnu15 compiler and openmpi5 MPI
./run_build.py ohpc-user path/to/component.spec

# Specify compiler family
./run_build.py ohpc-user --compiler-family=intel path/to/component.spec

# Specify MPI family
./run_build.py ohpc-user --mpi-family=mpich path/to/component.spec
```

**Environment Variables**:
- `SKIP_CI_SPECS`: Space-separated list of spec files to skip during CI

### `check_spec.py`
**Purpose**: Validates OpenHPC spec files against project coding standards and conventions.

**Validation Rules**:
- **Forbidden patterns**: Source42, OHPC_macros in Source lines, %changelog, DocDir, BuildRoot, %defattr, global PROJ_DELIM
- **Required elements**: Proper Group designation with %{PROJ_NAME}, URL field

**Usage**:
```bash
./check_spec.py path/to/component1.spec path/to/component2.spec
```

### `spec_to_test_mapping.py`
**Purpose**: Creates mapping between spec files and their corresponding test suites for automated testing.

**Features**:
- Maps spec files to test configure options (`--enable-*`)
- Identifies required packages for testing
- Handles admin tests vs. user tests
- Supports compiler family substitution in package names
- OS-specific Python prefix handling (python3 vs python311 vs python3.11)

**Usage**:
```bash
./spec_to_test_mapping.py --compiler-family=gnu15 components/libs/fftw/SPECS/fftw.spec
# Output: TESTS=(--enable-fftw) ADMIN_TESTS=() PKGS=()
```

## Environment Setup Scripts

### `prepare-ci-environment.sh`
**Purpose**: Configures CI environment with necessary dependencies and repositories.

**Features**:
- Multi-distribution support (RHEL/Rocky/AlmaLinux, SUSE/openSUSE, openEuler)
- Automatic package manager detection (dnf vs zypper)
- OpenHPC repository configuration
- Intel OneAPI toolkit integration
- Environment information reporting for debugging

**Usage**:
```bash
# Basic setup
./prepare-ci-environment.sh

# Enable Intel OneAPI
./prepare-ci-environment.sh intel

# Pre-release mode (skip OHPC_RELEASE)
./prepare-ci-environment.sh --pre-release
```

### `setup_slurm_and_run_tests.sh`
**Purpose**: Sets up Slurm cluster simulation and executes OpenHPC test suite.

**Features**:
- Multi-node Slurm cluster simulation (node0, node1)
- MUNGE authentication setup
- Test suite configuration and execution
- Package installation from rebuilt RPMs
- Comprehensive logging and error reporting

**Usage**:
```bash
./setup_slurm_and_run_tests.sh ohpc-user gnu15 components/libs/fftw/SPECS/fftw.spec
```

## Utility Scripts

### `cirrus_get_changed_files.sh`
**Purpose**: Retrieves list of changed files from GitHub pull requests for Cirrus CI.

**Features**:
- GitHub API integration
- Multi-commit PR support
- Deleted file handling
- Requires `CIRRUS_PR`, `CIRRUS_REPO_OWNER`, `CIRRUS_REPO_NAME` environment variables

**Usage**:
```bash
export CIRRUS_PR=123
export CIRRUS_REPO_OWNER=openhpc
export CIRRUS_REPO_NAME=ohpc
./cirrus_get_changed_files.sh
```

## Test Suites

### `test_parse_doc.bats`
**Purpose**: BATS test suite for validating documentation parsing scripts.

**Test Coverage**:
- LaTeX macro substitution (install, chrootinstall, etc.)
- Variable replacement (ARCH, TAG, OSTREE, BASEURL, OSNAME)
- Input file handling and nested includes
- HERE document processing
- Line continuation handling
- Comment and indentation processing
- CI-only command filtering
- Error handling for missing files/macros

**Usage**:
```bash
# Test Perl implementation (default)
bats test_parse_doc.bats

# Test Python implementation
PARSE_DOC_TEST_IMPL=python bats test_parse_doc.bats
```

## Build Automation

### `Makefile`
**Purpose**: Provides build targets for CI tasks and linting operations.

**Key Targets**:
- `clang-format-lint`: Code formatting validation using clang-format
- `build-ohpc-ci`: Build OHPC lint container locally with podman

**Usage**:
```bash
# Run clang-format linting
make -C tests/ci clang-format-lint

# Build lint container locally
make -C tests/ci build-ohpc-ci
```

### `Containerfile.ohpc-lint`
**Purpose**: Container definition for OpenHPC linting and code quality tools.

**Included Tools**:
- **codespell**: Spell checking for code
- **ruff**: Python linting and formatting
- **ShellCheck**: Shell script analysis
- **shfmt**: Shell script formatting
- **clang-tools-extra**: C/C++ code analysis tools
- **make**: Build automation
- **git**: Version control operations

**Usage**:
```bash
# Build container
podman build -f Containerfile.ohpc-lint -t ghcr.io/openhpc/ohpc-lint:latest

# Use in CI (automatic via GitHub Actions)
```

## CI Workflow Integration

These tools are designed to work together in CI pipelines:

1. **Environment Setup**: `prepare-ci-environment.sh` configures the build environment
2. **File Detection**: `cirrus_get_changed_files.sh` identifies modified files
3. **Spec Validation**: `check_spec.py` validates spec file standards
4. **Package Building**: `run_build.py` builds and tests packages
5. **Test Mapping**: `spec_to_test_mapping.py` determines required tests
6. **Test Execution**: `setup_slurm_and_run_tests.sh` runs comprehensive tests
7. **Documentation Testing**: `test_parse_doc.bats` validates documentation parsing

## Environment Variables

### Global CI Configuration
- `SKIP_CI_SPECS`: Space-separated list of spec files to skip
- `SIMPLE_CI`: Set to 1 for simplified CI mode
- `PARSE_DOC_TEST_IMPL`: Set to "python" to test Python parse_doc implementation

### Cirrus CI Specific
- `CIRRUS_PR`: Pull request number
- `CIRRUS_REPO_OWNER`: Repository owner (e.g., "openhpc")
- `CIRRUS_REPO_NAME`: Repository name (e.g., "ohpc")

## Supported Platforms

- **RHEL/Rocky/AlmaLinux**: EL8, EL9, EL10
- **SUSE/openSUSE**: Leap 15.x, Tumbleweed
- **openEuler**: 22.03, 24.03
- **Architectures**: x86_64, aarch64

## Error Handling and Debugging

All scripts include comprehensive error handling and logging:
- Retry logic for network operations
- Detailed environment reporting
- Log file collection and reporting
- Architecture-specific workarounds (e.g., single-threaded builds for ARM64)

## Contributing

When adding new components or modifying CI behavior:
1. Update `spec_to_test_mapping.py` for new testable components
2. Add any new forbidden patterns to `check_spec.py`
3. Update environment setup in `prepare-ci-environment.sh` for new dependencies
4. Add tests to `test_parse_doc.bats` for documentation parsing changes