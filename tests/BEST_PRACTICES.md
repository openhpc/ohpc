# OpenHPC Test Framework Best Practices

This document describes the best practices for writing tests in the OpenHPC
`tests/` directory, derived from modernized test implementations across
libraries and performance tools.

## Table of Contents

1. [Directory Structure](#directory-structure)
2. [Test File Types](#test-file-types)
3. [BATS Test Framework](#bats-test-framework)
   - [The SIMPLE_CI Environment Variable](#the-simple_ci-environment-variable)
4. [Common Patterns](#common-patterns)
5. [CI Integration](#ci-integration)
6. [Code Formatting](#code-formatting)
   - [Shell Variable Quoting (IMPORTANT)](#shell-variable-quoting-important)

---

## Directory Structure

Each test component follows a standard directory layout:

```
tests/libs/<package>/
├── bootstrap -> ../../bootstrap      # Symlink to common bootstrap
├── configure.ac                      # Autoconf configuration
├── m4/                               # Autoconf macros
├── Makefile.am                       # Automake configuration
├── ohpc-tests/
│   ├── test_compiler_families        # Compiler family test runner
│   └── test_mpi_families             # MPI family test runner (if applicable)
└── tests/
    ├── Makefile.am                   # Test build configuration
    ├── rm_execution                  # Resource manager execution tests
    ├── test_module                   # Module validation tests
    └── *.c, *.cpp, *.f90             # Test source files
```

---

## Test File Types

### `test_module`

Validates that the module is properly installed and configured:
- Module is loaded and version matches RPM
- Environment variables (`${PKG}_DIR`, `${PKG}_LIB`, `${PKG}_INC`, etc.) are defined
- Required directories, libraries, and headers exist
- Binaries are executable and functional

### `rm_execution`

Tests binary execution under the resource manager (Slurm/OpenPBS):
- Serial binary execution
- MPI binary execution (if applicable)
- Fortran binary execution
- Validates compiled test programs run successfully

### `ohpc-tests/test_compiler_families` or `test_mpi_families`

Top-level test orchestration scripts that:
- Iterate over compiler/MPI families
- Load appropriate modules
- Configure, build, and run tests
- Save logs per family combination

---

## BATS Test Framework

All tests use the BATS (Bash Automated Testing System) framework with JUnit
reporting.

### Shebang and Parallel Execution

Use the modern shebang format with parallel execution flags:

```bash
#!/usr/bin/env -S bats --report-formatter junit --formatter tap -j 4
# -*-sh-*-
```

The `-j N` flag enables parallel test execution (adjust N based on test count).

### Loading Common Functions

Always source common test infrastructure:

```bash
# For rm_execution files (use 'load' for bats helper functions):
load ../../../common/test_helper_functions || exit 1
source ../../../common/functions || exit 1

if [ -s ../../../TEST_ENV ]; then
    source ../../../TEST_ENV
fi

# For test_module files:
source ../../../common/test_helper_functions.bash || exit 1
source ../../../common/functions || exit 1

if [ -s ../../../common/TEST_ENV ]; then
    source ../../../common/TEST_ENV
fi
```

### `setup_file()` Function

Use `setup_file()` for one-time initialization that runs before all tests:

```bash
setup_file() {
    # For rm_execution:
    NODES=2
    TASKS=$(tasks_count 8)
    ARGS=8
    TIMEOUT="00:05:00"
    TESTNAME="libs/FFTW"

    check_rms

    export NODES TASKS ARGS TIMEOUT TESTNAME
}
```

```bash
setup_file() {
    # For test_module:
    PKG=SUPERLU
    MODULE=superlu
    TESTNAME=libs/SuperLU
    LIBRARY=libsuperlu
    HEADER=slu_ddefs.h

    export PKG MODULE TESTNAME LIBRARY HEADER
}
```

### `setup()` and `teardown()` Functions

Use for per-test setup and cleanup (especially for temporary files):

```bash
setup() {
    OUTPUT="$(mktemp)"

    export OUTPUT
}

teardown() {
    rm -f "${OUTPUT}"
}
```

For conditional test skipping:

```bash
setup() {
    [[ -n "${SIMPLE_CI}" ]] && skip "Skipping tests in SIMPLE_CI mode"
}
```

### The `SIMPLE_CI` Environment Variable

The `SIMPLE_CI` environment variable indicates the test execution context:

| Value | Environment | Description |
|-------|-------------|-------------|
| Set (non-empty) | Container/GitHub Actions | Running in a container for local development or in GitHub Actions CI. Resources are limited. |
| Unset or `0` | Real cluster | Running on an actual HPC cluster with full resources available. |

**When `SIMPLE_CI` is set:**
- `tasks_count` returns 2 instead of the requested task count
- Tests requiring real hardware (e.g., performance counters, InfiniBand) should be skipped
- Timeouts may be extended to accommodate slower container environments

**Usage patterns:**

```bash
# Skip tests that require real cluster hardware
setup() {
    [[ -n "${SIMPLE_CI}" ]] && skip "Skipping in SIMPLE_CI mode"
}

# Use reduced task count in CI environments
TASKS=$(tasks_count 8)  # Returns 2 if SIMPLE_CI is set, 8 otherwise
```

### Test Function Format

Follow this naming convention for test functions:

```bash
@test "[${TESTNAME}] Description ($LMOD_FAMILY_COMPILER)" {
    # Test implementation
}
```

For MPI tests, include both compiler and MPI family:

```bash
@test "[${TESTNAME}] Description (${RESOURCE_MANAGER}/$LMOD_FAMILY_COMPILER/$LMOD_FAMILY_MPI)" {
    # Test implementation
}
```

### Binary Existence Checks

Always verify binaries exist before testing:

```bash
@test "[${TESTNAME}] Serial C binary runs under resource manager" {
    if [ ! -s C_test ]; then
        flunk "C_test binary does not exist"
    fi

    run_serial_binary -t "${TIMEOUT}" ./C_test
    assert_success
}
```

### Using Helper Functions

Use provided helper functions for common operations:

```bash
# Run serial binary with timeout
run_serial_binary -t "${TIMEOUT}" ./my_binary

# Run MPI binary with nodes/tasks configuration
run_mpi_binary -t "${TIMEOUT}" ./mpi_binary "${ARGS}" "${NODES}" "${TASKS}"

# Get RPM name for version checking
rpm=$(get_rpm_name "${MODULE}")

# Get task count (respects SIMPLE_CI environment)
TASKS=$(tasks_count 8)
```

---

## Common Patterns

### Module Version Verification

```bash
@test "[${TESTNAME}] Verify $PKG module is loaded and matches rpm version" {
    module list "${MODULE}" | grep "1) ${MODULE}" >&"${OUTPUT}" || exit 1
    run grep "${MODULE}" "${OUTPUT}"
    assert_success

    # check version against rpm
    local rpm
    rpm=$(get_rpm_name "${MODULE}")
    local version
    version="$(rpm -q --queryformat='%{VERSION}\n' "${rpm}")"
    run cat "${OUTPUT}"
    assert_output "  1) ${MODULE}/$version"
}
```

### Environment Variable Validation

```bash
@test "[${TESTNAME}] Verify ${PKG}_DIR is defined and exists" {
    PKG_DIR="${PKG}_DIR"

    if [ -z "${!PKG_DIR}" ]; then
        flunk "${PKG}_DIR directory not defined"
    fi

    if [ ! -d "${!PKG_DIR}" ]; then
        flunk "directory ${!PKG_DIR} does not exist"
    fi
}
```

### Library Existence Checks

```bash
@test "[${TESTNAME}] Verify dynamic library available in ${PKG}_LIB" {
    LIB="${PKG}_LIB"

    if [ -z "${!LIB}" ]; then
        flunk "${PKG}_LIB directory not defined"
    fi

    if [ ! -s "${!LIB}/${LIBRARY}.so" ]; then
        flunk "${LIBRARY}.so does not exist"
    fi
}

@test "[${TESTNAME}] Verify static library is not present in ${PKG}_LIB" {
    LIB="${PKG}_LIB"

    if [ -z "${!LIB}" ]; then
        flunk "${PKG}_LIB directory not defined"
    fi

    if [ -e "${!LIB}/${LIBRARY}.a" ]; then
        flunk "${LIBRARY}.a exists when not expecting it"
    fi
}
```

### Header File Validation

```bash
@test "[${TESTNAME}] Verify header file is present in ${PKG}_INC" {
    INC="${PKG}_INC"

    if [ -z "${!INC}" ]; then
        flunk "${PKG}_INC directory not defined"
    fi

    if [ ! -s "${!INC}/${HEADER}" ]; then
        flunk "${HEADER} file does not exist"
    fi
}
```

### MPI-Specific Configuration

Handle MPI-specific settings within tests:

```bash
@test "[libs/FFTW] MPI C binary runs under resource manager" {
    if [ ! -s C_mpi_test ]; then
        flunk "C_mpi_test binary does not exist"
    fi

    # choose specific pml for openmpi5
    if [[ "${LMOD_FAMILY_MPI}" == "openmpi5" ]]; then
        export OMPI_MCA_pml=ob1
    fi

    run_mpi_binary -t "${TIMEOUT}" ./C_mpi_test "${ARGS}" "${NODES}" "${TASKS}"
    assert_success
}
```

---

## CI Integration

### Adding Tests to Lint Targets

All test files must be added to the CI lint targets in `tests/ci/Makefile`:

1. **codespell-lint**: Spell checking
2. **whitespace-lint**: Trailing whitespace detection
3. **shellcheck-lint**: Shell script static analysis
4. **shfmt-lint**: Shell script formatting
5. **clang-format-lint**: C/C++ code formatting

Example additions to `tests/ci/Makefile`:

```makefile
# Add to codespell-lint target:
tests/libs/mypackage/tests/rm_execution \
tests/libs/mypackage/tests/test_module

# Add to shellcheck-lint target:
../../tests/libs/mypackage/tests/rm_execution \
../../tests/libs/mypackage/tests/test_module

# Add to clang-format-lint target:
../../tests/libs/mypackage/tests/*.c \
../../tests/libs/mypackage/tests/*.cpp
```

---

## Code Formatting

### Shell Variable Quoting (IMPORTANT)

**All shell variables MUST be quoted and use curly braces.** This is enforced
by shellcheck with the `require-variable-braces` and `quote-safe-variables`
options.

#### Correct Usage

```bash
# Always use curly braces AND double quotes:
echo "${VARIABLE}"
run_serial_binary -t "${TIMEOUT}" ./test
if [ -z "${!PKG_DIR}" ]; then
rm -f "${OUTPUT}"
module load "${MODULE}"
```

#### Incorrect Usage

```bash
# Missing curly braces - WRONG:
echo "$VARIABLE"
run_serial_binary -t "$TIMEOUT" ./test

# Missing quotes - WRONG:
echo ${VARIABLE}
rm -f ${OUTPUT}

# Missing both - WRONG:
echo $VARIABLE
```

#### Why This Matters

1. **Curly braces** (`${}`) prevent ambiguity in variable names and enable
   advanced expansions like indirect references (`${!VAR}`)
2. **Double quotes** prevent word splitting and glob expansion, avoiding bugs
   with filenames containing spaces or special characters
3. **shellcheck enforcement** ensures consistent style across all test scripts

### Shell Scripts

- Use tabs for indentation
- **Quote all variables with curly braces: `"${VARIABLE}"`**
- Use `${RESOURCE_MANAGER}` instead of local `$RM` variables
- Use `${LMOD_FAMILY_COMPILER}` and `${LMOD_FAMILY_MPI}` for family checks
- Pass shellcheck with options:
  `-o require-variable-braces,quote-safe-variables,deprecate-which,avoid-nullary-conditions`
- Format with `shfmt`

### C/C++ Source Files

- Format with `clang-format` using project style guide
- Follow kernel-style brace placement
- 80-character line limit
- Consistent spacing and pointer alignment

### Makefile.am

Use `LDADD` instead of `LDFLAGS` for library linking:

```makefile
# Correct:
mytest_LDADD = -lmylib

# Incorrect (libraries must come after object files):
# LDFLAGS += -lmylib
```

---

## Summary Checklist

When creating or modernizing tests:

- [ ] Use modern BATS shebang with parallel execution
- [ ] Source common functions from `../../../common/`
- [ ] Implement `setup_file()` for one-time initialization
- [ ] Implement `setup()`/`teardown()` for temp file handling
- [ ] Use `${RESOURCE_MANAGER}` variable (not local aliases)
- [ ] Use `get_rpm_name` helper for RPM version checks
- [ ] Use `tasks_count` helper for SIMPLE_CI compatibility
- [ ] Include timeout support with `-t` flag
- [ ] Quote all variables with curly braces: `"${VARIABLE}"`
- [ ] Add test files to all CI lint targets
- [ ] Format shell scripts with `shfmt`
- [ ] Format C/C++ files with `clang-format`
- [ ] Verify all lint checks pass
