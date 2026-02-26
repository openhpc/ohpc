# check_for_package_updates.py

Check for newer upstream releases of OpenHPC packages by analyzing RPM
spec files under `components/`.

The script extracts the package name, version, and source URL from each
spec file using the
[specfile](https://github.com/rpm-software-management/specfile) Python
library, then queries the appropriate upstream source to find the latest
available version.

## Supported upstream sources

The following sources are tried in order for each package.  The first
match wins; if none match, [release-monitoring.org](https://release-monitoring.org/)
(Anitya) is used as a fallback.

| Source | Example packages |
|--------|-----------------|
| GitHub releases/tags | lmod, slurm, hwloc, pmix |
| GitLab tags | libfabric |
| PyPI | Cython, meson |
| JSC perftools | Score-P, Scalasca |
| UOregon TAU/PDT | tau, pdtoolkit |
| BSC tools FTP | dimemas, wxparaver |
| PnetCDF download page | pnetcdf |
| Open MPI GitHub tags | openmpi |
| GNU FTP mirror | gcc, gmp, mpc, mpfr |
| release-monitoring.org | cmake, gsl, valgrind, mpich |

## Requirements

The script must be run from the **OpenHPC top-level directory** (the one
containing `components/`).

Python dependencies:

- `requests`
- `specfile` (the RPM specfile library)
- `rpm` (Python bindings for librpm)

These are pre-installed in the OpenHPC analysis container.

## Usage

```
check_for_package_updates.py [-h] [-v] [-p] [-o {table,json,markdown}]
                              [-t TOKEN] [--no-glow] [--update]
                              [package]
```

### Options

| Option | Description |
|--------|-------------|
| `package` | Check only packages whose spec file path contains this string (case-insensitive) |
| `-v`, `--verbose` | Enable verbose output (shows which upstream source is queried) |
| `-p`, `--prereleases` | Include pre-release versions in checks |
| `-o`, `--output` | Output format: `table` (default), `json`, or `markdown` |
| `-t`, `--token` | GitHub API token (or set `GITHUB_TOKEN` env var) |
| `--no-glow` | Disable [glow](https://github.com/charmbracelet/glow) rendering for markdown output |
| `--update` | Automatically update spec files with the newer detected version |

### Exit codes

| Code | Meaning |
|------|---------|
| 0 | All packages are up to date |
| 1 | One or more packages have updates available |

## Examples

### Check all packages

```bash
python3 misc/check_for_package_updates.py
```

### Check a single package

Pass a substring that matches the spec file path:

```bash
python3 misc/check_for_package_updates.py gsl
```

```
PACKAGE                             CURRENT         LATEST                    STATUS               REPOSITORY
----------------------------------- --------------- ------------------------- -------------------- --------------------------------------------------
gsl-gnu15-ohpc                      2.8             2.8                       UP_TO_DATE           release-monitoring.org/1267
```

You can also use a longer path fragment:

```bash
python3 misc/check_for_package_updates.py components/serial-libs/gsl
```

### Check the GNU compiler suite

The GNU compilers spec file receives special handling -- each bundled
component (gcc, gmp, mpc, mpfr) is checked individually against the GNU
FTP mirror:

```bash
python3 misc/check_for_package_updates.py gnu-compilers
```

```
PACKAGE                             CURRENT         LATEST                    STATUS               REPOSITORY
----------------------------------- --------------- ------------------------- -------------------- --------------------------------------------------
gnu15-compilers-gcc                 15.2.0          15.2.0                    UP_TO_DATE           gnu.org/gcc
gnu15-compilers-gmp                 6.3.0           6.3.0                     UP_TO_DATE           gnu.org/gmp
gnu15-compilers-mpc                 1.3.1           1.3.1                     UP_TO_DATE           gnu.org/mpc
gnu15-compilers-mpfr                4.2.2           4.2.2                     UP_TO_DATE           gnu.org/mpfr
```

### Verbose output

Use `-v` to see which upstream source is being queried and the
comparison result:

```bash
python3 misc/check_for_package_updates.py -v cmake
```

```
[INFO] Scanning for OpenHPC packages with supported upstream sources...
[INFO] Found 1 spec files to process
[INFO] Processing cmake.spec
[INFO] Trying release-monitoring.org fallback for cmake-ohpc (project: id:306)
[INFO] Checking release-monitoring.org project ID: 306
[INFO] Result: cmake-ohpc 4.1.2 -> 4.2.3 (UPDATE_AVAILABLE)
[INFO] Processed 1 spec files
...
```

### JSON output

```bash
python3 misc/check_for_package_updates.py -o json gsl
```

```json
[
  {
    "name": "gsl-gnu15-ohpc",
    "current_version": "2.8",
    "latest_version": "2.8",
    "status": "UP_TO_DATE",
    "repository": "release-monitoring.org/1267",
    "spec_file": "components/serial-libs/gsl/SPECS/gsl.spec"
  }
]
```

### Markdown output

```bash
python3 misc/check_for_package_updates.py -o markdown --no-glow gsl
```

If [glow](https://github.com/charmbracelet/glow) is installed and
`--no-glow` is not set, the output is rendered in the terminal.
Otherwise raw Markdown is printed:

```markdown
# OpenHPC Package Release Status

| Package | Current | Latest | Status | Repository |
|---------|---------|--------|--------|------------|
| gsl-gnu15-ohpc | 2.8 | 2.8 | UP_TO_DATE | [release-monitoring.org/1267](...) |
```

### Update a spec file

When `--update` is passed, the script modifies any spec file where an
update is available.  For standard packages it uses the `specfile`
library to update the `Version:` tag.  For GNU compiler components it
updates the corresponding `%global` macro directly.

```bash
# See what would be updated (dry run -- omit --update)
python3 misc/check_for_package_updates.py cmake

# Apply the update
python3 misc/check_for_package_updates.py --update cmake

# Review the change
git diff components/dev-tools/cmake/SPECS/cmake.spec
```

### Using a GitHub token

Without a token the GitHub API allows only 60 requests per hour, which
is not enough for a full scan.  Set a token to raise the limit to
5,000 requests per hour:

```bash
export GITHUB_TOKEN=ghp_...
python3 misc/check_for_package_updates.py
```

Or pass it directly:

```bash
python3 misc/check_for_package_updates.py -t ghp_...
```

## Status values

| Status | Meaning |
|--------|---------|
| `UP_TO_DATE` | Current version matches the latest upstream release |
| `UPDATE_AVAILABLE` | A newer version is available upstream |
| `AHEAD` | The packaged version is newer than the latest upstream release |
| `ERROR` | Failed to query the upstream source |
| `RATE_LIMITED` | GitHub API rate limit was exceeded |
| `NOT_FOUND` | No upstream source could be identified |

## Adding upstream source mappings

Some packages have upstream names that differ from their OpenHPC names.
The `RELEASE_MONITORING_MAP` dictionary near the top of the script maps
stripped package names to release-monitoring.org project names or IDs:

```python
RELEASE_MONITORING_MAP = {
    "R": "id:386062",
    "omb": "osu-micro-benchmarks",
    "valgrind": "id:13639",
    ...
}
```

Use `id:<number>` to match by project ID when the name is ambiguous.
Packages with unique matching names on release-monitoring.org need no
entry.
