#!/usr/bin/env python3
"""
Generate Package Manifest tables from pkg-ohpc.all and meta-ohpc.all.

Reads raw package data files from a manifest directory and produces
markdown tables for inclusion by the manifest template.

Optionally generates .all files by querying the system package manager.

Usage:
    python generate_manifest.py manifests/el10-x86_64
    python generate_manifest.py manifests/el10-aarch64

    # Generate .all files first (requires dnf/rpm on target system)
    python generate_manifest.py manifests/el10-x86_64 --generate-all
    lima python3 generate_manifest.py manifests/el10-aarch64 --generate-all

Input files (in the specified directory):
    pkg-ohpc.all   - Package data: name version url category summary
    meta-ohpc.all  - Meta-package groups: name description

Output (in the same directory):
    meta-ohpc.md    - Meta-package table
    pkg-ohpc.md.j2  - Per-category package tables with Jinja2 conditionals
"""

import argparse
import re
import subprocess
import sys
import yaml
from dataclasses import dataclass
from pathlib import Path


# ---------------------------------------------------------------------------
# Configuration Loading
# ---------------------------------------------------------------------------

def load_config() -> dict:
    """Load configuration from manifests/config.yaml."""
    config_path = Path(__file__).parent / "manifests" / "config.yaml"
    with open(config_path, 'r', encoding='utf-8') as f:
        return yaml.safe_load(f)


_CONFIG = load_config()

# Load configuration into module-level constants
COMPILER_FAMILIES = _CONFIG['compiler_families']
MPI_FAMILIES = _CONFIG['mpi_families']
EXCLUDE_PACKAGES = set(_CONFIG['exclude_packages'])
CATEGORY_ORDER = list(_CONFIG['categories'].items())


# ---------------------------------------------------------------------------
# Data Structures
# ---------------------------------------------------------------------------

@dataclass
class Package:
    name: str
    version: str
    url: str
    category: str
    summary: str


@dataclass
class PackageGroup:
    names: list[str]
    version: str
    url: str
    summary: str


# ---------------------------------------------------------------------------
# Parsing
# ---------------------------------------------------------------------------

def parse_pkg_ohpc(filepath: Path) -> list[Package]:
    """Parse pkg-ohpc.all into a list of Package objects.

    Format: name version url ohpc/category summary text
    Lines without ohpc/ category prefix are skipped.
    """
    packages = []
    with open(filepath, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            match = re.match(
                r"^(\S+)\s+(\S+)\s+(\S+)\s+(ohpc/\S+)\s+(.+)$", line
            )
            if not match:
                continue
            name, version, url, category_full, summary = match.groups()
            category = category_full.split("/", 1)[1]
            packages.append(Package(name, version, url, category, summary))
    return packages


def parse_meta_ohpc(filepath: Path) -> list[tuple[str, str]]:
    """Parse meta-ohpc.all into (name, description) tuples.

    Format: group-name Description text
    """
    patterns = []
    with open(filepath, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            match = re.match(r"^(\S+)\s+(.+)$", line)
            if match:
                patterns.append((match.group(1), match.group(2)))
    return patterns


# ---------------------------------------------------------------------------
# Package Family Classification and Grouping
# ---------------------------------------------------------------------------

def classify_package(name: str) -> tuple[str, bool]:
    """Determine if a package is a compiler/MPI family variant.

    Returns (base_name, is_family).  The base_name is the package name
    prefix before the compiler/MPI suffix.
    """
    # Try MPI family first (more specific: base-compiler-mpi-ohpc)
    for compiler in COMPILER_FAMILIES:
        for mpi in MPI_FAMILIES:
            suffix = f"-{compiler}-{mpi}-ohpc"
            if name.endswith(suffix):
                return name[: -len(suffix)], True

    # Try compiler family (base-compiler-ohpc)
    for compiler in COMPILER_FAMILIES:
        suffix = f"-{compiler}-ohpc"
        if name.endswith(suffix):
            return name[: -len(suffix)], True

    return name, False


def group_packages(packages: list[Package]) -> list[PackageGroup]:
    """Group consecutive compiler/MPI family variants.

    Packages sharing the same base name are combined into a single
    PackageGroup with <br>-separated names.  Input must be sorted
    (as produced by listohpc).

    This mirrors the grouping logic in common/build_tables.pl.
    """
    groups: list[PackageGroup] = []
    i = 0

    while i < len(packages):
        pkg = packages[i]

        if pkg.name in EXCLUDE_PACKAGES:
            i += 1
            continue

        base, is_family = classify_package(pkg.name)

        if not is_family:
            groups.append(PackageGroup(
                [pkg.name], pkg.version, pkg.url, pkg.summary,
            ))
            i += 1
            continue

        # Scan forward for all variants sharing this base prefix.
        # This matches the Perl approach: once a base is identified,
        # consume consecutive packages matching {base}-*-ohpc.
        end = i + 1
        prefix = base + "-"
        while end < len(packages):
            next_pkg = packages[end]
            if next_pkg.name in EXCLUDE_PACKAGES:
                end += 1
                continue
            if next_pkg.name.startswith(prefix) and next_pkg.name.endswith("-ohpc"):
                end += 1
            else:
                break

        variant_names = [
            p.name for p in packages[i:end]
            if p.name not in EXCLUDE_PACKAGES
        ]
        groups.append(PackageGroup(
            variant_names, pkg.version, pkg.url, pkg.summary,
        ))
        i = end

    return groups


# ---------------------------------------------------------------------------
# Table Formatting
# ---------------------------------------------------------------------------

def format_summary(summary: str) -> str:
    """Ensure summary ends with a period."""
    summary = summary.rstrip()
    if not summary.endswith("."):
        summary += "."
    return summary


def format_url(url: str) -> str:
    """Format URL as a markdown link."""
    url = url.rstrip("/")
    if url == "(none)":
        return ""
    return f"[{url}]({url})"


def generate_package_table(groups: list[PackageGroup]) -> str:
    """Generate a markdown table from PackageGroups."""
    if not groups:
        return ""

    lines = [
        "| **RPM Package Name** | **Version** | **Info/URL** |",
        "|----------------------|-------------|--------------|",
    ]

    for group in groups:
        names_cell = "<br>".join(group.names)
        summary = format_summary(group.summary)
        url = format_url(group.url)
        info = f"{summary} {url}".rstrip()
        lines.append(f"| {names_cell} | {group.version} | {info} |")

    return "\n".join(lines)


def generate_meta_table(patterns: list[tuple[str, str]]) -> str:
    """Generate a markdown table from meta-package data."""
    if not patterns:
        return ""

    lines = [
        "| **Group Name** | **Description** |",
        "|----------------|----------------|",
    ]

    for name, description in patterns:
        desc = format_summary(description)
        lines.append(f"| {name} | {desc} |")

    return "\n".join(lines)


# ---------------------------------------------------------------------------
# Document Assembly
# ---------------------------------------------------------------------------

def generate_meta_md(meta_path: Path) -> str:
    """Generate meta-package table markdown from meta-ohpc.all."""
    meta_patterns = parse_meta_ohpc(meta_path)
    return generate_meta_table(meta_patterns) + "\n"


def generate_pkg_md(pkg_path: Path) -> str:
    """Generate per-category package tables from pkg-ohpc.all.

    Each category is wrapped in a Jinja2 conditional so the including
    template can selectively disable categories via e.g.
    ``{% set pkg_provisioning = false %}``.  Categories default to
    enabled via ``| default(true)``.

    Output is a .md.j2 file (Jinja2 template).
    """
    all_packages = parse_pkg_ohpc(pkg_path)

    # Group packages by category
    packages_by_category: dict[str, list[Package]] = {}
    for pkg in all_packages:
        packages_by_category.setdefault(pkg.category, []).append(pkg)

    # Build category tables wrapped in Jinja2 conditionals
    parts = []
    for cat_key, cat_heading in CATEGORY_ORDER:
        cat_packages = packages_by_category.get(cat_key, [])
        if not cat_packages:
            continue
        groups = group_packages(cat_packages)
        if not groups:
            continue
        table = generate_package_table(groups)
        # Variable name: pkg_admin, pkg_provisioning, pkg_io_libs, etc.
        var_name = "pkg_" + cat_key.replace("-", "_")
        parts.append(f"{{% if {var_name} | default(true) %}}")
        parts.append(f"### {cat_heading}\n")
        parts.append(table)
        parts.append("")
        parts.append("{% endif %}")
        parts.append("")

    return "\n".join(parts) + "\n"


# ---------------------------------------------------------------------------
# .all File Generation (queries system package manager)
# ---------------------------------------------------------------------------

def parse_manifest_dir(manifest_dir: Path) -> tuple[str, str]:
    """Extract distro prefix and architecture from manifest directory name.

    e.g., 'el10-x86_64' -> ('el10', 'x86_64')
    """
    name = manifest_dir.name
    for arch in ("x86_64", "aarch64"):
        if name.endswith(f"-{arch}"):
            prefix = name[: -len(f"-{arch}")]
            return prefix, arch
    print(f"Error: Cannot detect architecture from directory name: {name}", file=sys.stderr)
    print("Expected format: <distro>-<arch> (e.g., el10-x86_64)", file=sys.stderr)
    sys.exit(1)


def build_repo_args(version: str, baseos: str) -> list[str]:
    """Build dnf repoquery args for the correct OpenHPC repos.

    Returns repo arguments including --repofrompath and --repoid flags.
    For minor versions > 0, also includes the update repo with --latest-limit 1.
    """
    repo_base_url = _CONFIG["repo_base_url"]
    parts = version.split(".")
    major = parts[0]
    minor = int(parts[1]) if len(parts) > 1 else 0

    base_url = f"{repo_base_url}/{major}/{baseos}"
    args = [
        f"--repofrompath=ohpc-base,{base_url}",
        "--repoid=ohpc-base",
    ]

    if minor > 0:
        update_url = f"{repo_base_url}/{major}/update.{version}/{baseos}"
        args += [
            "--latest-limit", "1",
            f"--repofrompath=ohpc-update,{update_url}",
            "--repoid=ohpc-update",
        ]

    return args


def dnf_repoquery(query_format: str, pattern: str, version: str,
                   baseos: str, arch: str) -> str:
    """Run dnf repoquery with proper repo pinning and arch filtering.

    Raises SystemExit on failure.
    """
    arch_query = f"{arch},noarch"
    cmd = [
        "dnf", "repoquery",
        f"--arch={arch_query}",
        *build_repo_args(version, baseos),
        f"--queryformat={query_format}",
        pattern,
    ]
    try:
        result = subprocess.run(cmd, capture_output=True, text=True, check=True,
                                env={**__import__('os').environ, "LC_COLLATE": "C"})
        return result.stdout
    except subprocess.CalledProcessError as e:
        print(f"Error: dnf repoquery failed", file=sys.stderr)
        print(f"Command: {' '.join(cmd)}", file=sys.stderr)
        if e.stderr:
            print(f"Error output: {e.stderr}", file=sys.stderr)
        sys.exit(1)
    except FileNotFoundError:
        print("Error: dnf not found. Must run on a system with dnf installed.", file=sys.stderr)
        print("For macOS with Lima: lima python3 generate_manifest.py ... --generate-all", file=sys.stderr)
        sys.exit(1)


def generate_all_files(manifest_dir: Path, version: str) -> None:
    """Generate pkg-ohpc.all and meta-ohpc.all by querying dnf.

    Mirrors the logic from common/listohpc:
    - Queries specific OpenHPC version repos (not system repos)
    - Filters by architecture
    - Applies aarch64 skip patterns
    - Sorts output
    """
    distro_prefix, arch = parse_manifest_dir(manifest_dir)

    # Map distro prefix to baseos name
    baseos_map = _CONFIG.get("baseos_map", {})
    baseos = baseos_map.get(distro_prefix)
    if not baseos:
        print(f"Error: Unknown distro prefix '{distro_prefix}'", file=sys.stderr)
        print(f"Known prefixes: {', '.join(baseos_map.keys())}", file=sys.stderr)
        sys.exit(1)

    print(f"  Version: {version}")
    print(f"  Base OS: {baseos}")
    print(f"  Architecture: {arch}")
    print()

    # Build aarch64 skip pattern
    skip_patterns = _CONFIG.get("aarch64_skip_patterns", [])
    skip_re = re.compile("|".join(skip_patterns)) if arch == "aarch64" and skip_patterns else None
    if skip_re:
        print(f"  Excluding aarch64-incompatible packages: {', '.join(skip_patterns)}")

    # --- Generate pkg-ohpc.all ---
    print("Querying packages...")
    raw = dnf_repoquery(
        "%{Name} %{Version} %{URL} %{Group} %{Summary}\\n", "*",
        version, baseos, arch,
    )

    # Filter: only *-ohpc packages, apply skip patterns, sort
    lines = []
    for line in raw.splitlines():
        line = line.strip()
        if not line or not re.search(r"-ohpc\b", line):
            continue
        if skip_re and skip_re.search(line):
            continue
        lines.append(line)

    # Also include ohpc-release
    release_raw = dnf_repoquery(
        "%{Name} %{Version} %{URL} %{Group} %{Summary}\\n", "ohpc-release",
        version, baseos, arch,
    )
    for line in release_raw.splitlines():
        line = line.strip()
        if line:
            lines.append(line)

    lines.sort()

    pkg_path = manifest_dir / "pkg-ohpc.all"
    with open(pkg_path, "w", encoding="utf-8") as f:
        f.write("\n".join(lines) + "\n")
    print(f"Generated: {pkg_path} ({len(lines)} packages)")

    # --- Generate meta-ohpc.all ---
    print("Querying meta-packages...")
    raw = dnf_repoquery(
        "%{Name} %{Group} %{Description}\\n", "ohpc*",
        version, baseos, arch,
    )

    # Filter: ohpc/meta-package group, extract name + description
    meta_lines = []
    for line in raw.splitlines():
        line = line.strip()
        if not line or "ohpc/meta-package" not in line:
            continue
        parts = line.split(None, 2)  # name, group, description
        if len(parts) >= 3 and parts[0].startswith("ohpc-"):
            meta_lines.append(f"{parts[0]} {parts[2]}")

    meta_lines.sort()

    meta_path = manifest_dir / "meta-ohpc.all"
    with open(meta_path, "w", encoding="utf-8") as f:
        f.write("\n".join(meta_lines) + "\n")
    print(f"Generated: {meta_path} ({len(meta_lines)} meta-packages)")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Generate Package Manifest tables from .all files",
        epilog="""
Examples:
  %(prog)s manifests/el10-x86_64                             # Generate markdown tables
  %(prog)s manifests/el10-x86_64 --generate-all --version 4.0  # Query dnf first
  lima python3 %(prog)s manifests/el10-aarch64 --generate-all --version 4.0
        """,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "manifest_dir",
        help="Manifest directory (e.g., manifests/el10-x86_64)",
    )
    parser.add_argument(
        "--generate-all",
        action="store_true",
        help="Generate .all files by querying dnf (requires dnf on target system)",
    )
    parser.add_argument(
        "--version",
        help="OpenHPC version (e.g., 4.0) - required with --generate-all",
    )
    args = parser.parse_args()

    manifest_dir = Path(args.manifest_dir)
    if not manifest_dir.is_dir():
        print(f"Error: {manifest_dir} is not a directory", file=sys.stderr)
        sys.exit(1)

    pkg_path = manifest_dir / "pkg-ohpc.all"
    meta_path = manifest_dir / "meta-ohpc.all"

    # Generate .all files if requested
    if args.generate_all:
        if not args.version:
            print("Error: --version is required with --generate-all", file=sys.stderr)
            sys.exit(1)
        generate_all_files(manifest_dir, args.version)
        print()

    # Check that .all files exist
    if not pkg_path.exists():
        print(f"Error: {pkg_path} not found", file=sys.stderr)
        print("Hint: Use --generate-all to create it from dnf", file=sys.stderr)
        sys.exit(1)
    if not meta_path.exists():
        print(f"Error: {meta_path} not found", file=sys.stderr)
        print("Hint: Use --generate-all to create it from dnf", file=sys.stderr)
        sys.exit(1)

    # Generate meta-package table
    meta_output = manifest_dir / "meta-ohpc.md"
    with open(meta_output, "w", encoding="utf-8") as f:
        f.write(generate_meta_md(meta_path))
    print(f"Generated: {meta_output}")

    # Generate per-category package tables (Jinja2 template)
    pkg_output = manifest_dir / "pkg-ohpc.md.j2"
    with open(pkg_output, "w", encoding="utf-8") as f:
        f.write(generate_pkg_md(pkg_path))
    print(f"Generated: {pkg_output}")


if __name__ == "__main__":
    main()
