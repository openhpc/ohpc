#!/usr/bin/env python3
"""
Generate a ChangeLog by comparing pkg-ohpc.all between a previous git
release tag and the current working tree.

Reads the current pkg-ohpc.all from a manifest directory and the old
version via ``git show``, then produces a plain-text ChangeLog with
sections for additions, version changes, and deprecations.

Usage:
    python generate_changelog.py manifests/el10-x86_64 --old-ref v4.0.GA
    python generate_changelog.py manifests/el10-aarch64 --old-ref v4.0.GA
    python generate_changelog.py manifests/oe2403-x86_64 --old-ref v4.0.GA
    python generate_changelog.py manifests/oe2403-aarch64 --old-ref v4.0.GA

Output (in the specified manifest directory):
    ChangeLog - Plain-text changelog with three sections
"""

import argparse
import subprocess
import sys
import yaml
from pathlib import Path


def load_config() -> dict:
    """Load configuration from manifests/config.yaml."""
    config_path = Path(__file__).parent / "manifests" / "config.yaml"
    with open(config_path, "r", encoding="utf-8") as f:
        return yaml.safe_load(f)


def parse_pkg_names_versions(text: str) -> dict[str, str]:
    """Parse pkg-ohpc.all text into a {name: version} dict.

    Each line has at least two whitespace-separated fields:
        name version [url category summary ...]
    Only the first two fields (name, version) are used.
    """
    packages: dict[str, str] = {}
    for line in text.splitlines():
        line = line.strip()
        if not line:
            continue
        parts = line.split(None, 2)
        if len(parts) >= 2:
            packages[parts[0]] = parts[1]
    return packages


def read_old_manifest(old_ref: str, old_path: str) -> str:
    """Read pkg-ohpc.all from a previous git ref via ``git show``."""
    git_path = f"{old_ref}:{old_path}/pkg-ohpc.all"
    try:
        result = subprocess.run(
            ["git", "show", git_path],
            capture_output=True,
            text=True,
            check=True,
        )
        return result.stdout
    except subprocess.CalledProcessError as e:
        print(f"Error: git show {git_path} failed", file=sys.stderr)
        if e.stderr:
            print(e.stderr.strip(), file=sys.stderr)
        sys.exit(1)


def generate_changelog(
    old_packages: dict[str, str],
    new_packages: dict[str, str],
) -> str:
    """Compare old and new package dicts and return formatted ChangeLog text."""
    old_names = set(old_packages.keys())
    new_names = set(new_packages.keys())

    additions = sorted(new_names - old_names)
    deletions = sorted(old_names - new_names)
    common = sorted(old_names & new_names)

    # Version changes: packages present in both with different versions
    updates = [
        (name, old_packages[name], new_packages[name])
        for name in common
        if old_packages[name] != new_packages[name]
    ]

    lines: list[str] = []

    lines.append("   [Component Additions]")
    for name in additions:
        version = new_packages[name]
        lines.append(f"      * {name:<40} (v{version})")

    lines.append("")
    lines.append("   [Component Version Changes]")
    for name, old_ver, new_ver in updates:
        lines.append(f"      * {name:<40} (v{old_ver} -> v{new_ver})")

    lines.append("")
    lines.append("   [Components Deprecated]")
    for name in deletions:
        version = old_packages[name]
        lines.append(f"      * {name:<40} (v{version})")

    lines.append("")
    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser(
        description="Generate ChangeLog from pkg-ohpc.all comparison",
        epilog="""
Examples:
  %(prog)s manifests/el10-x86_64 --old-ref v4.0.GA
  %(prog)s manifests/oe2403-aarch64 --old-ref v4.0.GA
        """,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "manifest_dir",
        help="Manifest directory (e.g., manifests/el10-x86_64)",
    )
    parser.add_argument(
        "--old-ref",
        required=True,
        help="Git ref for the previous release (e.g., v4.0.GA)",
    )
    args = parser.parse_args()

    manifest_dir = Path(args.manifest_dir)
    if not manifest_dir.is_dir():
        print(f"Error: {manifest_dir} is not a directory", file=sys.stderr)
        sys.exit(1)

    config = load_config()
    old_manifest_paths = config.get("old_manifest_paths", {})

    dir_name = manifest_dir.name
    old_path = old_manifest_paths.get(dir_name)
    if not old_path:
        print(
            f"Error: no old_manifest_paths entry for '{dir_name}' "
            f"in manifests/config.yaml",
            file=sys.stderr,
        )
        print(
            f"Known entries: {', '.join(old_manifest_paths.keys())}",
            file=sys.stderr,
        )
        sys.exit(1)

    # Read current pkg-ohpc.all
    pkg_path = manifest_dir / "pkg-ohpc.all"
    if not pkg_path.exists():
        print(f"Error: {pkg_path} not found", file=sys.stderr)
        sys.exit(1)

    with open(pkg_path, "r", encoding="utf-8") as f:
        new_text = f.read()
    new_packages = parse_pkg_names_versions(new_text)

    # Read old pkg-ohpc.all from git
    old_text = read_old_manifest(args.old_ref, old_path)
    old_packages = parse_pkg_names_versions(old_text)

    # Generate and write ChangeLog
    changelog = generate_changelog(old_packages, new_packages)
    output_path = manifest_dir / "ChangeLog"
    with open(output_path, "w", encoding="utf-8") as f:
        f.write(changelog)

    print(f"Generated: {output_path}")
    print(
        f"  Additions: {len(set(new_packages) - set(old_packages))}, "
        f"Updates: {sum(1 for n in set(new_packages) & set(old_packages) if old_packages[n] != new_packages[n])}, "
        f"Deprecated: {len(set(old_packages) - set(new_packages))}"
    )


if __name__ == "__main__":
    main()
