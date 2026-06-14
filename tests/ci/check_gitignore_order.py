#!/usr/bin/env python3
"""Check that entries within each section of .gitignore are sorted."""

import os
import subprocess
import sys


def check_gitignore(path):
    errors = []
    section = None
    section_start = 0
    entries = []

    with open(path) as f:
        lines = f.readlines()

    def check_section(section_name, section_entries, start_line):
        """Verify entries are in case-insensitive sorted order."""
        if len(section_entries) < 2:
            return
        for i in range(len(section_entries) - 1):
            a_line, a_text = section_entries[i]
            b_line, b_text = section_entries[i + 1]
            if a_text.lower() > b_text.lower():
                errors.append(
                    f"  line {b_line}: '{b_text}' should come before "
                    f"'{a_text}' (line {a_line})"
                )

    for lineno, raw in enumerate(lines, start=1):
        line = raw.rstrip("\n")

        # Detect section headers (comment lines starting with #)
        if line.startswith("#"):
            # Check previous section before starting a new one
            if entries:
                check_section(section, entries, section_start)
            section = line
            section_start = lineno
            entries = []
            continue

        # Skip blank lines - they also end the current group
        if not line.strip():
            if entries:
                check_section(section, entries, section_start)
            entries = []
            continue

        # Negation lines (!) are part of the section but excluded from
        # sort checks as they must appear in a specific position
        if line.startswith("!"):
            continue

        entries.append((lineno, line))

    # Check the final section
    if entries:
        check_section(section, entries, section_start)

    return errors


def check_single_gitignore(repo_root):
    """Verify that only one .gitignore is tracked in the repository."""
    result = subprocess.run(
        ["git", "ls-files", "**/.gitignore", ".gitignore"],
        capture_output=True,
        text=True,
        cwd=repo_root,
    )
    extra = []
    for line in result.stdout.strip().splitlines():
        if line and line != ".gitignore":
            extra.append(line)
    return extra


def main():
    # Find repo root (two levels up from tests/ci/)
    script_dir = os.path.dirname(os.path.abspath(__file__))
    repo_root = os.path.abspath(os.path.join(script_dir, "..", ".."))
    gitignore_path = os.path.join(repo_root, ".gitignore")

    failed = False

    if not os.path.isfile(gitignore_path):
        print(f"ERROR: {gitignore_path} not found")
        sys.exit(1)

    # Check that no extra .gitignore files exist
    extra = check_single_gitignore(repo_root)
    if extra:
        print("ERROR: only one .gitignore should exist at the repo root.")
        print("  Found extra .gitignore files:")
        for path in sorted(extra):
            print(f"    {path}")
        failed = True
    else:
        print(".gitignore: no extra .gitignore files found")

    # Check sort order
    errors = check_gitignore(gitignore_path)
    if errors:
        print("ERROR: .gitignore entries are not in alphabetical order:")
        for e in errors:
            print(e)
        failed = True
    else:
        print(".gitignore: entries are in alphabetical order")

    if failed:
        sys.exit(1)


if __name__ == "__main__":
    main()
