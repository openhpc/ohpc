#!/usr/bin/env python3
"""
Simple Jinja2 template renderer - replacement for jinja2-cli.

Usage: jinja2-render.py [--strict] [--format=yaml] TEMPLATE [DATA_FILE] [-o OUTPUT]

This script provides a minimal replacement for the jinja2-cli tool,
supporting the subset of features used by OpenHPC documentation builds.
"""

import argparse
import os
import sys
from pathlib import Path

import yaml
from jinja2 import Environment, FileSystemLoader, StrictUndefined, Undefined


def main():
    parser = argparse.ArgumentParser(
        description="Render Jinja2 templates with YAML data",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "template",
        help="Jinja2 template file to render",
    )
    parser.add_argument(
        "data_file",
        nargs="?",
        help="YAML data file with template variables",
    )
    parser.add_argument(
        "-o", "--output",
        dest="output",
        help="Output file (default: stdout)",
    )
    parser.add_argument(
        "--strict",
        action="store_true",
        help="Fail on undefined variables",
    )
    parser.add_argument(
        "--format",
        dest="data_format",
        default="yaml",
        choices=["yaml"],
        help="Data file format (default: yaml)",
    )

    args = parser.parse_args()

    # Resolve template path
    template_path = Path(args.template).resolve()
    if not template_path.exists():
        print(f"Error: Template file not found: {args.template}", file=sys.stderr)
        sys.exit(1)

    # Set up Jinja2 environment with template directory as search path
    template_dir = template_path.parent
    env = Environment(
        loader=FileSystemLoader(str(template_dir)),
        undefined=StrictUndefined if args.strict else Undefined,
        # Keep trailing newlines (like jinja2-cli default)
        keep_trailing_newline=True,
    )

    # Load template
    template = env.get_template(template_path.name)

    # Load data from YAML file
    data = {}
    if args.data_file:
        data_path = Path(args.data_file)
        if not data_path.exists():
            print(f"Error: Data file not found: {args.data_file}", file=sys.stderr)
            sys.exit(1)

        with open(data_path, "r", encoding="utf-8") as f:
            data = yaml.safe_load(f) or {}

    # Render template
    try:
        output = template.render(**data)
    except Exception as e:
        print(f"Error rendering template: {e}", file=sys.stderr)
        sys.exit(1)

    # Write output
    if args.output:
        output_path = Path(args.output)
        output_path.parent.mkdir(parents=True, exist_ok=True)
        with open(output_path, "w", encoding="utf-8") as f:
            f.write(output)
    else:
        sys.stdout.write(output)


if __name__ == "__main__":
    main()
