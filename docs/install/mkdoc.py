#!/usr/bin/env python3
"""
OpenHPC Documentation Build Tool

Builds documentation from Jinja2 templates with a pre-merged YAML config.
Config merging is handled by the Makefile using yq.

Usage:
    mkdoc.py <build/name.yaml>                        # Generate .md in build/
    mkdoc.py <build/name.yaml> --markdown output.md   # Generate .md at custom path
    mkdoc.py <build/name.yaml> --with-script          # Also generate .sh in build/
    mkdoc.py <build/name.yaml> --script output.sh     # Generate .sh at custom path
    mkdoc.py <build/name.yaml> --list-vars            # Show all variables
    mkdoc.py <build/name.yaml> --ignore-warnings      # Don't fail on warnings
"""

import argparse
import re
import sys
import traceback
from pathlib import Path

import yaml
from jinja2 import Environment, FileSystemLoader, StrictUndefined, TemplateSyntaxError, TemplateRuntimeError, UndefinedError
from jinja2.ext import Extension

BASE_DIR = Path(__file__).resolve().parent


# ---------------------------------------------------------------------------
# Configuration Loading
# ---------------------------------------------------------------------------

def load_yaml(path: Path) -> dict:
    """Load a YAML file."""
    if not path.exists():
        print(f"Error: Config file not found: {path}", file=sys.stderr)
        sys.exit(1)
    with open(path, "r", encoding="utf-8") as f:
        return yaml.safe_load(f) or {}


# ---------------------------------------------------------------------------
# Template Rendering
# ---------------------------------------------------------------------------

class SectionCommentExtension(Extension):
    """Inject <!-- +section: path --> ... <!-- -section --> comments around every {% include %}."""

    def preprocess(self, source, name, filename=None):
        def add_comment(match):
            path = match.group(1)
            return f"<!-- +section: {path} -->\n{match.group(0)}\n<!-- -section: {path} -->\n"

        return re.sub(
            r'{%-?\s*include\s+["\']([^"\']+)["\']\s*-?%}',
            add_comment,
            source,
        )


def create_jinja_env(template_dirs: list[Path], config: dict = None) -> Environment:
    """Create Jinja2 environment with template search paths."""
    env = Environment(
        loader=FileSystemLoader([str(d) for d in template_dirs]),
        undefined=StrictUndefined,
        keep_trailing_newline=True,
        extensions=[SectionCommentExtension],
    )

    # Add config variables to globals (available in macros.j2)
    if config:
        env.globals.update(config)

    # Load global macros from templates/macros.j2 (required)
    try:
        macros_template = env.get_template("macros.j2")
        env.globals.update(macros_template.module.__dict__)
    except Exception as e:
        print(f"Error loading macros from macros.j2: {e}", file=sys.stderr)
        raise

    return env


def render_template(
    env: Environment,
    template_name: str,
    config: dict,
) -> str:
    """Render a Jinja2 template with the given config."""
    try:
        template = env.get_template(template_name)
        return template.render(**config)
    except (TemplateSyntaxError, TemplateRuntimeError, UndefinedError) as e:
        # Extract file/line from traceback
        tb = traceback.extract_tb(e.__traceback__)
        if tb:
            # Find the first frame in a template file (not in mkdoc.py)
            for frame in reversed(tb):
                if frame.filename.endswith('.j2') or 'template' in frame.filename:
                    print(f"Error in {frame.filename}", file=sys.stderr)
                    print(f"  Line {frame.lineno}: {str(e)}", file=sys.stderr)
                    break
        raise


def render_document(env: Environment, config: dict) -> str:
    """Render the document from the main chapter template."""
    try:
        return render_template(env, "chapters/main.md.j2", config)
    except Exception as e:
        print(f"Error rendering chapters/main.md.j2:\n  {e}", file=sys.stderr)
        sys.exit(1)


def check_line_lengths(content: str, max_length: int = 87) -> int:
    """Warn about lines in ```bash and ```console code blocks that exceed max_length."""
    current_section = "<unknown>"
    in_code_block = False
    warnings = 0

    for lineno, line in enumerate(content.split("\n"), 1):
        stripped = line.rstrip()

        # Track current section from comment markers
        match = re.match(r'<!-- [+-]section: (.+?) -->', stripped)
        if match:
            current_section = match.group(1)
            continue

        # Track code blocks
        if stripped.startswith("```"):
            if not in_code_block and stripped.lstrip("`") in ("bash", "console"):
                in_code_block = True
            else:
                in_code_block = False
            continue

        if in_code_block and len(stripped) > max_length:
            print(
                f"WARNING: line too long ({len(stripped)} > {max_length}): "
                f"{current_section}:{lineno}",
                file=sys.stderr,
            )
            print(f"  {stripped}", file=sys.stderr)
            warnings += 1

    return warnings


def render_pandoc_headers(config_path: Path, build_dir: Path, config: dict) -> None:
    """
    Render per-recipe LaTeX headers needed for PDF generation.

    Writes two files to build/:
      {name}-header.tex  — rendered from pandoc/header-includes.tex.j2 with
                           recipe config (distro, arch, provisioner, scheduler
                           appear in page headers)
      graphicspath.tex   — absolute \\graphicspath for xelatex image lookup

    Called unconditionally during the .md build so these files are available
    when the Makefile invokes pandoc for PDF output.
    """
    pandoc_dir = BASE_DIR / "pandoc"
    templates_dir = BASE_DIR / "templates"
    figures_dir = templates_dir / "figures"

    # Per-recipe header (page headers contain distro/arch/provisioner/scheduler)
    header_tex = build_dir / (config_path.stem + "-header.tex")
    header_env = Environment(
        loader=FileSystemLoader(pandoc_dir),
        undefined=StrictUndefined,
    )
    header_tmpl = header_env.get_template("header-includes.tex.j2")
    with open(header_tex, "w", encoding="utf-8") as f:
        f.write(header_tmpl.render(config))

    # Shared graphicspath (same content for all recipes)
    graphicspath_tex = build_dir / "graphicspath.tex"
    with open(graphicspath_tex, "w", encoding="utf-8") as f:
        f.write(f"\\graphicspath{{{{{figures_dir}/}}{{{templates_dir}/}}}}\n")


def extract_recipe_script(content: str) -> str:
    """
    Extract shell commands from markdown to create recipe.sh.

    Extracts code blocks within <!-- ohpc_begin --> / <!-- ohpc_end -->
    sections, along with ohpc_comment/command/if/endif directives.

    Markers:
      <!-- ohpc_begin -->           start extraction block
      <!-- ohpc_end -->             end extraction block
      <!-- ohpc_command CMD -->     raw shell line
      <!-- ohpc_comment TEXT -->    shell comment (# TEXT)
      <!-- ohpc_if_set VAR -->      if [[ ${VAR} -eq 1 ]];then
      <!-- ohpc_if CONDITION -->    if CONDITION;then
      <!-- ohpc_else -->            else
      <!-- ohpc_fi -->           fi
    Blank lines inside ohpc_begin/ohpc_end blocks pass through.
    """
    lines = []
    in_run_block = False
    in_code_block = False

    for line in content.split("\n"):
        # Pass through section markers
        match = re.search(r'<!-- ([+-])section: (.+) -->', line)
        if match:
            where, section = match.groups()
            lines.append(f"# --- {where}section: {section} ---")
            continue

        # Track run blocks
        if "<!-- ohpc_begin -->" in line:
            in_run_block = True
            continue
        if "<!-- ohpc_end -->" in line:
            in_run_block = False
            continue

        if not in_run_block:
            continue

        # Track code blocks
        if line.startswith("```"):
            in_code_block = not in_code_block
            continue

        if in_code_block:
            lines.append(line)
        elif line.strip() == "":
            lines.append("")
        else:
            # Warn if an ohpc_ directive is missing its closing -->
            if line.startswith("<!-- ohpc_") and line[-3:] != "-->":
                print(f"Error: ohpc directive missing closing -->: {line}", file=sys.stderr)
                sys.exit(1)
            # ohpc_if_set: shorthand for enable_* boolean flags (check before ohpc_if)
            match = re.search(r'<!-- ohpc_if_set (\S+) -->', line)
            if match:
                lines.append(f'if [[ "${{{match.group(1)}}}" -eq 1 ]];then')
                continue
            # ohpc_if: general condition
            match = re.search(r'<!-- ohpc_if (.+) -->', line)
            if match:
                lines.append(f'if {match.group(1)};then')
                continue
            if "<!-- ohpc_else -->" in line:
                lines.append("else")
                continue
            if "<!-- ohpc_fi -->" in line:
                lines.append("fi")
                continue
            # ohpc_command: raw shell line
            match = re.search(r'<!-- ohpc_command (.+) -->', line)
            if match:
                lines.append(match.group(1).rstrip())
                continue
            # ohpc_comment: shell comment line
            match = re.search(r'<!-- ohpc_comment (.+) -->', line)
            if match:
                comment = match.group(1).replace("'", "'\\''")
                lines.append(f"echo '--- {comment}'")

    # Add shebang and header
    script_lines = [
        "#!/bin/bash",
        "# OpenHPC Installation Recipe",
        "# Auto-generated from markdown documentation",
        "",
        "set -e",
        "",
    ]
    script_lines.extend(lines)

    return "\n".join(script_lines)


# ---------------------------------------------------------------------------
# Main Build Function
# ---------------------------------------------------------------------------

def resolve_output(flag, suffix: str, config_path: Path, build_dir: Path) -> Path | None:
    """Resolve an output flag: True -> default path, Path -> as-is, None -> skip."""
    if flag is True:
        build_dir.mkdir(exist_ok=True)
        return build_dir / config_path.with_suffix(suffix).name
    return flag


def build(
    config_path: Path,
    markdown_output: Path | None = None,
    script_output=None,
    list_vars: bool = False,
    ignore_warnings: bool = False,
) -> None:
    """
    Build documentation from a pre-merged config YAML file.

    Args:
        config_path: Path to pre-merged config YAML (build/<name>.yaml)
        markdown_output: Path for .md output (default: build/<name>.md)
        script_output: True for build/<name>.sh, Path for custom, None to skip
        list_vars: Just list variables and exit
        ignore_warnings: Don't fail on warnings
    """

    templates_dir = BASE_DIR / "templates"
    manifests_dir = BASE_DIR / "manifests"
    build_dir = BASE_DIR / "build"

    # Load pre-merged config (produced by yq in Makefile, includes vc_revision/vc_date)
    config = load_yaml(config_path)

    if list_vars:
        print("# Configuration variables")
        for key in sorted(config.keys()):
            if not key.startswith("_"):
                print(f"{key}: {config[key]!r}")
        return

    # Create Jinja2 environment
    template_dirs = [templates_dir, manifests_dir, BASE_DIR]
    env = create_jinja_env(template_dirs, config)

    # Render document
    output = render_document(env, config)

    # When --with-script/--script is the only output requested (no explicit
    # --markdown), skip writing the .md and header .tex files. The Makefile
    # always builds .md first, so the files already exist; re-writing them
    # is redundant and triggers unnecessary downstream rebuilds.
    script_only = markdown_output is None and script_output is not None

    # Resolve output paths (True -> default in build/, Path -> as-is, None -> skip)
    if markdown_output is None:
        build_dir.mkdir(exist_ok=True)
        markdown_output = build_dir / config_path.with_suffix(".md").name
    script_output = resolve_output(script_output, ".sh", config_path, build_dir)

    # Check for long lines in bash code blocks
    warning_count = check_line_lengths(output)
    if warning_count and not ignore_warnings:
        print(
            f"Error: {warning_count} warning(s) found. "
            f"Use --ignore-warnings to override.",
            file=sys.stderr,
        )
        sys.exit(1)

    if not script_only:
        # Generate markdown (primary output)
        with open(markdown_output, "w", encoding="utf-8") as f:
            f.write(output)
        print(f"Built: {markdown_output}")

        # Render per-recipe LaTeX headers (consumed by Makefile pandoc PDF rule)
        render_pandoc_headers(config_path, build_dir, config)

    # Generate recipe script if requested
    if script_output is not None:
        recipe_script = extract_recipe_script(output)
        with open(script_output, "w", encoding="utf-8") as f:
            f.write(recipe_script)
        script_output.chmod(0o755)
        print(f"Generated: {script_output}")


def parse_output_flag(args, with_name: str, path_name: str) -> Path | bool | None:
    """Parse a --with-X / --X PATH flag pair into True, Path, or None."""
    with_flag = getattr(args, with_name)
    path_flag = getattr(args, path_name)
    if with_flag and path_flag:
        print(f"Error: --{with_name.replace('_', '-')} and --{path_name} are mutually exclusive",
              file=sys.stderr)
        sys.exit(1)
    if with_flag:
        return True  # sentinel: use default path in build/
    if path_flag:
        return Path(path_flag)
    return None


def main():
    parser = argparse.ArgumentParser(
        description="Build OpenHPC documentation from templates",
        epilog="""
Examples:
  %(prog)s build/name.yaml                          # Generate .md in build/
  %(prog)s build/name.yaml --markdown output.md     # Generate .md at custom path
  %(prog)s build/name.yaml --with-script            # Also generate .sh in build/
  %(prog)s build/name.yaml --script output.sh       # Generate .sh at custom path
        """,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "config",
        help="Pre-merged config YAML file (build/<name>.yaml)",
    )
    parser.add_argument(
        "--markdown",
        metavar="PATH",
        help="Generate markdown at PATH (default: build/<name>.md)",
    )
    parser.add_argument(
        "--with-script",
        action="store_true",
        help="Also generate recipe script in build/ directory",
    )
    parser.add_argument(
        "--script",
        metavar="PATH",
        help="Generate recipe script at specified PATH",
    )
    parser.add_argument(
        "--list-vars",
        action="store_true",
        help="List all variables after merging config",
    )
    parser.add_argument(
        "--ignore-warnings",
        action="store_true",
        help="Do not fail on warnings (e.g., long lines in code blocks)",
    )

    args = parser.parse_args()

    config_path = Path(args.config).resolve()
    markdown_output = Path(args.markdown) if args.markdown else None

    print(f'=== mkdoc.py {args.config} ===')
    build(
        config_path=config_path,
        markdown_output=markdown_output,
        script_output=parse_output_flag(args, "with_script", "script"),
        list_vars=args.list_vars,
        ignore_warnings=args.ignore_warnings,
    )


if __name__ == "__main__":
    main()
