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
    mkdoc.py --manifest                               # Write explorer/manifest.json
"""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
import time
import traceback
from pathlib import Path

import yaml
from jinja2 import (
    Environment,
    FileSystemLoader,
    StrictUndefined,
    TemplateRuntimeError,
    TemplateSyntaxError,
    UndefinedError,
)
from jinja2.exceptions import TemplateError
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
    """Wrap every ``{% include X %}`` with ``<!-- +section: X -->`` /
    ``<!-- -section: X -->`` markers that label the sections of the generated
    recipe.

    ``X`` is re-emitted as the include's own expression inside ``{{ }}``, so Jinja
    resolves it at render time. This marks dynamic includes such as
    ``{% include "scheduler/" ~ scheduler ~ "/startup.md.j2" %}`` with their
    concrete path, not only literal-string includes — and a literal
    ``{% include "foo.md.j2" %}`` yields ``{{ "foo.md.j2" }}``, which renders
    exactly as before. Reusing Jinja's own evaluation avoids re-implementing
    path resolution here."""

    _INCLUDE = re.compile(r"{%-?\s*include\s+(?P<expr>.+?)\s*-?%}")

    def preprocess(self, source, name, filename=None):
        def wrap(match):
            path = "{{ " + match.group("expr") + " }}"
            return f"<!-- +section: {path} -->\n{match.group(0)}\n<!-- -section: {path} -->\n"

        return self._INCLUDE.sub(wrap, source)


def create_jinja_env(
    template_dirs: list[Path], config: dict | None = None
) -> Environment:
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
                if frame.filename.endswith(".j2") or "template" in frame.filename:
                    print(f"Error in {frame.filename}", file=sys.stderr)
                    print(f"  Line {frame.lineno}: {e!s}", file=sys.stderr)
                    break
        raise


def render_document(env: Environment, config: dict) -> str:
    """Render the document from the main chapter template."""
    try:
        return render_template(env, "chapters/main.md.j2", config)
    except TemplateError as e:
        print(f"Error rendering chapters/main.md.j2:\n  {e}", file=sys.stderr)
        sys.exit(1)


def check_line_lengths(content: str, max_length: int = 87) -> int:
    """Warn about lines in ```bash and ```console code blocks that exceed max_length."""
    current_section = "<unknown>"
    section_start_lineno = 1
    in_code_block = False
    warnings = 0

    for lineno, line in enumerate(content.split("\n"), 1):
        stripped = line.rstrip()

        # Track current section from comment markers
        match = re.match(r"<!-- [+-]section: (.+?) -->", stripped)
        if match:
            current_section = match.group(1)
            section_start_lineno = lineno
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
                f"{current_section}:{lineno - section_start_lineno}",
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
      <!-- ohpc_fi -->              fi
    Blank lines inside ohpc_begin/ohpc_end blocks pass through.
    """
    lines = []
    in_run_block = False
    in_code_block = False
    current_section = None

    for line in content.split("\n"):
        # Pass through section markers
        match = re.search(r"<!-- ([+-])section: (.+) -->", line)
        if match:
            where, section = match.groups()
            lines.append(f"# --- {where}section: {section} ---")
            if where == "+":
                current_section = section
            continue

        # Track run blocks
        if "<!-- ohpc_begin -->" in line:
            in_run_block = True
            if current_section:
                lines.append(f"echo '=== section: {current_section} ==='")
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
                print(
                    f"Error: ohpc directive missing closing -->: {line}",
                    file=sys.stderr,
                )
                sys.exit(1)
            # ohpc_if_set: shorthand for enable_* boolean flags (check before ohpc_if)
            match = re.search(r"<!-- ohpc_if_set (\S+) -->", line)
            if match:
                lines.append(f'if [[ "${{{match.group(1)}}}" -eq 1 ]];then')
                continue
            # ohpc_if: general condition
            match = re.search(r"<!-- ohpc_if (.+) -->", line)
            if match:
                lines.append(f"if {match.group(1)};then")
                continue
            if "<!-- ohpc_else -->" in line:
                lines.append("else")
                continue
            if "<!-- ohpc_fi -->" in line:
                lines.append("fi")
                continue
            # ohpc_command: raw shell line
            match = re.search(r"<!-- ohpc_command (.+) -->", line)
            if match:
                lines.append(match.group(1).rstrip())
                continue
            # ohpc_comment: shell comment line
            match = re.search(r"<!-- ohpc_comment (.+) -->", line)
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
# Explorer manifest (explorer/manifest.json)
# ---------------------------------------------------------------------------
#
# The browser-side recipe explorer (explorer/) renders the guides from the
# templates in the browser. A static web server has no directory listing, so
# this is the one index the page needs: the recipes with the config files each
# .conf merges (what the Makefile's build/%.yaml rule does), the files the
# template loader may fetch, the runtime enable_*/has_* flags and the pick-one
# settings declared in input.local.template, and the vc_revision/vc_date the
# Makefile injects.

MANIFEST_PATH = BASE_DIR / "explorer" / "manifest.json"
# Directories the loader searches (templates, manifests), plus config/ so the
# page can pick axis config files without probing.
MANIFEST_ROOTS = ("templates", "manifests", "config")
MANIFEST_SUFFIXES = (".j2", ".md", ".svg", ".png", ".yaml")
FLAG_RE = re.compile(
    r'^(?P<name>(enable_|has_|update_)\w+)="?\$\{(?P=name):-(?P<default>[^}]*)\}"?'
)
# A pick-one setting: NAME_choices='["a","b"]' (a JSON list) on the line before
# NAME="${NAME:-a}". The explorer offers the list as a selector.
CHOICES_RE = re.compile(r"^(?P<name>\w+)_choices='(?P<choices>[^']*)'$")
SETTING_RE = re.compile(r'^(?P<name>\w+)="?\$\{(?P=name):-(?P<default>[^}]*)\}"?')


def git_output(*args: str) -> str | None:
    """stdout of a git command run in docs/install, or None if unavailable."""
    try:
        return subprocess.run(
            ["git", *args], cwd=BASE_DIR, check=True, capture_output=True, text=True
        ).stdout.strip()
    except (subprocess.CalledProcessError, FileNotFoundError):
        return None


def manifest_recipes(base: Path = BASE_DIR) -> list[dict]:
    entries = []
    for conf in sorted((base / "recipes").glob("*.conf")):
        files = [
            line.strip()
            for line in conf.read_text(encoding="utf-8").splitlines()
            if line.strip() and not line.strip().startswith("#")
        ]
        override = conf.with_suffix(".yaml")
        entries.append(
            {
                "name": conf.stem,
                "conf": f"recipes/{conf.name}",
                "configs": files,
                "override": f"recipes/{override.name}" if override.exists() else None,
            }
        )
    return entries


def manifest_files(base: Path = BASE_DIR) -> list[str]:
    files = []
    for root in MANIFEST_ROOTS:
        for path in sorted((base / root).rglob("*")):
            if path.is_file() and path.suffix in MANIFEST_SUFFIXES:
                files.append(path.relative_to(base).as_posix())
    return files


def manifest_flags(base: Path = BASE_DIR) -> list[dict]:
    """enable_*/has_* flags and defaults from input.local.template; the
    preceding comment line is kept as the flag's description."""
    flags = []
    comment = None
    template = base / "input.local.template"
    for line in template.read_text(encoding="utf-8").splitlines():
        stripped = line.strip()
        if stripped.startswith("#"):
            text = stripped.lstrip("#").strip()
            comment = text if text and not set(text) <= {"-"} else None
            continue
        match = FLAG_RE.match(stripped)
        if match:
            default = match.group("default")
            flags.append(
                {
                    "name": match.group("name"),
                    "default": int(default) if default.isdigit() else default,
                    "description": comment,
                }
            )
        comment = None
    return flags


def manifest_choices(base: Path = BASE_DIR) -> list[dict]:
    """Pick-one settings from input.local.template: a NAME_choices JSON list
    directly before NAME="${NAME:-default}". The comment block above the pair is
    the setting's description. A list that is not JSON, not a list of strings,
    not followed by its setting, or without the default fails the build."""
    choices = []
    comment = None
    pending = None
    template = base / "input.local.template"
    for number, line in enumerate(
        template.read_text(encoding="utf-8").splitlines(), start=1
    ):
        stripped = line.strip()
        where = f"input.local.template:{number}"
        if pending:
            match = SETTING_RE.match(stripped)
            if not match or match.group("name") != pending["name"]:
                raise ValueError(
                    f"{where}: {pending['name']}_choices must be followed by "
                    f'{pending["name"]}="${{{pending["name"]}:-...}}"'
                )
            if match.group("default") not in pending["choices"]:
                raise ValueError(
                    f"{where}: default {match.group('default')!r} of "
                    f"{pending['name']} is not one of {pending['choices']}"
                )
            choices.append({**pending, "default": match.group("default")})
            pending = None
            comment = None
            continue
        if stripped.startswith("#"):
            text = stripped.lstrip("#").strip()
            if text and not set(text) <= {"-"}:
                comment = f"{comment} {text}" if comment else text
            else:
                comment = None
            continue
        match = CHOICES_RE.match(stripped)
        if match:
            try:
                values = json.loads(match.group("choices"))
            except json.JSONDecodeError as e:
                raise ValueError(f"{where}: {match.group('name')}_choices: {e}")
            if (
                not isinstance(values, list)
                or not values
                or not all(isinstance(v, str) and v for v in values)
            ):
                raise ValueError(
                    f"{where}: {match.group('name')}_choices must be a JSON list "
                    "of non-empty strings"
                )
            pending = {
                "name": match.group("name"),
                "choices": values,
                "description": comment,
            }
            continue
        comment = None
    if pending:
        raise ValueError(
            f"input.local.template: {pending['name']}_choices is not followed by "
            f"its setting"
        )
    return choices


def write_manifest(
    output: Path,
    vc_revision: str | None = None,
    vc_date: str | None = None,
    vc_branch: str | None = None,
    base: Path = BASE_DIR,
) -> None:
    """Write the explorer manifest for the docs/install tree at `base`. The
    vc_* overrides serve trees that are not a git checkout (explorer/mksite.py
    exports refs with `git archive`)."""
    manifest = {
        "generated": time.strftime("%Y-%m-%d", time.gmtime()),
        "vc_revision": vc_revision
        or git_output("log", "-1", "HEAD", "--pretty=format:%H")
        or "non-git-checkout",
        "vc_date": vc_date
        or git_output("log", "-1", "HEAD", "--pretty=format:%as")
        or time.strftime("%Y-%m-%d", time.gmtime()),
        "vc_branch": vc_branch or git_output("rev-parse", "--abbrev-ref", "HEAD"),
        "recipes": manifest_recipes(base),
        "files": manifest_files(base),
        "flags": manifest_flags(base),
        "choices": manifest_choices(base),
    }
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
    print(
        f"Wrote {output}: {len(manifest['recipes'])} recipes, "
        f"{len(manifest['files'])} files, {len(manifest['flags'])} flags, "
        f"{len(manifest['choices'])} choices"
    )


# ---------------------------------------------------------------------------
# Main Build Function
# ---------------------------------------------------------------------------


def resolve_output(
    flag, suffix: str, config_path: Path, build_dir: Path
) -> Path | None:
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
        print(
            f"Error: --{with_name.replace('_', '-')} and --{path_name} are mutually exclusive",
            file=sys.stderr,
        )
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
        nargs="?",
        help="Pre-merged config YAML file (build/<name>.yaml)",
    )
    parser.add_argument(
        "--manifest",
        nargs="?",
        const=str(MANIFEST_PATH),
        metavar="PATH",
        help=f"Write the recipe explorer's manifest (default: {MANIFEST_PATH.relative_to(BASE_DIR)}) and exit",
    )
    parser.add_argument(
        "--vc-revision",
        help="with --manifest: commit hash to stamp (default: git HEAD)",
    )
    parser.add_argument(
        "--vc-date", help="with --manifest: commit date YYYY-MM-DD (default: git HEAD)"
    )
    parser.add_argument(
        "--vc-branch", help="with --manifest: branch or tag name (default: git HEAD)"
    )
    parser.add_argument(
        "--base",
        metavar="DIR",
        help="with --manifest: the docs/install tree to index (default: this one)",
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

    if args.manifest:
        try:
            write_manifest(
                Path(args.manifest),
                args.vc_revision,
                args.vc_date,
                args.vc_branch,
                Path(args.base).resolve() if args.base else BASE_DIR,
            )
        except ValueError as e:
            print(f"Error: {e}", file=sys.stderr)
            sys.exit(1)
        return
    if not args.config:
        parser.error("a config file is required unless --manifest is given")

    config_path = Path(args.config).resolve()
    markdown_output = Path(args.markdown) if args.markdown else None

    print(f"=== mkdoc.py {args.config} ===")
    build(
        config_path=config_path,
        markdown_output=markdown_output,
        script_output=parse_output_flag(args, "with_script", "script"),
        list_vars=args.list_vars,
        ignore_warnings=args.ignore_warnings,
    )


if __name__ == "__main__":
    main()
