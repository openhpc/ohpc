#!/usr/bin/env python3
"""Hard-wrap prose in a Jinja2 markdown template to 80 columns.

Skips: fenced code blocks, HTML comments, Jinja2 block/comment tags,
table rows, headings, LaTeX commands, pandoc div markers.
Wraps: prose paragraphs and list items (with correct continuation indent).

Usage:
    python wraptext.py <file.md.j2>          # wrap in-place
    python wraptext.py <file.md.j2> --check  # exit 1 if changes needed
"""

import re
import sys
import textwrap
from pathlib import Path

WIDTH = 80
LIST_RE = re.compile(r"^(\s*)([-*+]|\d+\.)\s+")


def is_verbatim(line: str) -> bool:
    """Return True for lines that must be output as-is (not accumulated)."""
    s = line.strip()
    return (
        not s  # empty (handled separately)
        or s.startswith("```")
        or s.startswith("<!--")
        or s.startswith("{%")
        or s.startswith("{#")
        or s.startswith("|")
        or s.startswith("#")
        or s.startswith("\\")
        or s.startswith(":::")
    )


def wrap_block(lines: list[str], prefix: str | None) -> str:
    """Join accumulated lines and re-wrap to WIDTH."""
    text = " ".join(line.strip() for line in lines if line.strip())
    if prefix is None:
        return textwrap.fill(
            text,
            width=WIDTH,
            break_long_words=False,
            break_on_hyphens=False,
        )
    subsequent = " " * len(prefix)
    return textwrap.fill(
        text,
        width=WIDTH,
        initial_indent=prefix,
        subsequent_indent=subsequent,
        break_long_words=False,
        break_on_hyphens=False,
    )


def process(content: str) -> str:
    result: list[str] = []
    in_code = False
    in_comment = False

    buf: list[str] = []  # accumulated lines for current block
    prefix: str | None = None  # None = paragraph, str = list item prefix

    def flush():
        nonlocal buf, prefix
        if buf:
            result.append(wrap_block(buf, prefix) + "\n")
        buf = []
        prefix = None

    for raw in content.splitlines(keepends=True):
        line = raw.rstrip("\n")
        stripped = line.strip()

        # --- Inside fenced code block ---
        if in_code:
            result.append(raw)
            if stripped.startswith("```"):
                in_code = False
            continue

        # --- Inside multi-line HTML comment ---
        if in_comment:
            result.append(raw)
            if "-->" in line:
                in_comment = False
            continue

        # --- Empty line ---
        if not stripped:
            flush()
            result.append(raw)
            continue

        # --- Code fence open ---
        if stripped.startswith("```"):
            flush()
            in_code = True
            result.append(raw)
            continue

        # --- HTML comment open ---
        if stripped.startswith("<!--"):
            flush()
            result.append(raw)
            if "-->" not in line:
                in_comment = True
            continue

        # --- Other verbatim lines ---
        if is_verbatim(line):
            flush()
            result.append(raw)
            continue

        # --- List item ---
        m = LIST_RE.match(line)
        if m:
            flush()
            item_prefix = m.group(0)  # e.g. "* " or "  1. "
            text = line[len(item_prefix) :]
            buf = [text]
            prefix = item_prefix
            continue

        # --- Continuation of list item (indented, not a new marker) ---
        if prefix is not None and line.startswith(" " * len(prefix)):
            if not LIST_RE.match(line):
                buf.append(line)
                continue
            # New list marker at same indent: flush and start fresh
            flush()
            m2 = LIST_RE.match(line)
            item_prefix = m2.group(0)
            buf = [line[len(item_prefix) :]]
            prefix = item_prefix
            continue

        # --- Regular paragraph text ---
        if prefix is not None:
            flush()
        buf.append(line)

    flush()
    return "".join(result)


def main() -> None:
    check_only = "--check" in sys.argv
    paths = [a for a in sys.argv[1:] if not a.startswith("--")]

    if not paths:
        print(f"Usage: {sys.argv[0]} <file.md.j2> [--check]", file=sys.stderr)
        sys.exit(1)

    changed = False
    for p in paths:
        path = Path(p)
        original = path.read_text()
        wrapped = process(original)
        if wrapped != original:
            changed = True
            if check_only:
                print(f"would change: {path}")
            else:
                path.write_text(wrapped)
                print(f"wrapped: {path}")

    if check_only and changed:
        sys.exit(1)


if __name__ == "__main__":
    main()
