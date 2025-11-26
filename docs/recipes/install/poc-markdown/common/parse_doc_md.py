#!/usr/bin/env python3
"""
Markdown version of parse_doc.py

Parser to extract command-line instructions from markdown documentation
for validation and recipe generation.

Adapted from LaTeX version by Claude Code
"""

import argparse
import re
import sys
from io import StringIO
from pathlib import Path


class ParseDocMarkdownProcessor:
    """Markdown documentation parser for OpenHPC recipes."""

    # Pre-compiled regex patterns
    _PATTERNS = {
        "source_file_start": re.compile(r"<!-- SOURCE_FILE: (\S+) -->"),
        "source_file_end": re.compile(r"<!-- END_SOURCE_FILE: (\S+) -->"),
        "sms_prompt": re.compile(r"\[sms\]# (.+)"),
        "sms_here_doc": re.compile(r"\[sms\]# (.+ <<-?\s*([^ ]+).*)"),
        "sms_continuation": re.compile(r"\[sms\]# (.+) \\$"),
        "sms_do_loop": re.compile(r"\[sms\]# (.+[ ]*;[ ]*do)$"),
        "header_comment": re.compile(r"<!-- ohpc_comment_header (.+) -->"),
        "validation_comment": re.compile(r"<!-- ohpc_validation_comment (.+) -->"),
        "command_directive": re.compile(r"<!-- ohpc_command (.+) -->"),
        "validation_newline": re.compile(r"<!-- ohpc_validation_newline -->"),
        "code_block_start": re.compile(r"^```\s*(\w*)"),
        "code_block_end": re.compile(r"^```\s*$"),
        "done_line": re.compile(r"[^#]*done"),
        "anchor_ref": re.compile(r"\[([^\]]+)\]\(#([^)]+)\)"),
        "if_statement": re.compile(r"^\s*if\s+"),
        "else_statement": re.compile(r"^\s*else\s*$"),
        "elif_statement": re.compile(r"^\s*elif\s+"),
        "fi_statement": re.compile(r"^\s*fi\s*$"),
    }

    def __init__(self):
        self._input_dir = Path()

    def process_file(self, file_path: Path) -> str:
        """Process markdown file and return generated bash script."""
        self._input_dir = file_path.parent.resolve()

        # Generate script
        return self._generate_script(file_path)

    def _generate_script(self, file_path: Path) -> str:
        """Generate bash script from markdown."""
        output = StringIO()
        output.write("#!/bin/bash\n")

        # Read the markdown file
        with open(file_path, 'r', encoding='utf-8') as f:
            lines = f.readlines()

        self._line_iterator = iter(lines)
        self._in_ohpc_run = False
        self._in_code_block = False
        self._indent_level = 0

        # Process lines
        try:
            while True:
                line = next(self._line_iterator)
                self._process_line(line, output)
        except StopIteration:
            pass

        return output.getvalue()

    def _process_line(self, line: str, output: StringIO) -> None:
        """Process a single line."""
        line = line.rstrip("\n\r")

        # Handle ohpc_run blocks
        if "<!-- begin_ohpc_run -->" in line:
            self._in_ohpc_run = True
            return
        elif "<!-- end_ohpc_run -->" in line:
            self._in_ohpc_run = False
            self._in_code_block = False
            return

        if not self._in_ohpc_run:
            return

        # Handle code blocks
        if match := self._PATTERNS["code_block_start"].match(line):
            self._in_code_block = True
            return
        elif self._PATTERNS["code_block_end"].match(line):
            self._in_code_block = False
            return

        # Process validation newline
        if self._PATTERNS["validation_newline"].match(line.strip()):
            output.write("\n")
            return

        # Process directives
        if match := self._PATTERNS["header_comment"].match(line.strip()):
            comment = self._process_refs(match.group(1))
            self._write_header(output, comment)
        elif match := self._PATTERNS["validation_comment"].match(line.strip()):
            comment = self._process_refs(match.group(1))
            output.write(f"# {comment}\n")
        elif match := self._PATTERNS["command_directive"].match(line.strip()):
            cmd = match.group(1).strip()
            self._write_indented_command(cmd, output)
        elif self._in_code_block:
            self._process_command_line(line, output)

    def _write_indented_command(self, cmd: str, output: StringIO) -> None:
        """Write command with proper indentation based on if/else/fi blocks."""
        # Check what type of statement this is
        is_if = self._PATTERNS["if_statement"].match(cmd)
        is_elif = self._PATTERNS["elif_statement"].match(cmd)
        is_else = self._PATTERNS["else_statement"].match(cmd)
        is_fi = self._PATTERNS["fi_statement"].match(cmd)

        # Adjust indentation for else and fi (decrease before writing)
        if is_else or is_fi:
            self._indent_level = max(0, self._indent_level - 1)

        # Write the command with current indentation
        indent = "  " * self._indent_level
        output.write(f"{indent}{cmd}\n")

        # Adjust indentation after writing (increase for if/elif/else)
        if is_if or is_elif:
            self._indent_level += 1
        elif is_else:
            self._indent_level += 1

    def _process_command_line(self, line: str, output: StringIO) -> None:
        """Process command lines in code blocks."""
        line = line.strip()

        # Skip empty lines and comments that aren't commands
        if not line or (line.startswith("#") and not line.startswith("# ")):
            return

        # HERE document
        if match := self._PATTERNS["sms_here_doc"].search(line):
            self._process_here_doc(match, output)
        # Line continuation
        elif match := self._PATTERNS["sms_continuation"].search(line):
            self._process_continuation(match, output)
        # Do loop
        elif match := self._PATTERNS["sms_do_loop"].search(line):
            self._process_do_loop(match, output)
        # Regular SMS prompt
        elif match := self._PATTERNS["sms_prompt"].search(line):
            cmd = match.group(1)
            indent = "  " * self._indent_level
            output.write(f"{indent}{cmd}\n")
        # Plain comments
        elif line.startswith("#"):
            indent = "  " * self._indent_level
            output.write(f"{indent}{line}\n")

    def _process_here_doc(self, match: re.Match, output: StringIO) -> None:
        """Process HERE document."""
        cmd = match.group(1)
        delimiter = match.group(2).strip()

        indent = "  " * self._indent_level
        output.write(f"{indent}{cmd}\n")

        # Process remaining lines until delimiter
        try:
            while True:
                line = next(self._line_iterator)
                line = line.rstrip("\n\r").strip()

                # Check if we hit code block end
                if self._PATTERNS["code_block_end"].match(line):
                    break

                output.write(f"{indent}{line}\n")
                if line.startswith(delimiter):
                    break
        except StopIteration:
            pass

    def _process_continuation(self, match: re.Match, output: StringIO) -> None:
        """Process line continuation."""
        cmd = match.group(1)
        indent = "  " * self._indent_level
        output.write(f"{indent}{cmd} \\")

        try:
            while True:
                line = next(self._line_iterator)
                line = line.rstrip("\n\r").strip()

                # Check for code block end
                if self._PATTERNS["code_block_end"].match(line):
                    output.write("\n")
                    break

                # Skip [sms]# prefix if present
                if line.startswith("[sms]#"):
                    line = line[6:].strip()

                if line.endswith("\\"):
                    output.write(f"\n{indent}\t{line}")
                else:
                    output.write(f"\n{indent}\t{line}\n")
                    break
        except StopIteration:
            output.write("\n")

    def _process_do_loop(self, match: re.Match, output: StringIO) -> None:
        """Process do loop with proper indentation."""
        cmd = match.group(1)
        indent = "  " * self._indent_level
        output.write(f"{indent}{cmd}\n")

        try:
            while True:
                line = next(self._line_iterator)
                line = line.rstrip("\n\r").strip()

                # Check for code block end
                if self._PATTERNS["code_block_end"].match(line):
                    break

                # Skip [sms]# prefix if present
                if line.startswith("[sms]#"):
                    line = line[6:].strip()

                # Add indentation for lines inside the do loop
                if not self._PATTERNS["done_line"].search(line):
                    output.write(f"{indent}   {line}\n")
                else:
                    output.write(f"{indent}{line}\n")

                if self._PATTERNS["done_line"].search(line):
                    break
        except StopIteration:
            pass

    def _process_refs(self, comment: str) -> str:
        """Process markdown anchor references."""
        # Extract text from [text](#anchor) format
        def replace_ref(match):
            text = match.group(1)
            anchor = match.group(2)
            return text

        return self._PATTERNS["anchor_ref"].sub(replace_ref, comment)

    def _write_header(self, output: StringIO, comment: str) -> None:
        """Write formatted header comment."""
        separator = "-" * len(comment)
        output.write(f"\n# {separator}\n# {comment}\n# {separator}\n")


def main() -> None:
    """Main entry point."""
    if len(sys.argv) < 2:
        print(
            "Usage: parse_doc_md.py <markdown_file>",
            file=sys.stderr,
        )
        sys.exit(1)

    parser = argparse.ArgumentParser(
        description="Parse Markdown documentation files to extract command-line instructions",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    parser.add_argument("filename", type=Path, help="Markdown file to process")

    try:
        args = parser.parse_args()

        processor = ParseDocMarkdownProcessor()
        result = processor.process_file(args.filename)

        sys.stdout.write(result)

    except (IOError, ValueError) as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Unexpected error: {e}", file=sys.stderr)
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
