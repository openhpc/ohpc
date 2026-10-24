"""
input.local settings in the guide's commands (DESIGN.md, "input.local variables
in commands"): a command that gives a setting a default, ${name:-default}, uses
the default from input.local.template, and a setting given a default anywhere
has it in every code block, so a pasted command works without input.local.

    cd docs/install && python3 -m unittest discover -s tests
"""

import re
import sys
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
BASE = HERE.parent
sys.path.insert(0, str(BASE))

SETTING_RE = re.compile(r'^(?P<name>\w+)="?\$\{(?P=name):-(?P<default>[^}]*)\}"?')
USE_RE = re.compile(r"\$\{(?P<name>\w+)(?P<default>:-[^}]*)?\}")


def template_defaults() -> dict[str, str]:
    defaults = {}
    for line in (BASE / "input.local.template").read_text().splitlines():
        match = SETTING_RE.match(line.strip())
        if match:
            defaults[match.group("name")] = match.group("default")
    return defaults


def code_lines():
    """(file, line number, text) of every line inside a fenced code block."""
    for path in sorted((BASE / "templates").rglob("*.md.j2")):
        fenced = False
        for number, line in enumerate(path.read_text().splitlines(), start=1):
            if line.lstrip().startswith("```"):
                fenced = not fenced
            elif fenced:
                yield path.relative_to(BASE), number, line


class CommandDefaults(unittest.TestCase):
    def test_defaults_match_the_template(self):
        defaults = template_defaults()
        for path, number, line in code_lines():
            for use in USE_RE.finditer(line):
                name, default = use.group("name"), use.group("default")
                if default and name in defaults:
                    with self.subTest(f"{path}:{number}"):
                        self.assertEqual(default[2:], defaults[name])

    def test_defaulted_settings_always_carry_it(self):
        uses = [(p, n, u) for p, n, line in code_lines() for u in USE_RE.finditer(line)]
        defaulted = {u.group("name") for _, _, u in uses if u.group("default")}
        defaulted &= set(template_defaults())
        self.assertIn("modules_package", defaulted)
        for path, number, use in uses:
            if use.group("name") in defaulted and not use.group("default"):
                with self.subTest(f"{path}:{number}"):
                    self.fail(f"${{{use.group('name')}}} without its default")


if __name__ == "__main__":
    unittest.main()
