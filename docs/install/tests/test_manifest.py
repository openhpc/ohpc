"""
Pick-one settings in input.local.template, as mkdoc.py --manifest reads them
for the explorer: NAME_choices='[...]' (a JSON list) directly before
NAME="${NAME:-default}".

    cd docs/install && python3 -m unittest discover -s tests
"""

import sys
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))

import mkdoc


def choices_of(template: str) -> list[dict]:
    with tempfile.TemporaryDirectory() as tmp:
        (Path(tmp) / "input.local.template").write_text(template, encoding="utf-8")
        return mkdoc.manifest_choices(Path(tmp))


class ManifestChoices(unittest.TestCase):
    def test_template(self):
        # whatever input.local.template declares parses, each default in its list
        for choice in mkdoc.manifest_choices():
            self.assertIn(choice["default"], choice["choices"])

    def test_description_is_the_comment_block(self):
        (choice,) = choices_of(
            '# Pick a tool:\n# a or b\nx_choices=\'["a","b"]\'\nx="${x:-b}"\n'
        )
        self.assertEqual(choice["description"], "Pick a tool: a or b")
        self.assertEqual(choice["choices"], ["a", "b"])
        self.assertEqual(choice["default"], "b")

    def test_bad_json(self):
        with self.assertRaisesRegex(ValueError, "x_choices"):
            choices_of('x_choices=\'["a",]\'\nx="${x:-a}"\n')

    def test_not_a_list_of_strings(self):
        with self.assertRaisesRegex(ValueError, "list of non-empty strings"):
            choices_of("x_choices='[1, 2]'\nx=\"${x:-1}\"\n")

    def test_default_not_in_list(self):
        with self.assertRaisesRegex(ValueError, "not one of"):
            choices_of('x_choices=\'["a","b"]\'\nx="${x:-c}"\n')

    def test_setting_must_follow(self):
        with self.assertRaisesRegex(ValueError, "must be followed by"):
            choices_of('x_choices=\'["a"]\'\ny="${y:-a}"\n')
        with self.assertRaisesRegex(ValueError, "not followed"):
            choices_of("x_choices='[\"a\"]'\n")


if __name__ == "__main__":
    unittest.main()
