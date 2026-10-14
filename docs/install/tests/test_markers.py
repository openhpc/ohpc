"""
Marker-grammar contract for mkdoc.py's script extraction.

tests/markers.md exercises every ``ohpc_*`` directive and the section markers;
tests/markers.sh is the script they must produce. The same fixture holds the
browser-side port (explorer/engine.js) to the same output via
explorer/test/markers.mjs, so a change to the grammar is made in both
implementations and the fixture together.

    cd docs/install && python3 -m unittest discover -s tests
"""

import io
import sys
import unittest
from contextlib import redirect_stderr
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))

import mkdoc


class MarkerExtraction(unittest.TestCase):
    def test_fixture(self):
        markdown = (HERE / "markers.md").read_text(encoding="utf-8")
        expected = (HERE / "markers.sh").read_text(encoding="utf-8")
        self.assertEqual(mkdoc.extract_recipe_script(markdown) + "\n", expected)

    def test_unterminated_directive_fails(self):
        broken = "<!-- ohpc_begin -->\n<!-- ohpc_if_set enable_x\n<!-- ohpc_end -->\n"
        with redirect_stderr(io.StringIO()), self.assertRaises(SystemExit):
            mkdoc.extract_recipe_script(broken)

    def test_long_lines_are_counted(self):
        long_line = "x" * 90
        markdown = f"```bash\n{long_line}\n```\n```text\n{long_line}\n```\n"
        with redirect_stderr(io.StringIO()):
            self.assertEqual(mkdoc.check_line_lengths(markdown), 1)


if __name__ == "__main__":
    unittest.main()
