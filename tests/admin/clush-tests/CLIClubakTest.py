#!/usr/bin/env python
# scripts/clubak.py tool test suite
# Written by S. Thiell


"""Unit test for CLI/Clubak.py"""

import sys
import unittest

from TLib import *
from ClusterShell.CLI.Clubak import main


class CLIClubakTest(unittest.TestCase):
    """Unit test class for testing CLI/Clubak.py"""

    def _clubak_t(self, args, input, expected_stdout, expected_rc=0,
                  expected_stderr=None):
        CLI_main(self, main, [ 'clubak' ] + args, input, expected_stdout,
                 expected_rc, expected_stderr)

    def test_000_noargs(self):
        """test clubak (no argument)"""
        outfmt = "---------------\n%s\n---------------\n bar\n"
        self._clubak_t([], "foo: bar\n", outfmt % "foo")
        self._clubak_t([], "foo space: bar\n", outfmt % "foo space")
        self._clubak_t([], "foo space1: bar\n", outfmt % "foo space1")
        self._clubak_t([], "foo space1: bar\nfoo space2: bar", outfmt % "foo space1" + outfmt % "foo space2")
        self._clubak_t([], ": bar\n", "", 1, "clubak: no node found (\": bar\")\n")
        self._clubak_t([], "foo[: bar\n", outfmt % "foo[")
        self._clubak_t([], "]o[o]: bar\n", outfmt % "]o[o]")
        self._clubak_t([], "foo:\n", "---------------\nfoo\n---------------\n\n")
        self._clubak_t([], "foo: \n", "---------------\nfoo\n---------------\n \n")

    def test_001_verbosity(self):
        """test clubak (-q/-v/-d)"""
        outfmt = "INPUT foo: bar\n---------------\n%s\n---------------\n bar\n"
        self._clubak_t(["-d"], "foo: bar\n", outfmt % "foo", 0, "line_mode=False gather=False tree_depth=1\n")
        self._clubak_t(["-d", "-b"], "foo: bar\n", outfmt % "foo", 0, "line_mode=False gather=True tree_depth=1\n")
        self._clubak_t(["-d", "-L"], "foo: bar\n", "INPUT foo: bar\nfoo:  bar\n", 0, "line_mode=True gather=False tree_depth=1\n")
        self._clubak_t(["-v"], "foo: bar\n", outfmt % "foo", 0)
        self._clubak_t(["-v", "-b"], "foo: bar\n", outfmt % "foo", 0)
        outfmt = "---------------\n%s\n---------------\n bar\n"
        # no node count with -q
        self._clubak_t(["-q", "-b"], "foo[1-5]: bar\n", outfmt % "foo[1-5]", 0)

    def test_002_b(self):
        """test clubak (gather -b)"""
        outfmt = "---------------\n%s\n---------------\n bar\n"
        self._clubak_t(["-b"], "foo: bar\n", outfmt % "foo")
        self._clubak_t(["-b"], "foo space: bar\n", outfmt % "foo space")
        self._clubak_t(["-b"], "foo space1: bar\n", outfmt % "foo space1")
        self._clubak_t(["-b"], "foo space1: bar\nfoo space2: bar", outfmt % "foo space[1-2] (2)")
        self._clubak_t(["-b"], "foo space1: bar\nfoo space2: foo", "---------------\nfoo space1\n---------------\n bar\n---------------\nfoo space2\n---------------\n foo\n")
        self._clubak_t(["-b"], ": bar\n", "", 1, "clubak: no node found (\": bar\")\n")
        self._clubak_t(["-b"], "foo[: bar\n", outfmt % "foo[")
        self._clubak_t(["-b"], "]o[o]: bar\n", outfmt % "]o[o]")
        self._clubak_t(["-b"], "foo:\n", "---------------\nfoo\n---------------\n\n")
        self._clubak_t(["-b"], "foo: \n", "---------------\nfoo\n---------------\n \n")

    def test_003_L(self):
        """test clubak (line mode -L)"""
        self._clubak_t(["-L"], "foo: bar\n", "foo:  bar\n")
        self._clubak_t(["-L", "-S", ": "], "foo: bar\n", "foo: bar\n")
        self._clubak_t(["-bL"], "foo: bar\n", "foo:  bar\n")
        self._clubak_t(["-bL", "-S", ": "], "foo: bar\n", "foo: bar\n")

    def test_004_N(self):
        """test clubak (no header -N)"""
        self._clubak_t(["-N"], "foo: bar\n", "\n bar\n")
        self._clubak_t(["-NL"], "foo: bar\n", " bar\n")
        self._clubak_t(["-N", "-S", ": "], "foo: bar\n", "\nbar\n")
        self._clubak_t(["-bN"], "foo: bar\n", "\n bar\n")
        self._clubak_t(["-bN", "-S", ": "], "foo: bar\n", "\nbar\n")

    def test_005_fast(self):
        """test clubak (fast mode --fast)"""
        outfmt = "---------------\n%s\n---------------\n bar\n"
        self._clubak_t(["--fast"], "foo: bar\n", outfmt % "foo")
        self._clubak_t(["-b", "--fast"], "foo: bar\n", outfmt % "foo")
        self._clubak_t(["-b", "--fast"], "foo2: bar\nfoo1: bar\nfoo4: bar", outfmt % "foo[1-2,4] (3)")
        # check conflicting options
        self._clubak_t(["-L", "--fast"], "foo2: bar\nfoo1: bar\nfoo4: bar", '', 2, "error: incompatible tree options\n")

    def test_006_tree(self):
        """test clubak (tree mode --tree)"""
        outfmt = "---------------\n%s\n---------------\n bar\n"
        self._clubak_t(["--tree"], "foo: bar\n", outfmt % "foo")
        self._clubak_t(["--tree", "-L"], "foo: bar\n", "foo:\n bar\n")
        input = """foo1:bar
foo2:bar
foo1:moo
foo1:bla
foo2:m00
foo2:bla
foo1:abc
"""
        self._clubak_t(["--tree", "-L"], input, "foo[1-2]:\nbar\nfoo2:\n  m00\n  bla\nfoo1:\n  moo\n  bla\n  abc\n")
        # check conflicting options
        self._clubak_t(["--tree", "--fast"], input, '', 2, "error: incompatible tree options\n")

    def test_007_interpret_keys(self):
        """test clubak (--interpret-keys)"""
        outfmt = "---------------\n%s\n---------------\n bar\n"
        self._clubak_t(["--interpret-keys=auto"], "foo: bar\n", outfmt % "foo")
        self._clubak_t(["-b", "--interpret-keys=auto"], "foo: bar\n", outfmt % "foo")
        self._clubak_t(["-b", "--interpret-keys=never"], "foo: bar\n", outfmt % "foo")
        self._clubak_t(["-b", "--interpret-keys=always"], "foo: bar\n", outfmt % "foo")
        self._clubak_t(["-b", "--interpret-keys=always"], "foo[1-3]: bar\n", outfmt % "foo[1-3] (3)")
        self._clubak_t(["-b", "--interpret-keys=auto"], "[]: bar\n", outfmt % "[]")
        self._clubak_t(["-b", "--interpret-keys=never"], "[]: bar\n", outfmt % "[]")
        self._clubak_t(["-b", "--interpret-keys=always"], "[]: bar\n", '', 1, "Parse error: empty node name: \"[]\"\n")

    def test_008_color(self):
        """test clubak (--color)"""
        outfmt = "---------------\n%s\n---------------\n bar\n"
        self._clubak_t(["-b"], "foo: bar\n", outfmt % "foo")
        self._clubak_t(["-b", "--color=never"], "foo: bar\n", outfmt % "foo")
        self._clubak_t(["-b", "--color=auto"], "foo: bar\n", outfmt % "foo")
        self._clubak_t(["-L", "--color=always"], "foo: bar\n", "\x1b[34mfoo: \x1b[0m bar\n")
        self._clubak_t(["-b", "--color=always"], "foo: bar\n", "\x1b[34m---------------\nfoo\n---------------\x1b[0m\n bar\n")

    def test_009_diff(self):
        """test clubak (--diff)"""
        self._clubak_t(["--diff"], "foo1: bar\nfoo2: bar", "")
        self._clubak_t(["--diff"], "foo1: bar\nfoo2: BAR\nfoo2: end\nfoo1: end",
                                   "--- foo1\n+++ foo2\n@@ -1,2 +1,2 @@\n- bar\n+ BAR\n  end\n")
        self._clubak_t(["--diff"], "foo1: bar\nfoo2: BAR\nfoo3: bar\nfoo2: end\nfoo1: end\nfoo3: end",
                                   "--- foo[1,3] (2)\n+++ foo2\n@@ -1,2 +1,2 @@\n- bar\n+ BAR\n  end\n")
        self._clubak_t(["--diff", "--color=always"], "foo1: bar\nfoo2: BAR\nfoo3: bar\nfoo2: end\nfoo1: end\nfoo3: end",
                                   "\x1b[1m--- foo[1,3] (2)\x1b[0m\n\x1b[1m+++ foo2\x1b[0m\n\x1b[36m@@ -1,2 +1,2 @@\x1b[0m\n\x1b[31m- bar\x1b[0m\n\x1b[32m+ BAR\x1b[0m\n  end\n")
        self._clubak_t(["--diff", "-d"], "foo: bar\n", "INPUT foo: bar\n", 0, "line_mode=False gather=True tree_depth=1\n")
        self._clubak_t(["--diff", "-L"], "foo1: bar\nfoo2: bar", "", 2, "clubak: error: option mismatch (diff not supported in line_mode)\n")
