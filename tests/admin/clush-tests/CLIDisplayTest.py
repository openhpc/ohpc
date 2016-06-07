#!/usr/bin/env python
# ClusterShell.CLI.Display test suite
# Written by S. Thiell


"""Unit test for CLI.Display"""

import os
import sys
import tempfile
import unittest
from StringIO import StringIO

sys.path.insert(0, '../lib')

from ClusterShell.CLI.Display import Display, WHENCOLOR_CHOICES, VERB_STD
from ClusterShell.CLI.OptionParser import OptionParser

from ClusterShell.MsgTree import MsgTree
from ClusterShell.NodeSet import NodeSet, set_std_group_resolver

from ClusterShell.NodeUtils import GroupResolverConfig



def makeTestFile(text):
    """Create a temporary file with the provided text."""
    f = tempfile.NamedTemporaryFile()
    f.write(text)
    f.flush()
    return f

class CLIDisplayTest(unittest.TestCase):
    """This test case performs a complete CLI.Display verification.  Also
    CLI.OptionParser is used and some parts are verified btw.
    """
    def testDisplay(self):
        """test CLI.Display"""
        parser = OptionParser("dummy")
        parser.install_display_options(verbose_options=True)
        options, _ = parser.parse_args([])

        ns = NodeSet("hostfoo")
        mtree = MsgTree()
        mtree.add("hostfoo", "message0")
        mtree.add("hostfoo", "message1")

        for whencolor in WHENCOLOR_CHOICES: # test whencolor switch
            for label in [True, False]:     # test no-label switch
                options.label = label
                options.whencolor = whencolor
                disp = Display(options)
                # inhibit output
                disp.out = StringIO()
                disp.err = StringIO()
                # test print_* methods...
                disp.print_line(ns, "foo bar")
                disp.print_line_error(ns, "foo bar")
                disp.print_gather(ns, list(mtree.walk())[0][0])
                # test also string nodeset as parameter
                disp.print_gather("hostfoo", list(mtree.walk())[0][0])
                # test line_mode property
                self.assertEqual(disp.line_mode, False)
                disp.line_mode = True
                self.assertEqual(disp.line_mode, True)
                disp.print_gather("hostfoo", list(mtree.walk())[0][0])
                disp.line_mode = False
                self.assertEqual(disp.line_mode, False)

    def testDisplayRegroup(self):
        """test CLI.Display (regroup)"""
        f = makeTestFile("""
# A comment

[Main]
default: local

[local]
map: echo hostfoo
#all:
list: echo all
#reverse:
        """)
        res = GroupResolverConfig(f.name)
        set_std_group_resolver(res)
        try:
            parser = OptionParser("dummy")
            parser.install_display_options(verbose_options=True)
            options, _ = parser.parse_args(["-r"])

            disp = Display(options, color=False)
            self.assertEqual(disp.regroup, True)
            disp.out = StringIO()
            disp.err = StringIO()
            self.assertEqual(disp.line_mode, False)

            ns = NodeSet("hostfoo")

            # nodeset.regroup() is performed by print_gather()
            disp.print_gather(ns, "message0\nmessage1\n")
            self.assertEqual(disp.out.getvalue(),
                "---------------\n@all\n---------------\nmessage0\nmessage1\n\n")
        finally:
            set_std_group_resolver(None)

    def testDisplayClubak(self):
        """test CLI.Display for clubak"""
        parser = OptionParser("dummy")
        parser.install_display_options(separator_option=True, dshbak_compat=True)
        options, _ = parser.parse_args([])
        disp = Display(options)
        self.assertEqual(bool(disp.gather), False)
        self.assertEqual(disp.line_mode, False)
        self.assertEqual(disp.label, True)
        self.assertEqual(disp.regroup, False)
        self.assertEqual(bool(disp.groupsource), False)
        self.assertEqual(disp.noprefix, False)
        self.assertEqual(disp.maxrc, False)
        self.assertEqual(disp.node_count, True)
        self.assertEqual(disp.verbosity, VERB_STD)
