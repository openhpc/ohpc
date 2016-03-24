#!/usr/bin/env python
# ClusterShell.CLI.OptionParser test suite
# Written by S. Thiell 2010-09-25


"""Unit test for CLI.OptionParser"""

from optparse import OptionConflictError
import os
import sys
import tempfile
import unittest

sys.path.insert(0, '../lib')

from ClusterShell.CLI.OptionParser import OptionParser


class CLIOptionParserTest(unittest.TestCase):
    """This test case performs a complete CLI.OptionParser
    verification.
    """
    def testOptionParser(self):
        """test CLI.OptionParser (1)"""
        parser = OptionParser("dummy")
        parser.install_nodes_options()
        parser.install_display_options(verbose_options=True)
        parser.install_filecopy_options()
        parser.install_connector_options()
        options, _ = parser.parse_args([])

    def testOptionParser2(self):
        """test CLI.OptionParser (2)"""
        parser = OptionParser("dummy")
        parser.install_nodes_options()
        parser.install_display_options(verbose_options=True, separator_option=True)
        parser.install_filecopy_options()
        parser.install_connector_options()
        options, _ = parser.parse_args([])

    def testOptionParserConflicts(self):
        """test CLI.OptionParser (conflicting options)"""
        parser = OptionParser("dummy")
        parser.install_nodes_options()
        parser.install_display_options(dshbak_compat=True)
        self.assertRaises(OptionConflictError, parser.install_filecopy_options)

    def testOptionParserClubak(self):
        """test CLI.OptionParser for clubak"""
        parser = OptionParser("dummy")
        parser.install_nodes_options()
        parser.install_display_options(separator_option=True, dshbak_compat=True)
        options, _ = parser.parse_args([])


if __name__ == '__main__':
    suites = [unittest.TestLoader().loadTestsFromTestCase(CLIOptionParserTest)]
    unittest.TextTestRunner(verbosity=2).run(unittest.TestSuite(suites))
