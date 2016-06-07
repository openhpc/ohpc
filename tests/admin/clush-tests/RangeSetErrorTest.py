#!/usr/bin/env python
# ClusterShell.NodeSet.RangeSet error handling test suite
# Written by S. Thiell 2008-09-28


"""Unit test for RangeSet errors"""

import copy
import sys
import unittest

sys.path.insert(0, '../lib')

from ClusterShell.NodeSet import RangeSet
from ClusterShell.NodeSet import RangeSetParseError


class RangeSetErrorTest(unittest.TestCase):

    def _testRS(self, r, exc):
        try:
            rset = RangeSet(r)
            print rset
        except RangeSetParseError, e:
            self.assertEqual(RangeSetParseError, exc)
            return
        except:
            raise
        self.assert_(0, "error not detected/no exception raised")
            

    def testBadUsages(self):
        """test parse errors"""
        self._testRS("", RangeSetParseError)
        self._testRS("-", RangeSetParseError)
        self._testRS("A", RangeSetParseError)
        self._testRS("2-5/a", RangeSetParseError)
        self._testRS("3/2", RangeSetParseError)
        self._testRS("3-/2", RangeSetParseError)
        self._testRS("-3/2", RangeSetParseError)
        self._testRS("-/2", RangeSetParseError)
        self._testRS("4-a/2", RangeSetParseError)
        self._testRS("4-3/2", RangeSetParseError)
        self._testRS("4-5/-2", RangeSetParseError)
        self._testRS("4-2/-2", RangeSetParseError)
        self._testRS("004-002", RangeSetParseError)
        self._testRS("3-59/2,102a", RangeSetParseError)




if __name__ == '__main__':
    suite = unittest.TestLoader().loadTestsFromTestCase(RangeSetErrorTest)
    unittest.TextTestRunner(verbosity=2).run(suite)
