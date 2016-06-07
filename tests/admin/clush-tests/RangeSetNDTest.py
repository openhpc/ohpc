#!/usr/bin/env python
# ClusterShell.RangeSet.RangeSetND test suite
# Written by S. Thiell


"""Unit test for RangeSetND"""

import sys
import unittest

sys.path.insert(0, '../lib')

from ClusterShell.RangeSet import RangeSet, RangeSetND


class RangeSetNDTest(unittest.TestCase):

    def _testRS(self, test, res, length):
        r1 = RangeSetND(test, autostep=3)
        self.assertEqual(str(r1), res)
        self.assertEqual(len(r1), length)

    def test_simple(self):
        # Test constructors
        self._testRS(None, "", 0)
        self._testRS([["0-10"], ["40-60"]], "0-10,40-60\n", 32)
        self._testRS([["0-2", "1-2"], ["10", "3-5"]], "0-2; 1-2\n10; 3-5\n", 9)
        self._testRS([[0, 1], [0, 2], [2, 2], [2, 1], [1, 1], [1, 2], [10, 4], [10, 5], [10, 3]], "0-2; 1-2\n10; 3-5\n", 9)
        self._testRS([(0, 4), (0, 5), (1, 4), (1, 5)], "0-1; 4-5\n", 4)
        # construct with copy_rangeset=False
        r0 = RangeSet("0-10,30-40,50")
        r1 = RangeSet("200-202")
        rn = RangeSetND([[r0, r1]], copy_rangeset=False)
        self.assertEqual(str(rn), "0-10,30-40,50; 200-202\n")
        self.assertEqual(len(rn), 69)

    def test_vectors(self):
        rn = RangeSetND([["0-10", "1-2"], ["5-60", "2"]])
        # vectors() should perform automatic folding
        self.assertEqual([[RangeSet("0-60"), RangeSet("2")], [RangeSet("0-10"), RangeSet("1")]], list(rn.vectors()))
        self.assertEqual(str(rn), "0-60; 2\n0-10; 1\n")
        self.assertEqual(len(rn), 72)

    def test_nonzero(self):
        r0 = RangeSetND()
        if r0:
            self.assertFalse("nonzero failed")
        r1 = RangeSetND([["0-10"], ["40-60"]])
        if not r1:
            self.assertFalse("nonzero failed")

    def test_eq(self):
        r0 = RangeSetND()
        r1 = RangeSetND()
        r2 = RangeSetND([["0-10", "1-2"], ["40-60", "1-3"]])
        r3 = RangeSetND([["0-10"], ["40-60"]])
        self.assertEqual(r0, r1)
        self.assertNotEqual(r0, r2)
        self.assertNotEqual(r0, r3)
        self.assertNotEqual(r2, r3)
        self.assertFalse(r3 == "foobar") # NotImplemented => object address comparison

    def test_dim(self):
        r0 = RangeSetND()
        self.assertEqual(r0.dim(), 0)
        r1 = RangeSetND([["0-10", "1-2"], ["40-60", "1-3"]])
        self.assertEqual(r1.dim(), 2)

    def test_fold(self):
        r1 = RangeSetND([["0-10", "1-2"], ["5-15,40-60", "1-3"], ["0-4", "3"]])
        r1.fold()
        self.assertEqual(str(r1._veclist), "[[0-15,40-60, 1-3]]")
        self.assertEqual(str(r1), "0-15,40-60; 1-3\n")

    def test_contains(self):
        r1 = RangeSetND([["0-10"], ["40-60"]])
        r2 = RangeSetND()
        self.assertTrue(r2 in r1) # <=> issubset()
        r1 = RangeSetND()
        r2 = RangeSetND([["0-10"], ["40-60"]])
        self.assertFalse(r2 in r1)
        r1 = RangeSetND([["0-10"], ["40-60"]])
        r2 = RangeSetND([["4-8"], ["10,40-41"]])
        self.assertTrue(r2 in r1)
        r1 = RangeSetND([["0-10", "1-2"], ["40-60", "2-5"]])
        r2 = RangeSetND([["4-8", "1"], ["10,40-41", "2"]])
        self.assertTrue(r2 in r1)
        r1 = RangeSetND([["0-10", "1-2"], ["40-60", "2-5"]])
        r2 = RangeSetND([["4-8", "5"], ["10,40-41", "2"]])
        self.assertTrue(r2 not in r1)
        r1 = RangeSetND([["0-10"], ["40-60"]])
        self.assertTrue("10" in r1)
        self.assertTrue(10 in r1)
        self.assertFalse(11 in r1)

    def test_subset_superset(self):
        r1 = RangeSetND([["0-10"], ["40-60"]])
        self.assertTrue(r1.issubset(r1))
        self.assertTrue(r1.issuperset(r1))
        r2 = RangeSetND([["0-10"], ["40-60"]])
        self.assertTrue(r2.issubset(r1))
        self.assertTrue(r1.issubset(r2))
        self.assertTrue(r2.issuperset(r1))
        self.assertTrue(r1.issuperset(r2))
        r1 = RangeSetND([["0-10"], ["40-60"]])
        r2 = RangeSetND()
        self.assertTrue(r2.issubset(r1))
        self.assertFalse(r1.issubset(r2))
        self.assertTrue(r1.issuperset(r2))
        self.assertFalse(r2.issuperset(r1))
        r1 = RangeSetND([["0-10"], ["40-60"]])
        r2 = RangeSetND([["4"], ["10,40-41"]])
        self.assertFalse(r1.issubset(r2))
        self.assertFalse(r1 < r2)
        self.assertTrue(r2.issubset(r1))
        self.assertTrue(r2 < r1)
        self.assertTrue(r1.issuperset(r2))
        self.assertTrue(r1 > r2)
        self.assertFalse(r2.issuperset(r1))
        self.assertFalse(r2 > r1)
        r1 = RangeSetND([["0-10", "1-2"], ["40-60", "2-5"]])
        r2 = RangeSetND([["4-8", "1"], ["10,40-41", "2"]])
        self.assertFalse(r1.issubset(r2))
        self.assertFalse(r1 < r2)
        self.assertTrue(r2.issubset(r1))
        self.assertTrue(r2 < r1)
        self.assertTrue(r1.issuperset(r2))
        self.assertTrue(r1 > r2)
        self.assertFalse(r2.issuperset(r1))
        self.assertFalse(r2 > r1)

    def test_sorting(self):
        # Test internal sorting algo
        # sorting condition (1) -- see RangeSetND._sort()
        self._testRS([["40-60", "5"], ["10-12", "6"]], "40-60; 5\n10-12; 6\n", 24)
        # sorting condition (2)
        self._testRS([["40-42", "5,7"], ["10-12", "6"]], "40-42; 5,7\n10-12; 6\n", 9)
        self._testRS([["40-42", "5"], ["10-12", "6-7"]], "10-12; 6-7\n40-42; 5\n", 9)
        # sorting condition (3)
        self._testRS([["40-60", "5"], ["10-30", "6"]], "10-30; 6\n40-60; 5\n", 42)
        self._testRS([["10-30", "3", "5"], ["10-30", "2", "6"]], "10-30; 2; 6\n10-30; 3; 5\n", 42)
        self._testRS([["10-30", "2", "6"], ["10-30", "3", "5"]], "10-30; 2; 6\n10-30; 3; 5\n", 42)
        # sorting condition (4)
        self._testRS([["10-30", "2,6", "6"], ["10-30", "2-3", "5"]], "10-30; 2-3; 5\n10-30; 2,6; 6\n", 84)
        # the following test triggers folding loop protection
        self._testRS([["40-60", "5"], ["30-50", "6"]], "30-50; 6\n40-60; 5\n", 42)
        # 1D
        self._testRS([["40-60"], ["10-12"]], "10-12,40-60\n", 24)

    def test_folding(self):
        self._testRS([["0-10"], ["11-60"]],
                     "0-60\n", 61)
        self._testRS([["0-2", "1-2"], ["3", "1-2"]],
                     "0-3; 1-2\n", 8)
        self._testRS([["3", "1-3"], ["0-2", "1-2"]],
                     "0-2; 1-2\n3; 1-3\n", 9)
        self._testRS([["0-2", "1-2"], ["3", "1-3"]],
                     "0-2; 1-2\n3; 1-3\n", 9)
        self._testRS([["0-2", "1-2"], ["1-3", "1-3"]],
                     "1-2; 1-3\n0,3; 1-2\n3; 3\n", 11)
        self._testRS([["0-2", "1-2", "0-4"], ["3", "1-2", "0-5"]],
                     "0-2; 1-2; 0-4\n3; 1-2; 0-5\n", 42)
        self._testRS([["0-2", "1-2", "0-4"], ["1-3", "1-3", "0-4"]],
                     "1-2; 1-3; 0-4\n0,3; 1-2; 0-4\n3; 3; 0-4\n", 55)
        # the following test triggers folding loop protection
        self._testRS([["0-100", "50-200"], ["2-101", "49"]],
                     "0-100; 50-200\n2-101; 49\n", 15351)
        # the following test triggers full expand
        veclist = []
        for v1, v2, v3 in zip(range(30), range(5, 35), range(10, 40)):
            veclist.append((v1, v2, v3))
        self._testRS(veclist, "0; 5; 10\n1; 6; 11\n2; 7; 12\n3; 8; 13\n4; 9; 14\n5; 10; 15\n6; 11; 16\n7; 12; 17\n8; 13; 18\n9; 14; 19\n10; 15; 20\n11; 16; 21\n12; 17; 22\n13; 18; 23\n14; 19; 24\n15; 20; 25\n16; 21; 26\n17; 22; 27\n18; 23; 28\n19; 24; 29\n20; 25; 30\n21; 26; 31\n22; 27; 32\n23; 28; 33\n24; 29; 34\n25; 30; 35\n26; 31; 36\n27; 32; 37\n28; 33; 38\n29; 34; 39\n", 30)

    def test_union(self):
        rn1 = RangeSetND([["10-100", "1-3"], ["1100-1300", "2-3"]])
        self.assertEqual(str(rn1), "1100-1300; 2-3\n10-100; 1-3\n")
        self.assertEqual(len(rn1), 675)
        rn2 = RangeSetND([["1100-1200", "1"], ["10-49", "1,3"]])
        self.assertEqual(str(rn2), "1100-1200; 1\n10-49; 1,3\n")
        self.assertEqual(len(rn2), 181)
        rnu = rn1.union(rn2)
        self.assertEqual(str(rnu), "1100-1300; 2-3\n10-100; 1-3\n1100-1200; 1\n")
        self.assertEqual(len(rnu), 776)
        rnu2 = rn1 | rn2
        self.assertEqual(str(rnu2), "1100-1300; 2-3\n10-100; 1-3\n1100-1200; 1\n")
        self.assertEqual(len(rnu2), 776)
        self.assertEqual(rnu, rnu2) # btw test __eq__
        self.assertNotEqual(rnu, rn1) # btw test __eq__
        self.assertNotEqual(rnu, rn2) # btw test __eq__
        try:
            dummy = rn1 | "foobar"
            self.assertFalse("TypeError not raised")
        except TypeError:
            pass
        # binary error
        if sys.version_info >= (2, 5, 0):
            rn1 = RangeSetND([["10", "10-13"], ["10", "9-12"]])
            rn2 = RangeSetND([["1100-1200", "1"], ["10-49", "1,3"]])
            rn1 |= rn2
            self.assertEqual(str(rn2), "1100-1200; 1\n10-49; 1,3\n")
            self.assertEqual(len(rn2), 181)
            rn2 = set([3, 5])
            self.assertRaises(TypeError, rn1.__ior__, rn2)

    def test_difference(self):
        rn1 = RangeSetND([["10", "10-13"], ["0-3", "1-2"]])
        rn2 = RangeSetND([["10", "12"]])
        self.assertEqual(len(rn1), 12)
        rnres = rn1.difference(rn2)
        self.assertEqual(str(rnres), "0-3; 1-2\n10; 10-11,13\n")
        self.assertEqual(len(rnres), 11)

        rn1 = RangeSetND([["0-2", "1-3", "4-5"]])
        rn2 = RangeSetND([["0-2", "1-3", "4"]])
        rnres = rn1.difference(rn2)
        self.assertEqual(str(rnres), "0-2; 1-3; 5\n")
        self.assertEqual(len(rnres), 9)

        rn1 = RangeSetND([["0-2", "1-3", "4-5"]])
        rn2 = RangeSetND([["10-40", "20-120", "0-100"]])
        rnres = rn1.difference(rn2)
        self.assertEqual(str(rnres), "0-2; 1-3; 4-5\n")
        self.assertEqual(len(rnres), 18)

        rn1 = RangeSetND([["0-2", "1-3", "4-5"]])
        rn2 = RangeSetND([["10-40", "20-120", "100-200"]])
        rnres = rn1.difference(rn2)
        self.assertEqual(str(rnres), "0-2; 1-3; 4-5\n")
        self.assertEqual(len(rnres), 18)

        rnres2 = rn1 - rn2
        self.assertEqual(str(rnres2), "0-2; 1-3; 4-5\n")
        self.assertEqual(len(rnres2), 18)
        self.assertEqual(rnres, rnres2) # btw test __eq__

        try:
            dummy = rn1 - "foobar"
            self.assertFalse("TypeError not raised")
        except TypeError:
            pass


    def test_difference_update(self):
        rn1 = RangeSetND([["10", "10-13"], ["10", "9-12"]])
        rn2 = RangeSetND([["10", "10"]])
        rn1.difference_update(rn2)
        self.assertEqual(len(rn1), 4)
        self.assertEqual(str(rn1), "10; 9,11-13\n")

        rn1 = RangeSetND([["10", "10-13"], ["10", "9-12"], ["8-9", "12-15"]])
        rn2 = RangeSetND([["10", "10"], ["9", "12-15"]])
        rn1.difference_update(rn2)
        self.assertEqual(len(rn1), 8)
        self.assertEqual(str(rn1), "8; 12-15\n10; 9,11-13\n")

        rn1 = RangeSetND([["10", "10-13"], ["10", "9-12"], ["8-9", "12-15"]])
        rn2 = RangeSetND([["10", "10"], ["9", "12-15"], ["10-12", "11-15"], ["11", "14"]])
        rn1.difference_update(rn2)
        self.assertEqual(len(rn1), 5)
        self.assertEqual(str(rn1), "8; 12-15\n10; 9\n")

        rn1 = RangeSetND([["10", "10-13"], ["10", "9-12"], ["8-9", "12-15"], ["10", "10-13"], ["10", "12-16"], ["9", "13-16"]])
        rn2 = RangeSetND([["10", "10"], ["9", "12-15"], ["10-12", "11-15"], ["11", "14"]])
        rn1.difference_update(rn2)
        self.assertEqual(len(rn1), 7)
        # no pre-fold (self._veclist)
        self.assertEqual(str(rn1), "8; 12-15\n9-10; 16\n10; 9\n")
        # pre-fold (self.veclist)
        #self.assertEqual(str(rn1), "8; 12-15\n10; 9,16\n9; 16\n")

        # strict mode
        rn1 = RangeSetND([["10", "10-13"], ["10", "9-12"], ["8-9", "12-15"]])
        rn2 = RangeSetND([["10", "10"], ["9", "12-15"], ["10-12", "11-15"], ["11", "14"]])
        self.assertRaises(KeyError, rn1.difference_update, rn2, strict=True)

        if sys.version_info >= (2, 5, 0):
            rn1 = RangeSetND([["10", "10-13"], ["10", "9-12"]])
            rn2 = RangeSetND([["10", "10"]])
            rn1 -= rn2
            self.assertEqual(str(rn1), "10; 9,11-13\n")
            self.assertEqual(len(rn1), 4)
            # binary error
            rn2 = set([3, 5])
            self.assertRaises(TypeError, rn1.__isub__, rn2)

    def test_intersection(self):
        rn1 = RangeSetND([["10", "10-13"], ["10", "9-12"], ["8-9", "12-15"]])
        self.assertEqual(len(rn1), 13)
        self.assertEqual(str(rn1), "8-9; 12-15\n10; 9-13\n")
        rn2 = RangeSetND([["10", "10"], ["9", "12-15"]])
        self.assertEqual(len(rn2), 5)
        self.assertEqual(str(rn2), "9; 12-15\n10; 10\n")
        rni = rn1.intersection(rn2)
        self.assertEqual(len(rni), 5)
        self.assertEqual(str(rni), "9; 12-15\n10; 10\n")
        self.assertEqual(len(rn1), 13)
        self.assertEqual(str(rn1), "8-9; 12-15\n10; 9-13\n")
        self.assertEqual(len(rn2), 5)
        self.assertEqual(str(rn2), "9; 12-15\n10; 10\n")
        # test __and__
        rni2 = rn1 & rn2
        self.assertEqual(len(rni2), 5)
        self.assertEqual(str(rni2), "9; 12-15\n10; 10\n")
        self.assertEqual(len(rn1), 13)
        self.assertEqual(str(rn1), "8-9; 12-15\n10; 9-13\n")
        self.assertEqual(len(rn2), 5)
        self.assertEqual(str(rn2), "9; 12-15\n10; 10\n")
        self.assertEqual(rni, rni2) # btw test __eq__
        try:
            dummy = rn1 & "foobar"
            self.assertFalse("TypeError not raised")
        except TypeError:
            pass

    def test_intersection_update(self):
        rn1 = RangeSetND([["10", "10-13"], ["10", "9-12"]])
        self.assertEqual(len(rn1), 5)
        self.assertEqual(str(rn1), "10; 9-13\n")
        # self test:
        rn1.intersection_update(rn1)
        self.assertEqual(len(rn1), 5)
        self.assertEqual(str(rn1), "10; 9-13\n")
        #
        rn2 = RangeSetND([["10", "10"]])
        rn1.intersection_update(rn2)
        self.assertEqual(len(rn1), 1)
        self.assertEqual(str(rn1), "10; 10\n")

        rn1 = RangeSetND([["10", "10-13"], ["10", "9-12"], ["8-9", "12-15"]])
        rn2 = RangeSetND([["10", "10"], ["9", "12-15"]])
        rn1.intersection_update(rn2)
        self.assertEqual(len(rn1), 5)
        self.assertEqual(str(rn1), "9; 12-15\n10; 10\n")

        rn1 = RangeSetND([["10", "10-13"], ["10", "9-12"], ["8-9", "12-15"]])
        rn2 = RangeSetND([["10", "10"], ["9", "12-15"], ["10-12", "11-15"], ["11", "14"]])
        rn1.intersection_update(rn2)
        self.assertEqual(len(rn1), 8)
        self.assertEqual(str(rn1), "9; 12-15\n10; 10-13\n")

        rn1 = RangeSetND([["10", "10-13"], ["10", "9-12"], ["8-9", "12-15"], ["10", "10-13"], ["10", "12-16"], ["9", "13-16"]])
        rn2 = RangeSetND([["10", "10"], ["9", "12-15"], ["10-12", "11-15"], ["11", "14"]])
        rn1.intersection_update(rn2)
        self.assertEqual(len(rn1), 10)
        self.assertEqual(str(rn1), "10; 10-15\n9; 12-15\n")

        rn1 = RangeSetND([["10", "10-13"], ["10", "9-12"], ["8-9", "12-15"], ["10", "10-13"], ["10", "12-16"], ["9", "13-16"]])
        rn2 = RangeSetND([["10", "10"], ["9", "12-16"], ["10-12", "11-15"], ["11", "14"], ["8", "10-20"]])
        rn1.intersection_update(rn2)
        self.assertEqual(len(rn1), 15)
        # no pre-fold (self._veclist)
        self.assertEqual(str(rn1), "10; 10-15\n9; 12-16\n8; 12-15\n")
        # pre-fold (self.veclist)
        #self.assertEqual(str(rn1), "8-9; 12-15\n10; 10-15\n9; 16\n")

        # binary error
        if sys.version_info >= (2, 5, 0):
            rn1 = RangeSetND([["10", "10-13"], ["10", "9-12"]])
            rn2 = RangeSetND([["10", "10"]])
            rn1 &= rn2
            self.assertEqual(len(rn1), 1)
            self.assertEqual(str(rn1), "10; 10\n")
            rn2 = set([3, 5])
            self.assertRaises(TypeError, rn1.__iand__, rn2)

    def test_xor(self):
        rn1 = RangeSetND([["10", "10-13"], ["10", "9-12"]])
        rn2 = RangeSetND([["10", "10"]])
        rnx = rn1.symmetric_difference(rn2)
        self.assertEqual(len(rnx), 4)
        self.assertEqual(str(rnx), "10; 9,11-13\n")
        rnx2 = rn1 ^ rn2
        self.assertEqual(len(rnx2), 4)
        self.assertEqual(str(rnx2), "10; 9,11-13\n")
        self.assertEqual(rnx, rnx2)
        try:
            dummy = rn1 ^ "foobar"
            self.assertFalse("TypeError not raised")
        except TypeError:
            pass
        # binary error
        if sys.version_info >= (2, 5, 0):
            rn1 = RangeSetND([["10", "10-13"], ["10", "9-12"]])
            rn2 = RangeSetND([["10", "10"]])
            rn1 ^= rn2
            self.assertEqual(len(rnx), 4)
            self.assertEqual(str(rnx), "10; 9,11-13\n")
            rn2 = set([3, 5])
            self.assertRaises(TypeError, rn1.__ixor__, rn2)

    def test_getitem(self):
        rn1 = RangeSetND([["10", "10-13"], ["0-3", "1-2"]])
        self.assertEqual(len(rn1), 12)
        self.assertEqual(rn1[0], (0, 1))
        self.assertEqual(rn1[1], (0, 2))
        self.assertEqual(rn1[2], (1, 1))
        self.assertEqual(rn1[3], (1, 2))
        self.assertEqual(rn1[4], (2, 1))
        self.assertEqual(rn1[5], (2, 2))
        self.assertEqual(rn1[6], (3, 1))
        self.assertEqual(rn1[7], (3, 2))
        self.assertEqual(rn1[8], (10, 10))
        self.assertEqual(rn1[9], (10, 11))
        self.assertEqual(rn1[10], (10, 12))
        self.assertEqual(rn1[11], (10, 13))
        self.assertRaises(IndexError, rn1.__getitem__, 12)
        # negative indices
        self.assertEqual(rn1[-1], (10, 13))
        self.assertEqual(rn1[-2], (10, 12))
        self.assertEqual(rn1[-3], (10, 11))
        self.assertEqual(rn1[-4], (10, 10))
        self.assertEqual(rn1[-5], (3, 2))
        self.assertEqual(rn1[-12], (0, 1))
        self.assertRaises(IndexError, rn1.__getitem__, -13)
        self.assertRaises(TypeError, rn1.__getitem__, "foo")

    def test_getitem_slices(self):
        rn1 = RangeSetND([["10", "10-13"], ["0-3", "1-2"]])
        # slices
        self.assertEqual(str(rn1[0:2]), "0; 1-2\n")
        self.assertEqual(str(rn1[0:4]), "0-1; 1-2\n")
        self.assertEqual(str(rn1[0:5]), "0-1; 1-2\n2; 1\n")
        self.assertEqual(str(rn1[0:6]), "0-2; 1-2\n")
        self.assertEqual(str(rn1[0:7]), "0-2; 1-2\n3; 1\n")
        self.assertEqual(str(rn1[0:8]), "0-3; 1-2\n")
        self.assertEqual(str(rn1[0:9]), "0-3; 1-2\n10; 10\n")
        self.assertEqual(str(rn1[0:10]), "0-3; 1-2\n10; 10-11\n")
        self.assertEqual(str(rn1[0:11]), "0-3; 1-2\n10; 10-12\n")
        self.assertEqual(str(rn1[0:12]), "0-3; 1-2\n10; 10-13\n")
        self.assertEqual(str(rn1[0:13]), "0-3; 1-2\n10; 10-13\n")
        # steps
        self.assertEqual(str(rn1[0:12:2]), "0-3; 1\n10; 10,12\n")
        self.assertEqual(str(rn1[1:12:2]), "0-3; 2\n10; 11,13\n")

    def test_contiguous(self):
        rn0 = RangeSetND()
        self.assertEqual([], [str(ns) for ns in rn0.contiguous()])
        rn1 = RangeSetND([["10", "10-13,15"], ["0-3,5-6", "1-2"]])
        self.assertEqual(str(rn1), "0-3,5-6; 1-2\n10; 10-13,15\n")
        self.assertEqual(['0-3; 1-2\n', '5-6; 1-2\n', '10; 10-13\n', '10; 15\n'], [str(ns) for ns in rn1.contiguous()])
        self.assertEqual(str(rn1), "0-3,5-6; 1-2\n10; 10-13,15\n")

    def test_iter(self):
        rn0 = RangeSetND([['1-2', '3'], ['1-2', '4'], ['2-6', '6-9,11']])
        self.assertEqual(len([r for r in rn0]), len(rn0))
        self.assertEqual([(2, 6), (2, 7), (2, 8), (2, 9), (2, 11), (3, 6),
                          (3, 7), (3, 8), (3, 9), (3, 11), (4, 6), (4, 7),
                          (4, 8), (4, 9), (4, 11), (5, 6), (5, 7), (5, 8),
                          (5, 9), (5, 11), (6, 6), (6, 7), (6, 8), (6, 9),
                          (6, 11), (1, 3), (1, 4), (2, 3), (2, 4)],
                         [r for r in rn0])

    def test_pads(self):
        rn0 = RangeSetND()
        self.assertEqual(str(rn0), "")
        self.assertEqual(len(rn0), 0)
        self.assertEqual(rn0.pads(), ())
        rn1 = RangeSetND([['01-02', '003'], ['01-02', '004'], ['02-06', '006-009,411']])
        self.assertEqual(str(rn1), "02-06; 006-009,411\n01-02; 003-004\n")
        self.assertEqual(len(rn1), 29)
        self.assertEqual(rn1.pads(), (2, 3))

    def test_mutability_1(self):
        rs0 = RangeSet("2-5")
        rs1 = RangeSet("0-1")
        rn0 = RangeSetND([[rs0, rs1]]) #, copy_rangeset=False)
        self.assertEqual(str(rn0), "2-5; 0-1\n")

        rs2 = RangeSet("6-7")
        rs3 = RangeSet("2-3")
        rn1 = RangeSetND([[rs2, rs3]]) #, copy_rangeset=False)
        rn0.update(rn1)
        self.assertEqual(str(rn0), "2-5; 0-1\n6-7; 2-3\n")

        # check mutability safety
        self.assertEqual(str(rs0), "2-5")
        self.assertEqual(str(rs1), "0-1")
        self.assertEqual(str(rs2), "6-7")
        self.assertEqual(str(rs3), "2-3")

        # reverse check
        rs1.add(2)
        self.assertEqual(str(rs1), "0-2")
        rs3.add(4)
        self.assertEqual(str(rs3), "2-4")
        self.assertEqual(str(rn0), "2-5; 0-1\n6-7; 2-3\n")

        self.assertEqual(str(rn1), "6-7; 2-3\n")
        rn1.update([[rs2, rs3]])
        self.assertEqual(str(rn1), "6-7; 2-4\n")

        self.assertEqual(str(rn0), "2-5; 0-1\n6-7; 2-3\n")

    def test_mutability_2(self):
        rs0 = RangeSet("2-5")
        rs1 = RangeSet("0-1")
        rn0 = RangeSetND([[rs0, rs1]]) #, copy_rangeset=False)
        self.assertEqual(str(rn0), "2-5; 0-1\n")

        rs2 = RangeSet("6-7")
        rs3 = RangeSet("2-3")
        rn0.update([[rs2, rs3]])
        self.assertEqual(str(rn0), "2-5; 0-1\n6-7; 2-3\n")

        rs3.add(4)
        self.assertEqual(str(rs3), "2-4")
        self.assertEqual(str(rn0), "2-5; 0-1\n6-7; 2-3\n")
