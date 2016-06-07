#!/usr/bin/env python
# ClusterShell test suite
# Written by S. Thiell 2010-02-03


"""Unit test for ClusterShell MsgTree Class"""

from operator import itemgetter
import sys
import unittest

sys.path.insert(0, '../lib')

from ClusterShell.MsgTree import *


class MsgTreeTest(unittest.TestCase):

    def test_001_basics(self):
        """test MsgTree basics"""
        tree = MsgTree()
        self.assertEqual(len(tree), 0)

        tree.add("key", "message")
        self.assertEqual(len(tree), 1)

        tree.add("key", "message2")
        self.assertEqual(len(tree), 1)

        tree.add("key2", "message3")
        self.assertEqual(len(tree), 2)

    def test_002_elem(self):
        """test MsgTreeElem"""
        elem = MsgTreeElem()
        self.assertEqual(len(elem), 0)
        for s in elem:
            self.fail("found line in empty MsgTreeElem!")

    def test_003_iterators(self):
        """test MsgTree iterators"""
        # build tree...
        tree = MsgTree()
        self.assertEqual(len(tree), 0)
        tree.add(("item1", "key"), "message0")
        self.assertEqual(len(tree), 1)
        tree.add(("item2", "key"), "message2")
        self.assertEqual(len(tree), 2)
        tree.add(("item3", "key"), "message3")
        self.assertEqual(len(tree), 3)
        tree.add(("item4", "key"), "message3")
        tree.add(("item2", "newkey"), "message4")
        self.assertEqual(len(tree), 5)
        self.assertEqual(tree._depth(), 1)

        # test standard iterator (over keys)
        cnt = 0
        what = set([ ("item1", "key"), ("item2", "key"), ("item3", "key"), \
                    ("item4", "key"), ("item2", "newkey") ])
        for key in tree:
            cnt += 1
            what.remove(key)
        self.assertEqual(cnt, 5)
        self.assertEqual(len(what), 0)

        # test keys() iterator
        cnt = 0
        for key in tree.keys(): # keep this test for return value check
            cnt += 1
        self.assertEqual(cnt, 5)
        self.assertEqual(len(list(iter(tree.keys()))), 5)

        # test messages() iterator (iterate over different messages)
        cnt = 0
        for msg in tree.messages():
            cnt += 1
            self.assertEqual(len(msg), len("message0"))
            self.assertEqual(msg[0][:-1], "message")
        self.assertEqual(cnt, 4)
        self.assertEqual(len(list(iter(tree.messages()))), 4)

        # test items() iterator (iterate over all key, msg pairs)
        cnt = 0
        for key, msg in tree.items():
            cnt += 1
        self.assertEqual(cnt, 5)
        self.assertEqual(len(list(iter(tree.items()))), 5)
            
        # test walk() iterator (iterate by msg and give the list of
        # associated keys)
        cnt = 0
        cnt_2 = 0
        for msg, keys in tree.walk():
            cnt += 1
            if len(keys) == 2:
                self.assertEqual(msg, "message3")
                cnt_2 += 1
        self.assertEqual(cnt, 4)
        self.assertEqual(cnt_2, 1)
        self.assertEqual(len(list(iter(tree.walk()))), 4)

        # test walk() with provided key-filter
        cnt = 0
        for msg, keys in tree.walk(match=lambda s: s[1] == "newkey"):
            cnt += 1
        self.assertEqual(cnt, 1)

        # test walk() with provided key-mapper
        cnt = 0
        cnt_2 = 0
        for msg, keys in tree.walk(mapper=itemgetter(0)):
            cnt += 1
            if len(keys) == 2:
                for k in keys:
                    self.assertEqual(type(k), str)
                cnt_2 += 1
        self.assertEqual(cnt, 4)
        self.assertEqual(cnt_2, 1)

        # test walk with full options: key-filter and key-mapper
        cnt = 0
        for msg, keys in tree.walk(match=lambda k: k[1] == "newkey",
                                       mapper=itemgetter(0)):
            cnt += 1
            self.assertEqual(msg, "message4")
            self.assertEqual(keys[0], "item2")
        self.assertEqual(cnt, 1)

        cnt = 0
        for msg, keys in tree.walk(match=lambda k: k[1] == "key",
                                       mapper=itemgetter(0)):
            cnt += 1
            self.assertEqual(keys[0][:-1], "item")
        self.assertEqual(cnt, 3) # 3 and not 4 because item3 and item4 are merged

    def test_004_getitem(self):
        """test MsgTree get and __getitem__"""
        # build tree...
        tree = MsgTree()
        tree.add("item1", "message0")
        self.assertEqual(len(tree), 1)
        tree.add("item2", "message2")
        tree.add("item3", "message2")
        tree.add("item4", "message3")
        tree.add("item2", "message4")
        tree.add("item3", "message4")
        self.assertEqual(len(tree), 4)
        self.assertEqual(tree["item1"], "message0")
        self.assertEqual(tree.get("item1"), "message0")
        self.assertEqual(tree["item2"], "message2\nmessage4")
        self.assertEqual(tree.get("item2"), "message2\nmessage4")
        self.assertEqual(tree.get("item5", "default_buf"), "default_buf")
        self.assertEqual(tree._depth(), 2)

    def test_005_remove(self):
        """test MsgTree.remove()"""
        # build tree
        tree = MsgTree()
        self.assertEqual(len(tree), 0)
        tree.add(("w1", "key1"), "message0")
        self.assertEqual(len(tree), 1)
        tree.add(("w1", "key2"), "message0")
        self.assertEqual(len(tree), 2)
        tree.add(("w1", "key3"), "message0")
        self.assertEqual(len(tree), 3)
        tree.add(("w2", "key4"), "message1")
        self.assertEqual(len(tree), 4)
        tree.remove(lambda k: k[1] == "key2")
        self.assertEqual(len(tree), 3)
        for msg, keys in tree.walk(match=lambda k: k[0] == "w1",
                                   mapper=itemgetter(1)):
            self.assertEqual(msg, "message0")
            self.assertEqual(len(keys), 2)
        tree.remove(lambda k: k[0] == "w1")
        self.assertEqual(len(tree), 1)
        tree.remove(lambda k: k[0] == "w2")
        self.assertEqual(len(tree), 0)
        tree.clear()
        self.assertEqual(len(tree), 0)

    def test_006_scalability(self):
        """test MsgTree scalability"""
        # build tree...
        tree = MsgTree()
        for i in xrange(0, 10000):
            tree.add("node%d" % i, "message%d" % i)
        self.assertEqual(len(tree), 10000)
        cnt = 0
        for msg, keys in tree.walk():
            cnt += 1

    def test_007_shift_mode(self):
        """test MsgTree in shift mode"""
        tree = MsgTree(mode=MODE_SHIFT)
        tree.add("item1", "message0")
        self.assertEqual(len(tree), 1)
        tree.add("item2", "message2")
        tree.add("item3", "message2")
        tree.add("item4", "message3")
        tree.add("item2", "message4")
        tree.add("item3", "message4")
        self.assertEqual(len(tree), 4)
        self.assertEqual(tree["item1"], "message0")
        self.assertEqual(tree.get("item1"), "message0")
        self.assertEqual(tree["item2"], "message2\nmessage4")
        self.assertEqual(tree.get("item2"), "message2\nmessage4")
        self.assertEqual(tree.get("item5", "default_buf"), "default_buf")
        self.assertEqual(tree._depth(), 2)
        self.assertEqual(len(list(tree.walk())), 3)

    def test_008_trace_mode(self):
        """test MsgTree in trace mode"""
        tree = MsgTree(mode=MODE_TRACE)
        tree.add("item1", "message0")
        self.assertEqual(len(tree), 1)
        tree.add("item2", "message2")
        tree.add("item3", "message2")
        tree.add("item4", "message3")
        tree.add("item2", "message4")
        tree.add("item3", "message4")
        self.assertEqual(len(tree), 4)
        self.assertEqual(tree["item1"], "message0")
        self.assertEqual(tree.get("item1"), "message0")
        self.assertEqual(tree["item2"], "message2\nmessage4")
        self.assertEqual(tree.get("item2"), "message2\nmessage4")
        self.assertEqual(tree.get("item5", "default_buf"), "default_buf")
        self.assertEqual(tree._depth(), 2)
        self.assertEqual(len(list(tree.walk())), 4)
        self.assertEqual(list(tree.walk_trace()), \
            [('message0', ['item1'], 1, 0),
             ('message2', ['item2', 'item3'], 1, 1),
             ('message4', ['item2', 'item3'], 2, 0),
             ('message3', ['item4'], 1, 0)])

    def test_009_defer_to_shift_mode(self):
        """test MsgTree defer to shift mode"""
        tree = MsgTree(mode=MODE_DEFER)
        tree.add("item1", "message0")
        self.assertEqual(len(tree), 1)
        tree.add("item2", "message1")
        self.assertEqual(len(tree), 2)
        tree.add("item3", "message2")
        self.assertEqual(len(tree), 3)
        tree.add("item2", "message4")
        tree.add("item1", "message3")
        self.assertEqual(tree["item1"], "message0\nmessage3")
        self.assertEqual(tree.mode, MODE_DEFER)
        # calling walk with call _update_keys() and change to MODE_SHIFT
        self.assertEqual([(k, e.message()) for e, k in tree.walk()],
                         [(['item1'], 'message0\nmessage3'),
                          (['item2'], 'message1\nmessage4'),
                          (['item3'], 'message2')])
        self.assertEqual(tree.mode, MODE_SHIFT)
        # further tree modifications should be safe...
        tree.add("item1", "message5")
        tree.add("item2", "message6")
        self.assertEqual(tree["item1"], "message0\nmessage3\nmessage5")
        self.assertEqual([(k, e.message()) for e, k in tree.walk()],
                         [(['item1'], 'message0\nmessage3\nmessage5'),
                          (['item2'], 'message1\nmessage4\nmessage6'),
                          (['item3'], 'message2')])

    def test_010_remove_in_defer_mode(self):
        """test MsgTree remove in defer mode"""
        tree = MsgTree(mode=MODE_DEFER)
        tree.add("item1", "message0")
        self.assertEqual(len(tree), 1)
        tree.add("item2", "message1")
        self.assertEqual(len(tree), 2)
        tree.add("item3", "message2")
        self.assertEqual(len(tree), 3)
        tree.add("item2", "message4")
        tree.add("item1", "message3")
        tree.remove(lambda k: k == "item2")
        self.assertEqual(tree["item1"], "message0\nmessage3")
        self.assertRaises(KeyError, tree.__getitem__, "item2")
        # calling walk with call _update_keys() and change to MODE_SHIFT
        self.assertEqual([(k, e.message()) for e, k in tree.walk()],
                         [(['item1'], 'message0\nmessage3'),
                          (['item3'], 'message2')])
        self.assertEqual(tree.mode, MODE_SHIFT)
        # further tree modifications should be safe...
        tree.add("item1", "message5")
        tree.add("item2", "message6")
        self.assertEqual(tree["item1"], "message0\nmessage3\nmessage5")
        self.assertEqual(tree["item2"], "message6")
        self.assertEqual([(k, e.message()) for e, k in tree.walk()],
                         [(['item1'], 'message0\nmessage3\nmessage5'),
                          (['item3'], 'message2'),
                          (['item2'], 'message6')])

