#!/usr/bin/env python
# ClusterShell.Topology test suite
# Written by H. Doreau


"""Unit test for Topology"""

import copy
import sys
import time
import unittest
import tempfile

# profiling imports
#import cProfile
#from guppy import hpy
# ---

sys.path.insert(0, '../lib')

from ClusterShell.Topology import *
from ClusterShell.NodeSet import NodeSet


def chrono(func):
    def timing(*args):
        start = time.time()
        res = func(*args)
        print "execution time: %f s" % (time.time() - start)
        return res
    return timing


class TopologyTest(unittest.TestCase):

    def testInvalidConfigurationFile(self):
        """test detecting invalid configuration file"""
        parser = TopologyParser()
        self.assertRaises(TopologyError,
                          parser.load,
                          '/invalid/path/for/testing')
        self.assertRaises(TopologyError,
                          TopologyParser,
                          '/invalid/path/for/testing')

    def testTopologyGraphGeneration(self):
        """test graph generation"""
        g = TopologyGraph()
        ns1 = NodeSet('nodes[0-5]')
        ns2 = NodeSet('nodes[6-10]')
        g.add_route(ns1, ns2)
        self.assertEqual(g.dest(ns1), ns2)

    def testAddingSeveralRoutes(self):
        """test adding several valid routes"""
        g = TopologyGraph()
        admin = NodeSet('admin')
        ns0 = NodeSet('nodes[0-9]')
        ns1 = NodeSet('nodes[10-19]')
        g.add_route(admin, ns0)
        g.add_route(ns0, ns1)
        # Connect a new dst nodeset to an existing src
        ns2 = NodeSet('nodes[20-29]')
        g.add_route(ns0, ns2)
        # Add the same dst nodeset twice (no error)
        g.add_route(ns0, ns2)

        self.assertEquals(g.dest(admin), ns0)
        self.assertEquals(g.dest(ns0), ns1 | ns2)

    def testBadLink(self):
        """test detecting bad links in graph"""
        g = TopologyGraph()
        admin = NodeSet('admin')
        ns0 = NodeSet('nodes[0-9]')
        ns1 = NodeSet('nodes[10-19]')
        g.add_route(admin, ns0)
        g.add_route(ns0, ns1)
        # Add a known src nodeset as a dst nodeset (error!)
        self.assertRaises(TopologyError, g.add_route, ns1, ns0)

    def testOverlappingRoutes(self):
        """test overlapping routes detection"""
        g = TopologyGraph()
        admin = NodeSet('admin')
        # Add the same nodeset twice
        ns0 = NodeSet('nodes[0-9]')
        ns1 = NodeSet('nodes[10-19]')
        ns1_overlap = NodeSet('nodes[5-29]')

        self.assertRaises(TopologyError, g.add_route, ns0, ns0)
        g.add_route(ns0, ns1)
        self.assertRaises(TopologyError, g.add_route, ns0, ns1_overlap)

    def testBadTopologies(self):
        """test detecting invalid topologies"""
        g = TopologyGraph()
        admin = NodeSet('admin')
        # Add the same nodeset twice
        ns0 = NodeSet('nodes[0-9]')
        ns1 = NodeSet('nodes[10-19]')
        ns2 = NodeSet('nodes[20-29]')

        g.add_route(admin, ns0)
        g.add_route(ns0, ns1)
        g.add_route(ns0, ns2)

        # add a superset of a known destination as source
        ns2_sup = NodeSet('somenode[0-10]')
        ns2_sup.add(ns2)
        self.assertRaises(TopologyError, g.add_route, ns2_sup, NodeSet('foo1'))

        # Add a known dst nodeset as a src nodeset
        ns3 = NodeSet('nodes[30-39]')
        g.add_route(ns1, ns3)

        # Add a subset of a known src nodeset as src
        ns0_sub = NodeSet(','.join(ns0[:3:]))
        ns4 = NodeSet('nodes[40-49]')
        g.add_route(ns0_sub, ns4)

        # Add a subset of a known dst nodeset as src
        ns1_sub = NodeSet(','.join(ns1[:3:]))
        self.assertRaises(TopologyError, g.add_route, ns4, ns1_sub)
        # Add a subset of a known src nodeset as dst
        self.assertRaises(TopologyError, g.add_route, ns4, ns0_sub)
        # Add a subset of a known dst nodeset as dst
        self.assertRaises(TopologyError, g.add_route, ns4, ns1_sub)
        # src <- subset of -> dst
        ns5 = NodeSet('nodes[50-59]')
        ns5_sub = NodeSet(','.join(ns5[:3:]))
        self.assertRaises(TopologyError, g.add_route, ns5, ns5_sub)
        self.assertRaises(TopologyError, g.add_route, ns5_sub, ns5)

        self.assertEqual(g.dest(ns0), (ns1 | ns2))
        self.assertEqual(g.dest(ns1), ns3)
        self.assertEqual(g.dest(ns2), None)
        self.assertEqual(g.dest(ns3), None)
        self.assertEqual(g.dest(ns4), None)
        self.assertEqual(g.dest(ns5), None)
        self.assertEqual(g.dest(ns0_sub), (ns1 | ns2 | ns4))

        g = TopologyGraph()
        root = NodeSet('root')
        ns01 = NodeSet('nodes[0-1]')
        ns23 = NodeSet('nodes[2-3]')
        ns45 = NodeSet('nodes[4-5]')
        ns67 = NodeSet('nodes[6-7]')
        ns89 = NodeSet('nodes[8-9]')

        g.add_route(root, ns01)
        g.add_route(root, ns23 | ns45)
        self.assertRaises(TopologyError, g.add_route, ns23, ns23)
        self.assertRaises(TopologyError, g.add_route, ns45, root)
        g.add_route(ns23, ns67)
        g.add_route(ns67, ns89)
        self.assertRaises(TopologyError, g.add_route, ns89, ns67)
        self.assertRaises(TopologyError, g.add_route, ns89, ns89)
        self.assertRaises(TopologyError, g.add_route, ns89, ns23)

        ns_all = NodeSet('root,nodes[0-9]')
        for nodegroup in g.to_tree('root'):
            ns_all.difference_update(nodegroup.nodeset)
        self.assertEqual(len(ns_all), 0)

    def testInvalidRootNode(self):
        """test invalid root node specification"""
        g = TopologyGraph()
        ns0 = NodeSet('node[0-9]')
        ns1 = NodeSet('node[10-19]')
        g.add_route(ns0, ns1)
        self.assertRaises(TopologyError, g.to_tree, 'admin1')

    def testMultipleAdminGroups(self):
        """test topology with several admin groups"""
        ## -------------------
        # TODO : uncommenting following lines should not produce an error. This
        # is a valid topology!!
        # ----------
        tmpfile = tempfile.NamedTemporaryFile()
        tmpfile.write('[routes]\n')
        tmpfile.write('admin0: nodes[0-1]\n')
        #tmpfile.write('admin1: nodes[0-1]\n')
        tmpfile.write('admin2: nodes[2-3]\n')
        #tmpfile.write('admin3: nodes[2-3]\n')
        tmpfile.write('nodes[0-1]: nodes[10-19]\n')
        tmpfile.write('nodes[2-3]: nodes[20-29]\n')
        tmpfile.flush()
        parser = TopologyParser(tmpfile.name)

        ns_all = NodeSet('admin2,nodes[2-3,20-29]')
        ns_tree = NodeSet()
        for nodegroup in parser.tree('admin2'):
           ns_tree.add(nodegroup.nodeset)
        self.assertEqual(str(ns_all), str(ns_tree))

    def testTopologyGraphBigGroups(self):
        """test adding huge nodegroups in routes"""
        g = TopologyGraph()
        ns0 = NodeSet('nodes[0-10000]')
        ns1 = NodeSet('nodes[12000-23000]')
        g.add_route(ns0, ns1)
        self.assertEqual(g.dest(ns0), ns1)

        ns2 = NodeSet('nodes[30000-35000]')
        ns3 = NodeSet('nodes[35001-45000]')
        g.add_route(ns2, ns3)
        self.assertEqual(g.dest(ns2), ns3)

    def testNodeString(self):
        """test loading a linear string topology"""
        tmpfile = tempfile.NamedTemporaryFile()
        tmpfile.write('[routes]\n')

        # TODO : increase the size
        ns = NodeSet('node[0-10]')

        prev = 'admin'
        for n in ns:
            tmpfile.write('%s: %s\n' % (prev, str(n)))
            prev = n
        tmpfile.flush()
        parser = TopologyParser(tmpfile.name)

        tree = parser.tree('admin')

        ns.add('admin')
        ns_tree = NodeSet()
        for nodegroup in tree:
            ns_tree.add(nodegroup.nodeset)
        self.assertEquals(ns, ns_tree)

    def testConfigurationParser(self):
        """test configuration parsing"""
        tmpfile = tempfile.NamedTemporaryFile()
        tmpfile.write('# this is a comment\n')
        tmpfile.write('[routes]\n')
        tmpfile.write('admin: nodes[0-1]\n')
        tmpfile.write('nodes[0-1]: nodes[2-5]\n')
        tmpfile.write('nodes[4-5]: nodes[6-9]\n')
        tmpfile.flush()
        parser = TopologyParser(tmpfile.name)

        parser.tree('admin')
        ns_all = NodeSet('admin,nodes[0-9]')
        ns_tree = NodeSet()
        for nodegroup in parser.tree('admin'):
           ns_tree.add(nodegroup.nodeset)
        self.assertEqual(str(ns_all), str(ns_tree))

    def testConfigurationParserCompatMain(self):
        """test configuration parsing (Main section compat)"""
        tmpfile = tempfile.NamedTemporaryFile()
        tmpfile.write('# this is a comment\n')
        tmpfile.write('[Main]\n')
        tmpfile.write('admin: nodes[0-1]\n')
        tmpfile.write('nodes[0-1]: nodes[2-5]\n')
        tmpfile.write('nodes[4-5]: nodes[6-9]\n')
        tmpfile.flush()
        parser = TopologyParser(tmpfile.name)

        parser.tree('admin')
        ns_all = NodeSet('admin,nodes[0-9]')
        ns_tree = NodeSet()
        for nodegroup in parser.tree('admin'):
           ns_tree.add(nodegroup.nodeset)
        self.assertEqual(str(ns_all), str(ns_tree))

    def testConfigurationShortSyntax(self):
        """test short topology specification syntax"""
        tmpfile = tempfile.NamedTemporaryFile()
        tmpfile.write('# this is a comment\n')
        tmpfile.write('[routes]\n')
        tmpfile.write('admin: nodes[0-9]\n')
        tmpfile.write('nodes[0-3,5]: nodes[10-19]\n')
        tmpfile.write('nodes[4,6-9]: nodes[30-39]\n')
        tmpfile.flush()
        parser = TopologyParser()
        parser.load(tmpfile.name)

        ns_all = NodeSet('admin,nodes[0-19,30-39]')
        ns_tree = NodeSet()
        for nodegroup in parser.tree('admin'):
           ns_tree.add(nodegroup.nodeset)
        self.assertEqual(str(ns_all), str(ns_tree))

    def testConfigurationLongSyntax(self):
        """test detailed topology description syntax"""
        tmpfile = tempfile.NamedTemporaryFile()
        tmpfile.write('# this is a comment\n')
        tmpfile.write('[routes]\n')
        tmpfile.write('admin: proxy\n')
        tmpfile.write('proxy: STA[0-1]\n')
        tmpfile.write('STA0: STB[0-1]\n')
        tmpfile.write('STB0: nodes[0-2]\n')
        tmpfile.write('STB1: nodes[3-5]\n')
        tmpfile.write('STA1: STB[2-3]\n')
        tmpfile.write('STB2: nodes[6-7]\n')
        tmpfile.write('STB3: nodes[8-10]\n')

        tmpfile.flush()
        parser = TopologyParser()
        parser.load(tmpfile.name)

        ns_all = NodeSet('admin,proxy,STA[0-1],STB[0-3],nodes[0-10]')
        ns_tree = NodeSet()
        for nodegroup in parser.tree('admin'):
           ns_tree.add(nodegroup.nodeset)
        self.assertEqual(str(ns_all), str(ns_tree))

    def testConfigurationParserDeepTree(self):
        """test a configuration that generates a deep tree"""
        tmpfile = tempfile.NamedTemporaryFile()
        tmpfile.write('# this is a comment\n')
        tmpfile.write('[routes]\n')
        tmpfile.write('admin: nodes[0-9]\n')

        levels = 15 # how deep do you want the tree to be?
        for i in xrange(0, levels*10, 10):
            line = 'nodes[%d-%d]: nodes[%d-%d]\n' % (i, i+9, i+10, i+19)
            tmpfile.write(line)
        tmpfile.flush()
        parser = TopologyParser()
        parser.load(tmpfile.name)

        ns_all = NodeSet('admin,nodes[0-159]')
        ns_tree = NodeSet()
        for nodegroup in parser.tree('admin'):
           ns_tree.add(nodegroup.nodeset)
        self.assertEqual(str(ns_all), str(ns_tree))

    def testConfigurationParserBigTree(self):
        """test configuration parser against big propagation tree"""
        tmpfile = tempfile.NamedTemporaryFile()
        tmpfile.write('# this is a comment\n')
        tmpfile.write('[routes]\n')
        tmpfile.write('admin: ST[0-4]\n')
        tmpfile.write('ST[0-4]: STA[0-49]\n')
        tmpfile.write('STA[0-49]: nodes[0-10000]\n')
        tmpfile.flush()
        parser = TopologyParser()
        parser.load(tmpfile.name)

        ns_all = NodeSet('admin,ST[0-4],STA[0-49],nodes[0-10000]')
        ns_tree = NodeSet()
        for nodegroup in parser.tree('admin'):
           ns_tree.add(nodegroup.nodeset)
        self.assertEqual(str(ns_all), str(ns_tree))

    def testConfigurationParserConvergentPaths(self):
        """convergent paths detection"""
        tmpfile = tempfile.NamedTemporaryFile()
        tmpfile.write('# this is a comment\n')
        tmpfile.write('[routes]\n')
        tmpfile.write('fortoy32: fortoy[33-34]\n')
        tmpfile.write('fortoy33: fortoy35\n')
        tmpfile.write('fortoy34: fortoy36\n')
        tmpfile.write('fortoy[35-36]: fortoy37\n')

        tmpfile.flush()
        parser = TopologyParser()
        self.assertRaises(TopologyError, parser.load, tmpfile.name)

    def testPrintingTree(self):
        """test printing tree"""
        tmpfile = tempfile.NamedTemporaryFile()
        tmpfile.write('[routes]\n')
        tmpfile.write('n0: n[1-2]\n')
        tmpfile.write('n1: n[10-49]\n')
        tmpfile.write('n2: n[50-89]\n')

        tmpfile.flush()
        parser = TopologyParser()
        parser.load(tmpfile.name)

        tree = parser.tree('n0')

        # In fact it looks like this:
        # ---------------------------
        # n0
        # |_ n1
        # |  |_ n[10-49]
        # |_ n2
        #    |_ n[50-89]
        # ---------------------------
        display_ref = 'n0\n|- n1\n|  `- n[10-49]\n`- n2\n   `- n[50-89]\n'
        display = str(tree)
        print "\n%s" % display
        self.assertEquals(display, display_ref)

        self.assertEquals(str(TopologyTree()), '<TopologyTree instance (empty)>')

    def testAddingInvalidChildren(self):
        """test detecting invalid children"""
        t0 = TopologyNodeGroup(NodeSet('node[0-9]'))
        self.assertRaises(AssertionError, t0.add_child, 'foobar')
        t1 = TopologyNodeGroup(NodeSet('node[10-19]'))

        t0.add_child(t1)
        self.assertEquals(t0.children_ns(), t1.nodeset)
        t0.add_child(t1)
        self.assertEquals(t0.children_ns(), t1.nodeset)

    def testRemovingChild(self):
        """test child removal operation"""
        t0 = TopologyNodeGroup(NodeSet('node[0-9]'))
        t1 = TopologyNodeGroup(NodeSet('node[10-19]'))

        t0.add_child(t1)
        self.assertEquals(t0.children_ns(), t1.nodeset)
        t0.clear_child(t1)
        self.assertEquals(t0.children_ns(), None)

        t0.clear_child(t1) # error discarded
        self.assertRaises(ValueError, t0.clear_child, t1, strict=True)

        t2 = TopologyNodeGroup(NodeSet('node[20-29]'))
        t0.add_child(t1)
        t0.add_child(t2)
        self.assertEquals(t0.children_ns(), t1.nodeset | t2.nodeset)
        t0.clear_children()
        self.assertEquals(t0.children_ns(), None)
        self.assertEquals(t0.children_len(), 0)

    def testStrConversions(self):
        """test str() casts"""
        t = TopologyNodeGroup(NodeSet('admin0'))
        self.assertEquals(str(t), '<TopologyNodeGroup (admin0)>')

        t = TopologyRoutingTable()
        r0 = TopologyRoute(NodeSet('src[0-9]'), NodeSet('dst[5-8]'))
        r1 = TopologyRoute(NodeSet('src[10-19]'), NodeSet('dst[15-18]'))

        self.assertEquals(str(r0), 'src[0-9] -> dst[5-8]')

        t.add_route(r0)
        t.add_route(r1)
        self.assertEquals(str(t), 'src[0-9] -> dst[5-8]\nsrc[10-19] -> dst[15-18]')

        g = TopologyGraph()
        # XXX: Actually if g is not empty other things will be printed out...
        self.assertEquals(str(g), '<TopologyGraph>\n')

