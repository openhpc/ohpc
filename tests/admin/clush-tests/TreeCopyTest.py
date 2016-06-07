#!/usr/bin/env python
# ClusterShell.Worker.WorkerTree copy test

import logging
import unittest

from os.path import dirname, join

from ClusterShell.NodeSet import NodeSet
from ClusterShell.Task import task_self, task_cleanup
from ClusterShell.Topology import TopologyGraph
import ClusterShell.Task
from ClusterShell.Worker.Tree import WorkerTree

from TLib import HOSTNAME, make_temp_file

# live logging with nosetests --nologcapture
logging.basicConfig(level=logging.DEBUG)


class TestWorkerTree(WorkerTree):
    """Test class used to mock WorkerTree."""

    TEST_INST = None

    def _copy_remote(self, source, dest, targets, gateway, timeout):
        """run a remote copy in tree mode (using gateway)"""
        self.TEST_INST.assertEqual(source, self.TEST_INST.tfile.name)
        # check that dest is our tfile.name dirname
        self.TEST_INST.assertEqual(dest, dirname(self.TEST_INST.tfile.name))
        self.TEST_INST.assertEqual(targets, NodeSet("n60"))
        self.TEST_INST.test_ok = True

    def write(self, buf):
        """dummy for mocking"""

    def set_write_eof(self):
        """dummy for mocking"""


class TreeCopyTestTest(unittest.TestCase):
    """tree copy test class"""

    def setUp(self):
        """setup WorkerTree mock for each test"""
        # topology
        graph = TopologyGraph()
        graph.add_route(NodeSet(HOSTNAME), NodeSet('n[1-2]'))
        graph.add_route(NodeSet('n1'), NodeSet('n[10-49]'))
        graph.add_route(NodeSet('n2'), NodeSet('n[50-89]'))
        ClusterShell.Task.WorkerTree = TestWorkerTree
        TestWorkerTree.TEST_INST = self
        task = task_self()
        task.topology = graph.to_tree(HOSTNAME)

    def tearDown(self):
        """remove WorkerTree mock after each test"""
        task_cleanup()
        ClusterShell.Task.WorkerTree = WorkerTree

    def test_copy(self):
        """test file copy setup in tree mode (1 gateway)"""
        self.test_ok = False
        self.tfile = make_temp_file("dummy")
        # add leading '/' like clush so that WorkerTree knows it's a dir
        task_self().copy(self.tfile.name,
                         join(dirname(self.tfile.name), ''),
                         "n60")
        task_self().resume()
        self.assertTrue(self.test_ok)

