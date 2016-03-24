#!/usr/bin/env python
# ClusterShell.Task tree test suite

import logging
import os
import unittest

from ClusterShell.Task import task_self
from ClusterShell.Topology import TopologyError

from TLib import HOSTNAME, make_temp_file

# live logging with nosetests --nologcapture
logging.basicConfig(level=logging.DEBUG)


class TreeTaskTest(unittest.TestCase):
    """Test cases for Tree-related Task methods"""

    def tearDown(self):
        """clear task topology"""
        task_self().topology = None

    def test_shell_auto_tree_dummy(self):
        """test task shell auto tree"""
        # initialize a dummy topology.conf file
        topofile = make_temp_file(
            '[Main]\n%s: dummy-gw\ndummy-gw: dummy-node\n' % HOSTNAME)
        task = task_self()
        task.set_default("auto_tree", True)
        task.TOPOLOGY_CONFIGS = [topofile.name]
        task.run("/bin/hostname", nodes="dummy-node", stderr=True)
        # FIXME gateway errors are not yet being handled correctly
        self.assertEqual(task.max_retcode(), 255)
        # XXX correct results would be:
        #self.assertEqual(task.max_retcode(), None)
        #expected = "Name or service not known"
        #if not task.node_error("dummy-node").endswith(expected):
        #    self.assertEqual(task.node_error("dummy-node"), expected)

    def test_shell_auto_tree_noconf(self):
        """test task shell auto tree [no topology.conf]"""
        task = task_self()
        task.set_default("auto_tree", True)
        dummyfile = "/some/dummy/path/topo.conf"
        self.assertFalse(os.path.exists(dummyfile))
        task.TOPOLOGY_CONFIGS = [dummyfile]
        # do not raise exception
        task.run("/bin/hostname", nodes="dummy-node")

    def test_shell_auto_tree_error(self):
        """test task shell auto tree [TopologyError]"""
        # initialize an erroneous topology.conf file
        topofile = make_temp_file(
            '[Main]\n%s: dummy-gw\ndummy-gw: dummy-gw\n' % HOSTNAME)
        task = task_self()
        task.set_default("auto_tree", True)
        task.TOPOLOGY_CONFIGS = [topofile.name]
        self.assertRaises(TopologyError, task.run, "/bin/hostname",
                          nodes="dummy-node")
