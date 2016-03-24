#!/usr/bin/env python
# ClusterShell.Defaults test suite
# Written by S. Thiell


"""Unit test for ClusterShell Defaults module"""

from textwrap import dedent
import sys
import unittest

sys.path.insert(0, '../lib')

from TLib import make_temp_file

from ClusterShell.Defaults import Defaults, _task_print_debug

from ClusterShell.Task import task_self, task_terminate
from ClusterShell.Worker.Pdsh import WorkerPdsh
from ClusterShell.Worker.Ssh import WorkerSsh


class Defaults000NoConfigTest(unittest.TestCase):

    def setUp(self):
        """setup test - initialize Defaults instance"""
        self.defaults = Defaults([])

    def test_000_initial(self):
        """test Defaults initial values"""
        # task_default
        self.assertFalse(self.defaults.stderr)
        self.assertTrue(self.defaults.stdout_msgtree)
        self.assertTrue(self.defaults.stderr_msgtree)
        self.assertEqual(self.defaults.engine, 'auto')
        self.assertEqual(self.defaults.port_qlimit, 100)
        self.assertTrue(self.defaults.auto_tree)
        self.assertEqual(self.defaults.local_workername, 'exec')
        self.assertEqual(self.defaults.distant_workername, 'ssh')
        # task_info
        self.assertFalse(self.defaults.debug)
        self.assertEqual(self.defaults.print_debug, _task_print_debug)
        self.assertFalse(self.defaults.print_debug is None)
        self.assertEqual(self.defaults.fanout, 64)
        self.assertEqual(self.defaults.grooming_delay, 0.25)
        self.assertEqual(self.defaults.connect_timeout, 10)
        self.assertEqual(self.defaults.command_timeout, 0)

    def test_001_setattr(self):
        """test Defaults setattr"""
        # task_default
        self.defaults.stderr = True
        self.assertTrue(self.defaults.stderr)
        self.defaults.stdout_msgtree = False
        self.assertFalse(self.defaults.stdout_msgtree)
        self.defaults.stderr_msgtree = False
        self.assertFalse(self.defaults.stderr_msgtree)
        self.defaults.engine = 'select'
        self.assertEqual(self.defaults.engine, 'select')
        self.defaults.port_qlimit = 1000
        self.assertEqual(self.defaults.port_qlimit, 1000)
        self.defaults.auto_tree = False
        self.assertFalse(self.defaults.auto_tree)
        self.defaults.local_workername = 'none'
        self.assertEqual(self.defaults.local_workername, 'none')
        self.defaults.distant_workername = 'pdsh'
        self.assertEqual(self.defaults.distant_workername, 'pdsh')
        # task_info
        self.defaults.debug = True
        self.assertTrue(self.defaults.debug)
        self.defaults.print_debug = None
        self.assertEqual(self.defaults.print_debug, None)
        self.defaults.fanout = 256
        self.assertEqual(self.defaults.fanout, 256)
        self.defaults.grooming_delay = 0.5
        self.assertEqual(self.defaults.grooming_delay, 0.5)
        self.defaults.connect_timeout = 12.5
        self.assertEqual(self.defaults.connect_timeout, 12.5)
        self.defaults.connect_timeout = 30.5

    def test_002_reinit_defaults(self):
        """Test Defaults manual reinit"""
        # For testing purposes only
        self.defaults.__init__(filenames=[])
        self.test_000_initial()

    def test_004_workerclass(self):
        """test Defaults workerclass"""
        self.defaults.distant_workername = 'pdsh'
        task_terminate()
        task = task_self(self.defaults)
        self.assertTrue(task.default("distant_worker") is WorkerPdsh)
        self.defaults.distant_workername = 'ssh'
        self.assertTrue(task.default("distant_worker") is WorkerPdsh)
        task_terminate()

        task = task_self(self.defaults)
        self.assertTrue(task.default("distant_worker") is WorkerSsh)
        task_terminate()

    def test_005_misc_value_errors(self):
        """test Defaults misc value errors"""
        task_terminate()
        self.defaults.local_workername = 'dummy1'
        self.assertRaises(ImportError, task_self, self.defaults)
        self.defaults.local_workername = 'exec'
        self.defaults.distant_workername = 'dummy2'
        self.assertRaises(ImportError, task_self, self.defaults)
        self.defaults.distant_workername = 'ssh'
        self.defaults.engine = 'unknown'
        self.assertRaises(KeyError, task_self, self.defaults)
        self.defaults.engine = 'auto'
        task = task_self(self.defaults)
        self.assertEqual(task.default('engine'), 'auto')
        task_terminate()


class Defaults001ConfigTest(unittest.TestCase):

    def setUp(self):
        self.defaults = None

    def _assert_default_values(self):
        # task_default
        self.assertFalse(self.defaults.stderr)
        self.assertTrue(self.defaults.stdout_msgtree)
        self.assertTrue(self.defaults.stderr_msgtree)
        self.assertEqual(self.defaults.engine, 'auto')
        self.assertEqual(self.defaults.port_qlimit, 100)
        self.assertTrue(self.defaults.auto_tree)
        self.assertEqual(self.defaults.local_workername, 'exec')
        self.assertEqual(self.defaults.distant_workername, 'ssh')
        # task_info
        self.assertFalse(self.defaults.debug)
        self.assertEqual(self.defaults.print_debug, _task_print_debug)
        self.assertFalse(self.defaults.print_debug is None)
        self.assertEqual(self.defaults.fanout, 64)
        self.assertEqual(self.defaults.grooming_delay, 0.25)
        self.assertEqual(self.defaults.connect_timeout, 10)
        self.assertEqual(self.defaults.command_timeout, 0)

    def test_000_empty(self):
        """test Defaults config file (empty)"""
        conf_test = make_temp_file('')
        self.defaults = Defaults(filenames=[conf_test.name])
        self._assert_default_values()

    def test_001_defaults(self):
        """test Defaults config file (defaults)"""
        conf_test = make_temp_file(dedent("""
            [task.default]
            stderr: false
            stdout_msgtree: true
            stderr_msgtree: true
            engine: auto
            port_qlimit: 100
            auto_tree: true
            local_workername: exec
            distant_workername: ssh

            [task.info]
            debug: false
            fanout: 64
            grooming_delay: 0.25
            connect_timeout: 10
            command_timeout: 0"""))
        self.defaults = Defaults(filenames=[conf_test.name])
        self._assert_default_values()

    def test_002_changed(self):
        """test Defaults config file (changed)"""
        conf_test = make_temp_file(dedent("""
            [task.default]
            stderr: true
            stdout_msgtree: false
            stderr_msgtree: false
            engine: select
            port_qlimit: 1000
            auto_tree: false
            local_workername: none
            distant_workername: pdsh

            [task.info]
            debug: true
            fanout: 256
            grooming_delay: 0.5
            connect_timeout: 12.5
            command_timeout: 30.5"""))
        self.defaults = Defaults(filenames=[conf_test.name])
        self.assertTrue(self.defaults.stderr)
        self.assertFalse(self.defaults.stdout_msgtree)
        self.assertFalse(self.defaults.stderr_msgtree)
        self.assertEqual(self.defaults.engine, 'select')
        self.assertEqual(self.defaults.port_qlimit, 1000)
        self.assertFalse(self.defaults.auto_tree)
        self.assertEqual(self.defaults.local_workername, 'none')
        self.assertEqual(self.defaults.distant_workername, 'pdsh')
        # task_info
        self.assertTrue(self.defaults.debug)
        self.assertEqual(self.defaults.fanout, 256)
        self.assertEqual(self.defaults.grooming_delay, 0.5)
        self.assertEqual(self.defaults.connect_timeout, 12.5)
