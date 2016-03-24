#!/usr/bin/env python
# ClusterShell task resource consumption/limits test suite
# Written by S. Thiell 2010-10-19


"""Unit test for ClusterShell Task (resource limits)"""

import resource
import subprocess
import sys
import unittest

sys.path.insert(0, '../lib')

from TLib import HOSTNAME
from ClusterShell.Task import *
from ClusterShell.Worker.Pdsh import WorkerPdsh


class TaskRLimitsTest(unittest.TestCase):

    def setUp(self):
        """set soft nofile resource limit to 100"""
        self.soft, self.hard = resource.getrlimit(resource.RLIMIT_NOFILE)
        resource.setrlimit(resource.RLIMIT_NOFILE, (100, self.hard))

    def tearDown(self):
        """restore original resource limits"""
        resource.setrlimit(resource.RLIMIT_NOFILE, (self.soft, self.hard))

    def _testPopen(self, stderr):
        task = task_self()
        self.assert_(task != None)
        task.set_info("fanout", 10)
        for i in xrange(2000):
            worker = task.shell("/bin/hostname", stderr=stderr)
            self.assert_(worker != None)
        # run task
        task.resume()

    def testPopen(self):
        """test resource usage with local task.shell(stderr=False)"""
        self._testPopen(False)

    def testPopenStderr(self):
        """test resource usage with local task.shell(stderr=True)"""
        self._testPopen(True)

    def _testRemote(self, stderr):
        task = task_self()
        self.assert_(task != None)
        task.set_info("fanout", 10)
        for i in xrange(400):
            worker = task.shell("/bin/hostname", nodes=HOSTNAME,
                                stderr=stderr)
            self.assert_(worker != None)
        # run task
        task.resume()

    def testRemote(self):
        """test resource usage with remote task.shell(stderr=False)"""
        self._testRemote(False)

    def testRemoteStderr(self):
        """test resource usage with remote task.shell(stderr=True)"""
        self._testRemote(True)

    def _testRemotePdsh(self, stderr):
        task = task_self()
        self.assert_(task != None)
        task.set_info("fanout", 10)
        for i in xrange(200):
            worker = WorkerPdsh(HOSTNAME, handler=None,
                                timeout=0,
                                command="/bin/hostname",
                                stderr=stderr)
            self.assert_(worker != None)
            task.schedule(worker)
        # run task
        task.resume()

    def testRemotePdsh(self):
        """test resource usage with WorkerPdsh(stderr=False)"""
        self._testRemotePdsh(False)

    def testRemotePdshStderr(self):
        """test resource usage with WorkerPdsh(stderr=True)"""
        self._testRemotePdsh(True)

if __name__ == '__main__':
    suite = unittest.TestLoader().loadTestsFromTestCase(TaskRLimitsTest)
    unittest.TextTestRunner(verbosity=2).run(suite)

