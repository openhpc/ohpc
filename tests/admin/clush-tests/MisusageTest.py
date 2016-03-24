#!/usr/bin/env python
# ClusterShell test suite
# Written by S. Thiell 2010-02-19


"""Unit test for ClusterShell common library misusages"""

import sys
import unittest

sys.path.insert(0, '../lib')

from TLib import HOSTNAME
from ClusterShell.Event import EventHandler
from ClusterShell.Worker.Popen import WorkerPopen
from ClusterShell.Worker.Ssh import WorkerSsh
from ClusterShell.Worker.Worker import WorkerError
from ClusterShell.Task import Task, task_self, AlreadyRunningError


class MisusageTest(unittest.TestCase):

    def testTaskResumedTwice(self):
        """test library misusage (task_self resumed twice)"""
        class ResumeAgainHandler(EventHandler):
            def ev_read(self, worker):
                worker.task.resume()
        task = task_self()
        task.shell("/bin/echo OK", handler=ResumeAgainHandler())
        self.assertRaises(AlreadyRunningError, task.resume)

    def testWorkerNotScheduledLocal(self):
        """test library misusage (local worker not scheduled)"""
        task = task_self()
        worker = WorkerPopen(command="/bin/hostname")
        task.resume()
        self.assertRaises(WorkerError, worker.read)

    def testWorkerNotScheduledDistant(self):
        """test library misusage (distant worker not scheduled)"""
        task = task_self()
        worker = WorkerSsh(HOSTNAME, command="/bin/hostname", handler=None, timeout=0)
        self.assert_(worker != None)
        task.resume()
        self.assertRaises(WorkerError, worker.node_buffer, HOSTNAME)

    def testTaskScheduleTwice(self):
        """test task worker schedule twice error"""
        task = task_self()
        self.assert_(task != None)
        worker = task.shell("/bin/echo itsme")
        self.assertRaises(WorkerError, task.schedule, worker)
        task.abort()


if __name__ == '__main__':
    suite = unittest.TestLoader().loadTestsFromTestCase(MisusageTest)
    unittest.TextTestRunner(verbosity=2).run(suite)

