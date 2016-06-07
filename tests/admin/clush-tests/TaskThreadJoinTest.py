#!/usr/bin/env python
# ClusterShell test suite
# Written by S. Thiell 2010-01-16


"""Unit test for ClusterShell task's join feature in multithreaded
environments"""

import sys
import time
import unittest

sys.path.insert(0, '../lib')

from ClusterShell.Task import *
from ClusterShell.Event import EventHandler


class TaskThreadJoinTest(unittest.TestCase):

    def tearDown(self):
        task_cleanup()

    def testThreadTaskWaitWhenRunning(self):
        """test task_wait() when workers are running"""

        for i in range(1, 5):
            task = Task()
            task.shell("sleep %d" % i)
            task.resume()

        task_wait()


    def testThreadTaskWaitWhenSomeFinished(self):
        """test task_wait() when some workers finished"""

        for i in range(1, 5):
            task = Task()
            task.shell("sleep %d" % i)
            task.resume()

        time.sleep(2)
        task_wait()


    def testThreadTaskWaitWhenAllFinished(self):
        """test task_wait() when all workers finished"""

        for i in range(1, 3):
            task = Task()
            task.shell("sleep %d" % i)
            task.resume()

        time.sleep(4)
        task_wait()

    def testThreadSimpleTaskSupervisor(self):
        """test task methods from another thread"""
        #print "PASS 1"
        task = Task()
        task.shell("sleep 3")
        task.shell("echo testing", key=1)
        task.resume()
        task.join()
        self.assertEqual(task.key_buffer(1), "testing")
        #print "PASS 2"
        task.shell("echo ok", key=2)
        task.resume()
        task.join()
        #print "PASS 3"
        self.assertEqual(task.key_buffer(2), "ok")
        task.shell("sleep 1 && echo done", key=3)
        task.resume()
        task.join()
        #print "PASS 4"
        self.assertEqual(task.key_buffer(3), "done")
        task.abort()

    def testThreadTaskBuffers(self):
        """test task data access methods after join()"""
        task = Task()
        # test data access from main thread

        # test stderr separated
        task.set_default("stderr", True)
        task.shell("echo foobar", key="OUT")
        task.shell("echo raboof 1>&2", key="ERR")
        task.resume()
        task.join()
        self.assertEqual(task.key_buffer("OUT"), "foobar")
        self.assertEqual(task.key_error("OUT"), "")
        self.assertEqual(task.key_buffer("ERR"), "")
        self.assertEqual(task.key_error("ERR"), "raboof")

        # test stderr merged
        task.set_default("stderr", False)
        task.shell("echo foobar", key="OUT")
        task.shell("echo raboof 1>&2", key="ERR")
        task.resume()
        task.join()
        self.assertEqual(task.key_buffer("OUT"), "foobar")
        self.assertEqual(task.key_error("OUT"), "")
        self.assertEqual(task.key_buffer("ERR"), "raboof")
        self.assertEqual(task.key_error("ERR"), "")

    def testThreadTaskUnhandledException(self):
        """test task unhandled exception in thread"""
        class TestUnhandledException(Exception):
            """test exception"""
        class RaiseOnRead(EventHandler):
            def ev_read(self, worker):
                raise TestUnhandledException("you should see this exception")

        task = Task()
        # test data access from main thread
        task.shell("echo raisefoobar", key=1, handler=RaiseOnRead())
        task.resume()
        task.join()
        self.assertEqual(task.key_buffer(1), "raisefoobar")
        time.sleep(1) # for pretty display, because unhandled exception
                      # traceback may be sent to stderr after the join()
        self.assertFalse(task.running())

