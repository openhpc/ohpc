#!/usr/bin/env python
# ClusterShell test suite
# Written by S. Thiell


"""Unit test for ClusterShell TaskMsgTree variants"""

import sys
import unittest

from ClusterShell.Task import Task, TaskMsgTreeError
from ClusterShell.Task import task_cleanup, task_self
from ClusterShell.Event import EventHandler


class TaskMsgTreeTest(unittest.TestCase):
    """Task/MsgTree test case class"""

    def tearDown(self):
        # cleanup task_self between tests to restore defaults
        task_cleanup()

    def testEnabledMsgTree(self):
        """test TaskMsgTree enabled"""
        task = task_self()
        # init worker
        worker = task.shell("echo foo bar")
        task.set_default('stdout_msgtree', True)
        # run task
        task.resume()
        # should not raise
        for buf, keys in task.iter_buffers():
            pass

    def testEmptyMsgTree(self):
        """test TaskMsgTree empty"""
        task = task_self()
        worker = task.shell("/bin/true")
        # should not raise nor returns anything
        self.assertEqual(list(task.iter_buffers()), [])

    def testDisabledMsgTree(self):
        """test TaskMsgTree disabled"""
        task = task_self()
        worker = task.shell("echo foo bar2")
        task.set_default('stdout_msgtree', False)
        task.resume()
        self.assertRaises(TaskMsgTreeError, task.iter_buffers)
        #
        # can be re-enabled (cold)
        task.set_default('stdout_msgtree', True)
        # but no messages should be found
        self.assertEqual(list(task.iter_buffers()), [])

    def testHotEnablingMsgTree(self):
        """test TaskMsgTree enabling at runtime (v1.7)"""
        class HotEH2(EventHandler):
            def ev_read(self, worker):
                worker.task.set_default("stdout_msgtree", True)
                worker.task.shell("echo foo bar2") # default EH
        task = task_self()
        task.set_default("stdout_msgtree", False)
        self.assertEqual(task.default("stdout_msgtree"), False)
        worker = task.shell("echo foo bar", handler=HotEH2())
        task.resume()
        # only second message has been recorded
        for buf, keys in task.iter_buffers():
            self.assertEqual(buf, "foo bar2")

    def testHotDisablingMsgTree(self):
        """test TaskMsgTree disabling at runtime (v1.7)"""
        class HotEH2(EventHandler):
            def ev_read(self, worker):
                worker.task.set_default("stdout_msgtree", False)
                worker.task.shell("echo foo bar2") # default EH
        task = task_self()
        self.assertEqual(task.default("stdout_msgtree"), True)
        worker = task.shell("echo foo bar", handler=HotEH2())
        task.resume()
        # only first message has been recorded
        for buf, keys in task.iter_buffers():
            self.assertEqual(buf, "foo bar")

    def testEnabledMsgTreeStdErr(self):
        """test TaskMsgTree enabled for stderr"""
        task = task_self()
        worker = task.shell("echo foo bar 1>&2", stderr=True)
        worker = task.shell("echo just foo bar", stderr=True)
        task.set_default('stderr_msgtree', True)
        # run task
        task.resume()
        # should not raise:
        for buf, keys in task.iter_errors():
            pass
        # this neither:
        for buf, keys in task.iter_buffers():
            pass

    def testDisabledMsgTreeStdErr(self):
        """test TaskMsgTree disabled for stderr"""
        task = task_self()
        worker = task.shell("echo foo bar2 1>&2", stderr=True)
        worker = task.shell("echo just foo bar2", stderr=True)
        task.set_default('stderr_msgtree', False)
        # run task
        task.resume()
        # iter_errors() should raise
        self.assertRaises(TaskMsgTreeError, task.iter_errors)
        # but stdout should not
        for buf, keys in task.iter_buffers():
            pass
        #
        # can be re-enabled (cold)
        task.set_default('stderr_msgtree', True)
        # but no messages should be found
        self.assertEqual(list(task.iter_errors()), [])

    def testTaskFlushBuffers(self):
        """test Task.flush_buffers"""
        task = task_self()
        worker = task.shell("echo foo bar")
        task.set_default('stdout_msgtree', True)
        # run task
        task.resume()
        task.flush_buffers()
        self.assertEqual(len(list(task.iter_buffers())), 0)

    def testTaskFlushErrors(self):
        """test Task.flush_errors"""
        task = task_self()
        worker = task.shell("echo foo bar 1>&2")
        task.set_default('stderr_msgtree', True)
        # run task
        task.resume()
        task.flush_errors()
        self.assertEqual(len(list(task.iter_errors())), 0)

    def testTaskModifyCommonStreams(self):
        """test worker common stream names change"""
        task = task_self()
        worker = task.shell("echo foo 1>&2; echo bar", stderr=True)
        worker.SNAME_STDOUT = 'dummy-stdout' # disable buffering on stdout only
        task.resume()
        # only stderr should have been buffered at task level
        self.assertEqual(len(list(task.iter_buffers())), 0)
        self.assertEqual(len(list(task.iter_errors())), 1)

