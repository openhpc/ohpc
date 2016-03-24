#!/usr/bin/env python
# ClusterShell test suite
# Written by S. Thiell 2009-12-19


"""Unit test for ClusterShell inter-Task msg"""

import pickle
import sys
import threading
import unittest

sys.path.insert(0, '../lib')

from ClusterShell.Task import *
from ClusterShell.Event import EventHandler


class TaskPortTest(unittest.TestCase):

    def tearDown(self):
        task_cleanup()

    def testPortMsg1(self):
        """test port msg from main thread to task"""
        
        TaskPortTest.got_msg = False

        # create task in new thread
        task = Task()

        class PortHandler(EventHandler):
            def ev_msg(self, port, msg):
                # receive msg
                assert msg == "toto"
                assert port.task.thread == threading.currentThread()
                TaskPortTest.got_msg = True
                port.task.abort()

        # create non-autoclosing port
        port = task.port(handler=PortHandler())
        task.resume()
        # send msg from main thread
        port.msg("toto")
        task_wait()
        self.assert_(TaskPortTest.got_msg)

    def testPortRemove(self):
        """test port remove [private as of 1.2]"""
        
        task = Task()

        class PortHandler(EventHandler):
            def ev_msg(self, port, msg):
                pass

        port = task.port(handler=PortHandler(), autoclose=True)
        task.resume()
        task._remove_port(port)
        task_wait()


if __name__ == '__main__':
    suite = unittest.TestLoader().loadTestsFromTestCase(TaskPortTest)
    unittest.TextTestRunner(verbosity=2).run(suite)

