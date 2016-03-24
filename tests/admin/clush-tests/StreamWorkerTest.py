#!/usr/bin/env python
# StreamWorker test suite

import os
import unittest

from ClusterShell.Worker.Worker import StreamWorker, WorkerError
from ClusterShell.Task import task_self
from ClusterShell.Event import EventHandler


class StreamTest(unittest.TestCase):

    def run_worker(self, worker):
        """helper method to schedule and run a worker"""
        task_self().schedule(worker)
        task_self().run()

    def test_001_empty(self):
        """test empty StreamWorker"""
        # that makes no sense but well...
        # handler=None is supported by base Worker class
        self.run_worker(StreamWorker(handler=None))

    def test_002_pipe_readers(self):
        """test StreamWorker bound to several pipe readers"""

        streams = { "pipe1_reader": "Some data to read from a pipe",
                    "stderr": "Error data to read using special keyword stderr",
                    "pipe2_reader": "Other data to read from another pipe",
                    "pipe3_reader": "Cool data to read from a third pipe" }

        class TestH(EventHandler):
            def __init__(self, testcase):
                self.snames = set()
                self.testcase = testcase

            def ev_error(self, worker):
                # test that ev_error is called in case of 'stderr' stream name
                self.testcase.assertEqual(worker.current_sname, "stderr")
                self.recv_msg(worker.current_errmsg)

            def ev_read(self, worker):
                self.recv_msg(worker.current_msg)

            def recv_msg(self, msg):
                self.testcase.assertTrue(len(self.snames) < len(streams))
                self.testcase.assertEqual(streams[worker.current_sname], msg)
                self.snames.add(worker.current_sname)
                if len(self.snames) == len(streams):
                    # before finishing, try to add another pipe at
                    # runtime: this is NOT allowed
                    rfd, wfd = os.pipe()
                    self.testcase.assertRaises(WorkerError,
                        worker.set_reader, "pipe4_reader", rfd)
                    self.testcase.assertRaises(WorkerError,
                        worker.set_writer, "pipe4_writer", wfd)
                    os.close(rfd)
                    os.close(wfd)

        # create a StreamWorker instance bound to several pipes
        hdlr = TestH(self)
        worker = StreamWorker(handler=hdlr)

        for sname in streams.keys():
            rfd, wfd = os.pipe()
            worker.set_reader(sname, rfd)
            os.write(wfd, streams[sname])
            os.close(wfd)

        self.run_worker(worker)

        # check that all ev_read have been received
        self.assertEqual(set(("pipe1_reader", "pipe2_reader", "pipe3_reader",
                              "stderr")), hdlr.snames)

    def test_003_io_pipes(self):
        """test StreamWorker bound to pipe readers and writers"""

        # os.write -> pipe1 -> worker -> pipe2 -> os.read

        class TestH(EventHandler):
            def __init__(self, testcase):
                self.testcase = testcase
                self.worker = None
                self.pickup_count = 0
                self.hup_count = 0

            def ev_pickup(self, worker):
                self.pickup_count += 1

            def ev_read(self, worker):
                self.testcase.assertEqual(worker.current_sname, "pipe1")
                worker.write(worker.current_msg, "pipe2")

            def ev_timer(self, timer):
                # call set_write_eof on specific stream after some delay
                worker = self.worker
                self.worker = 'DONE'
                worker.set_write_eof("pipe2")

            def ev_hup(self, worker):
                # ev_hup called at the end (after set_write_eof is called)
                self.hup_count += 1
                self.testcase.assertEqual(self.worker, 'DONE')
                # no rc code should be set
                self.testcase.assertEqual(worker.current_rc, None)

        # create a StreamWorker instance bound to several pipes
        hdlr = TestH(self)
        worker = StreamWorker(handler=hdlr)
        hdlr.worker = worker

        rfd1, wfd1 = os.pipe()
        worker.set_reader("pipe1", rfd1)
        os.write(wfd1, "Some data\n")
        os.close(wfd1)

        rfd2, wfd2 = os.pipe()
        worker.set_writer("pipe2", wfd2)

        timer1 = task_self().timer(1.0, handler=hdlr)
        self.run_worker(worker)
        self.assertEqual(os.read(rfd2, 1024), "Some data")
        os.close(rfd2)
        # wfd2 should be closed by CS
        self.assertRaises(OSError, os.close, wfd2)
        # rfd1 should be closed by CS
        self.assertRaises(OSError, os.close, rfd1)
        # check pickup/hup
        self.assertEqual(hdlr.hup_count, 1)
        self.assertEqual(hdlr.pickup_count, 1)

    def test_004_timeout_on_open_stream(self):
        """test StreamWorker with timeout set on open stream"""
        # Create worker set with timeout
        worker = StreamWorker(handler=None, timeout=0.5)

        # Create pipe stream
        rfd1, wfd1 = os.pipe()
        worker.set_reader("pipe1", rfd1, closefd=False)
        # Write some chars without line break (worst case)
        os.write(wfd1, "Some data")
        # TEST: Do not close wfd1 to simulate open stream

        # Need to enable pipe1_msgtree
        task_self().set_default("pipe1_msgtree", True)
        self.run_worker(worker)

        # Timeout occured - read buffer should have been flushed
        self.assertEqual(worker.read(sname="pipe1"), "Some data")

        # closefd was set, we should be able to close pipe fds
        os.close(rfd1)
        os.close(wfd1)

    def test_005_timeout_events(self):
        """test StreamWorker with timeout set (event based)"""
        class TestH(EventHandler):
            def __init__(self, testcase):
                self.testcase = testcase
                self.ev_pickup_called = False
                self.ev_read_called = False
                self.ev_hup_called = False
                self.ev_timeout_called = False

            def ev_pickup(self, worker):
                self.ev_pickup_called = True

            def ev_read(self, worker):
                self.ev_read_called = True
                self.testcase.assertEqual(worker.current_sname, "pipe1")
                self.testcase.assertEqual(worker.current_msg, "Some data")

            def ev_hup(self, worker):
                # ev_hup is called but no rc code should be set
                self.ev_hup_called = True
                self.testcase.assertEqual(worker.current_rc, None)

            def ev_timeout(self, worker):
                self.ev_timeout_called = True

        hdlr = TestH(self)
        worker = StreamWorker(handler=hdlr, timeout=0.5)

        # Create pipe stream with closefd set (default)
        rfd1, wfd1 = os.pipe()
        worker.set_reader("pipe1", rfd1)
        # Write some chars without line break (worst case)
        os.write(wfd1, "Some data")
        # TEST: Do not close wfd1 to simulate open stream

        self.run_worker(worker)
        self.assertTrue(hdlr.ev_timeout_called)
        self.assertTrue(hdlr.ev_read_called)
        self.assertTrue(hdlr.ev_pickup_called)
        self.assertTrue(hdlr.ev_hup_called)

        # rfd1 should be already closed by CS
        self.assertRaises(OSError, os.close, rfd1)
        os.close(wfd1)

