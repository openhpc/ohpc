#!/usr/bin/env python
# ClusterShell (local) test suite
# Written by S. Thiell


"""Unit test for ClusterShell Task (local)"""

import copy
import os
import signal
import sys
import time

sys.path.insert(0, '../lib')

import ClusterShell

from ClusterShell.Defaults import DEFAULTS
from ClusterShell.Event import EventHandler
from ClusterShell.NodeSet import NodeSet
from ClusterShell.Task import *
from ClusterShell.Worker.Worker import WorkerSimple, WorkerError
from ClusterShell.Worker.Worker import WorkerBadArgumentError

# private import
from ClusterShell.Engine.Engine import E_READ, E_WRITE

import socket

import threading
import tempfile


def _test_print_debug(task, s):
    # Use custom task info (prefix 'user_' is recommended)
    task.set_info("user_print_debug_last", s)

class TaskLocalMixin(object):
    """Mixin test case class: should be overrided and used in multiple
    inheritance with unittest.TestCase"""

    def testSimpleCommand(self):
        task = task_self()
        self.assert_(task != None)
        # init worker
        worker = task.shell("/bin/hostname")
        self.assert_(worker != None)
        # run task
        task.resume()

    def testSimpleDualTask(self):
        task0 = task_self()
        self.assert_(task0 != None)
        worker1 = task0.shell("/bin/hostname")
        worker2 = task0.shell("/bin/uname -a")
        task0.resume()
        b1 = copy.copy(worker1.read())
        b2 = copy.copy(worker2.read())
        task1 = task_self()
        self.assert_(task1 is task0)
        worker1 = task1.shell("/bin/hostname")
        self.assert_(worker1 != None)
        worker2 = task1.shell("/bin/uname -a")
        self.assert_(worker2 != None)
        task1.resume()
        self.assert_(worker2.read() == b2)
        self.assert_(worker1.read() == b1)

    def testSimpleCommandNoneArgs(self):
        task = task_self()
        self.assert_(task != None)
        # init worker
        worker = task.shell("/bin/hostname", nodes=None, handler=None)
        self.assert_(worker != None)
        # run task
        task.resume()

    def testSimpleMultipleCommands(self):
        task = task_self()
        self.assert_(task != None)
        # run commands
        workers = []
        for i in range(0, 100):
            workers.append(task.shell("/bin/hostname"))
        task.resume()
        # verify results
        hn = socket.gethostname()
        for i in range(0, 100):
            t_hn = workers[i].read().splitlines()[0]
            self.assertEqual(t_hn, hn)

    def testHugeOutputCommand(self):
        task = task_self()

        # init worker
        worker = task.shell("python test_command.py --test huge --rc 0")
        self.assert_(worker != None)

        # run task
        task.resume()
        self.assertEqual(worker.retcode(), 0)
        self.assertEqual(len(worker.read()), 699999)

    # task configuration
    def testTaskInfo(self):
        task = task_self()
        fanout = task.info("fanout")
        self.assertEqual(fanout, DEFAULTS.fanout)

    def testSimpleCommandTimeout(self):
        task = task_self()

        # init worker
        worker = task.shell("/bin/sleep 30")
        self.assert_(worker != None)

        # run task
        self.assertRaises(TimeoutError, task.resume, 1)

    def testSimpleCommandNoTimeout(self):
        task = task_self()

        # init worker
        worker = task.shell("/bin/sleep 1")
        self.assert_(worker != None)

        try:
            # run task
            task.resume(3)
        except TimeoutError:
            self.fail("did detect timeout")

    def testSimpleCommandNoTimeout(self):
        task = task_self()

        # init worker
        worker = task.shell("/bin/usleep 900000")
        self.assert_(worker != None)

        try:
            # run task
            task.resume(1)
        except TimeoutError:
            self.fail("did detect timeout")

    def testWorkersTimeout(self):
        task = task_self()
        self.assert_(task != None)

        # init worker
        worker = task.shell("/bin/sleep 6", timeout=1)
        self.assert_(worker != None)

        worker = task.shell("/bin/sleep 6", timeout=0.5)
        self.assert_(worker != None)

        try:
            # run task
            task.resume()
        except TimeoutError:
            self.fail("did detect timeout")

        self.assert_(worker.did_timeout())

    def testWorkersTimeout2(self):
        task = task_self()
        self.assert_(task != None)

        worker = task.shell("/bin/sleep 10", timeout=1)
        self.assert_(worker != None)

        worker = task.shell("/bin/sleep 10", timeout=0.5)
        self.assert_(worker != None)

        try:
            # run task
            task.resume()
        except TimeoutError:
            self.fail("did detect task timeout")

    def testWorkersAndTaskTimeout(self):
        task = task_self()
        self.assert_(task != None)

        worker = task.shell("/bin/sleep 10", timeout=5)
        self.assert_(worker != None)

        worker = task.shell("/bin/sleep 10", timeout=3)
        self.assert_(worker != None)

        self.assertRaises(TimeoutError, task.resume, 1)

    def testLocalEmptyBuffer(self):
        task = task_self()
        self.assert_(task != None)
        task.shell("true", key="empty")
        task.resume()
        self.assertEqual(task.key_buffer("empty"), '')
        for buf, keys in task.iter_buffers():
            self.assert_(False)

    def testLocalEmptyError(self):
        task = task_self()
        self.assert_(task != None)
        task.shell("true", key="empty")
        task.resume()
        self.assertEqual(task.key_error("empty"), '')
        for buf, keys in task.iter_errors():
            self.assert_(False)

    def testTaskKeyErrors(self):
        task = task_self()
        self.assert_(task != None)
        task.shell("true", key="dummy")
        task.resume()
        # task.key_retcode raises KeyError
        self.assertRaises(KeyError, task.key_retcode, "not_known")
        # unlike task.key_buffer/error
        self.assertEqual(task.key_buffer("not_known"), '')
        self.assertEqual(task.key_error("not_known"), '')

    def testLocalSingleLineBuffers(self):
        task = task_self()
        self.assert_(task != None)

        task.shell("/bin/echo foo", key="foo")
        task.shell("/bin/echo bar", key="bar")
        task.shell("/bin/echo bar", key="bar2")
        task.shell("/bin/echo foobar", key="foobar")
        task.shell("/bin/echo foobar", key="foobar2")
        task.shell("/bin/echo foobar", key="foobar3")

        task.resume()

        self.assert_(task.key_buffer("foobar") == "foobar")

        cnt = 3
        for buf, keys in task.iter_buffers():
            cnt -= 1
            if buf == "foo":
                self.assertEqual(len(keys), 1)
                self.assertEqual(keys[0], "foo")
            elif buf == "bar":
                self.assertEqual(len(keys), 2)
                self.assert_(keys[0] == "bar" or keys[1] == "bar")
            elif buf == "foobar":
                self.assertEqual(len(keys), 3)

        self.assertEqual(cnt, 0)

    def testLocalBuffers(self):
        task = task_self()
        self.assert_(task != None)

        task.shell("/usr/bin/printf 'foo\nbar\n'", key="foobar")
        task.shell("/usr/bin/printf 'foo\nbar\n'", key="foobar2")
        task.shell("/usr/bin/printf 'foo\nbar\n'", key="foobar3")
        task.shell("/usr/bin/printf 'foo\nbar\nxxx\n'", key="foobarX")
        task.shell("/usr/bin/printf 'foo\nfuu\n'", key="foofuu")
        task.shell("/usr/bin/printf 'faa\nber\n'", key="faaber")
        task.shell("/usr/bin/printf 'foo\nfuu\n'", key="foofuu2")

        task.resume()

        cnt = 4
        for buf, keys in task.iter_buffers():
            cnt -= 1
            if buf == "faa\nber\n":
                self.assertEqual(len(keys), 1)
                self.assert_(keys[0].startswith("faaber"))
            elif buf == "foo\nfuu\n":
                self.assertEqual(len(keys), 2)
                self.assert_(keys[0].startswith("foofuu"))
            elif buf == "foo\nbar\n":
                self.assertEqual(len(keys), 3)
            elif buf == "foo\nbar\nxxx\n":
                self.assertEqual(len(keys), 1)
                self.assert_(keys[0].startswith("foobarX"))
                self.assert_(keys[0].startswith("foobar"))
            elif buf == "foo\nbar\nxxx\n":
                self.assertEqual(len(keys), 1)
                self.assert_(keys[0].startswith("foobarX"))

        self.assertEqual(cnt, 0)

    def testLocalRetcodes(self):
        task = task_self()
        self.assert_(task != None)

        # 0 ['worker0']
        # 1 ['worker1']
        # 2 ['worker2']
        # 3 ['worker3bis', 'worker3']
        # 4 ['worker4']
        # 5 ['worker5bis', 'worker5']

        task.shell("true", key="worker0")
        task.shell("false", key="worker1")
        task.shell("/bin/sh -c 'exit 1'", key="worker1bis")
        task.shell("/bin/sh -c 'exit 2'", key="worker2")
        task.shell("/bin/sh -c 'exit 3'", key="worker3")
        task.shell("/bin/sh -c 'exit 3'", key="worker3bis")
        task.shell("/bin/sh -c 'exit 4'", key="worker4")
        task.shell("/bin/sh -c 'exit 1'", key="worker4")
        task.shell("/bin/sh -c 'exit 5'", key="worker5")
        task.shell("/bin/sh -c 'exit 5'", key="worker5bis")

        task.resume()

        # test key_retcode(key)
        self.assertEqual(task.key_retcode("worker2"), 2) # single
        self.assertEqual(task.key_retcode("worker4"), 4) # multiple
        self.assertRaises(KeyError, task.key_retcode, "worker9") # error

        cnt = 6
        for rc, keys in task.iter_retcodes():
            cnt -= 1
            if rc == 0:
                self.assertEqual(len(keys), 1)
                self.assert_(keys[0] == "worker0" )
            elif rc == 1:
                self.assertEqual(len(keys), 3)
                self.assert_(keys[0] in ("worker1", "worker1bis", "worker4"))
            elif rc == 2:
                self.assertEqual(len(keys), 1)
                self.assert_(keys[0] == "worker2" )
            elif rc == 3:
                self.assertEqual(len(keys), 2)
                self.assert_(keys[0] in ("worker3", "worker3bis"))
            elif rc == 4:
                self.assertEqual(len(keys), 1)
                self.assert_(keys[0] == "worker4" )
            elif rc == 5:
                self.assertEqual(len(keys), 2)
                self.assert_(keys[0] in ("worker5", "worker5bis"))

        self.assertEqual(cnt, 0)

        # test max retcode API
        self.assertEqual(task.max_retcode(), 5)

    def testCustomPrintDebug(self):
        task = task_self()
        self.assert_(task != None)

        # first test that simply changing print_debug doesn't enable debug
        default_print_debug = task.info("print_debug")
        try:
            task.set_info("print_debug", _test_print_debug)
            task.shell("true")
            task.resume()
            self.assertEqual(task.info("user_print_debug_last"), None)

            # with debug enabled, it should work
            task.set_info("debug", True)
            task.shell("true")
            task.resume()
            self.assertEqual(task.info("user_print_debug_last"), "POPEN: true")

            # remove debug
            task.set_info("debug", False)
            # re-run for default print debug callback code coverage
            task.shell("true")
            task.resume()
        finally:
            # restore default print_debug
            task.set_info("debug", False)
            task.set_info("print_debug", default_print_debug)

    def testLocalRCBufferGathering(self):
        task = task_self()
        self.assert_(task != None)

        task.shell("/usr/bin/printf 'foo\nbar\n' && exit 1", key="foobar5")
        task.shell("/usr/bin/printf 'foo\nbur\n' && exit 1", key="foobar2")
        task.shell("/usr/bin/printf 'foo\nbar\n' && exit 1", key="foobar3")
        task.shell("/usr/bin/printf 'foo\nfuu\n' && exit 5", key="foofuu")
        task.shell("/usr/bin/printf 'foo\nbar\n' && exit 4", key="faaber")
        task.shell("/usr/bin/printf 'foo\nfuu\n' && exit 1", key="foofuu2")

        task.resume()

        foobur = "foo\nbur"

        cnt = 5
        for rc, keys in task.iter_retcodes():
            for buf, keys in task.iter_buffers(keys):
                cnt -= 1
                if buf == "foo\nbar":
                    self.assert_(rc == 1 or rc == 4)
                elif foobur == buf:
                    self.assertEqual(rc, 1)
                elif "foo\nfuu" == buf:
                    self.assert_(rc == 1 or rc == 5)
                else:
                    self.fail("invalid buffer returned")

        self.assertEqual(cnt, 0)

    def testLocalBufferRCGathering(self):
        task = task_self()
        self.assert_(task != None)

        task.shell("/usr/bin/printf 'foo\nbar\n' && exit 1", key="foobar5")
        task.shell("/usr/bin/printf 'foo\nbur\n' && exit 1", key="foobar2")
        task.shell("/usr/bin/printf 'foo\nbar\n' && exit 1", key="foobar3")
        task.shell("/usr/bin/printf 'foo\nfuu\n' && exit 5", key="foofuu")
        task.shell("/usr/bin/printf 'foo\nbar\n' && exit 4", key="faaber")
        task.shell("/usr/bin/printf 'foo\nfuu\n' && exit 1", key="foofuu2")

        task.resume()

        cnt = 9
        for buf, keys in task.iter_buffers():
            for rc, keys in task.iter_retcodes(keys):
                # same checks as testLocalRCBufferGathering
                cnt -= 1
                if buf == "foo\nbar\n":
                    self.assert_(rc == 1 and rc == 4)
                elif buf == "foo\nbur\n":
                    self.assertEqual(rc, 1)
                elif buf == "foo\nbuu\n":
                    self.assertEqual(rc, 5)

        self.assertEqual(cnt, 0)

    def testLocalWorkerWrites(self):
        # Simple test: we write to a cat process and see if read matches.
        task = task_self()
        worker = task.shell("cat")
        # write first line
        worker.write("foobar\n")
        # write second line
        worker.write("deadbeaf\n")
        worker.set_write_eof()
        task.resume()
        self.assertEqual(worker.read(), "foobar\ndeadbeaf")

    def testLocalWorkerWritesBcExample(self):
        # Other test: write a math statement to a bc process and check
        # for the result.
        task = task_self()
        self.assert_(task != None)
        worker = task.shell("bc -q")

        # write statement
        worker.write("2+2\n")
        worker.set_write_eof()

        # execute
        task.resume()

        # read result
        self.assertEqual(worker.read(), "4")

    def testLocalWorkerWritesWithLateEOF(self):
        class LateEOFHandler(EventHandler):
            def ev_start(self, worker):
                worker.set_write_eof()

        task = task_self()
        self.assert_(task != None)
        worker = task.shell("(sleep 1; cat)", handler=LateEOFHandler())
        worker.write("cracoucasse\n")
        task.resume()

        # read result
        self.assertEqual(worker.read(), "cracoucasse")

    def testEscape(self):
        task = task_self()
        self.assert_(task != None)
        worker = task.shell("export CSTEST=foobar; /bin/echo \$CSTEST | sed 's/\ foo/bar/'")
        # execute
        task.resume()
        # read result
        self.assertEqual(worker.read(), "$CSTEST")

    def testEscape2(self):
        task = task_self()
        self.assert_(task != None)
        worker = task.shell("export CSTEST=foobar; /bin/echo $CSTEST | sed 's/\ foo/bar/'")
        # execute
        task.resume()
        # read result
        self.assertEqual(worker.read(), "foobar")

    def testEngineClients(self):
        # private EngineClient stream basic tests
        class StartHandler(EventHandler):
            def __init__(self, test):
                self.test = test
            def ev_start(self, worker):
                if len(streams) == 2:
                    for streamd in streams:
                        for name, stream in streamd.iteritems():
                            self.test.assertTrue(name in ['stdin', 'stdout', 'stderr'])
                            if name == 'stdin':
                                self.test.assertTrue(stream.writable())
                                self.test.assertFalse(stream.readable())
                            else:
                                self.test.assertTrue(stream.readable())
                                self.test.assertFalse(stream.writable())

        task = task_self()
        self.assert_(task != None)
        shdl = StartHandler(self)
        worker1 = task.shell("/bin/hostname", handler=shdl)
        self.assert_(worker1 != None)
        worker2 = task.shell("echo ok", handler=shdl)
        self.assert_(worker2 != None)
        engine = task._engine
        clients = engine.clients()
        self.assertEqual(len(clients), 2)
        streams = [client.streams for client in clients]
        task.resume()

    def testEnginePorts(self):
        task = task_self()
        self.assert_(task != None)
        worker = task.shell("/bin/hostname")
        self.assert_(worker != None)
        self.assertEqual(len(task._engine.ports()), 1)
        task.resume()

    def testSimpleCommandAutoclose(self):
        task = task_self()
        self.assert_(task != None)
        worker = task.shell("/bin/sleep 3; /bin/uname", autoclose=True)
        self.assert_(worker != None)
        task.resume()
        self.assertEqual(worker.read(), None)

    def testTwoSimpleCommandsAutoclose(self):
        task = task_self()
        self.assert_(task != None)
        worker1 = task.shell("/bin/sleep 2; /bin/echo ok")
        worker2 = task.shell("/bin/sleep 3; /bin/uname", autoclose=True)
        self.assert_(worker2 != None)
        task.resume()
        self.assertEqual(worker1.read(), "ok")
        self.assertEqual(worker2.read(), None)

    def test_unregister_stream_autoclose(self):
        task = task_self()
        self.assert_(task != None)
        worker1 = task.shell("/bin/sleep 2; /bin/echo ok")
        worker2 = task.shell("/bin/sleep 3; /bin/uname", autoclose=True)
        # the following leads to a call to unregister_stream() with autoclose flag set
        worker3 = task.shell("sleep 1; echo blah | cat", autoclose=True)
        task.resume()
        self.assertEqual(worker1.read(), "ok")
        self.assertEqual(worker2.read(), None)

    def testLocalWorkerErrorBuffers(self):
        task = task_self()
        self.assert_(task != None)
        w1 = task.shell("/usr/bin/printf 'foo bar\n' 1>&2", key="foobar", stderr=True)
        w2 = task.shell("/usr/bin/printf 'foo\nbar\n' 1>&2", key="foobar2", stderr=True)
        task.resume()
        self.assertEqual(w1.error(), 'foo bar')
        self.assertEqual(w2.error(), 'foo\nbar')

    def testLocalErrorBuffers(self):
        task = task_self()
        self.assert_(task != None)

        task.shell("/usr/bin/printf 'foo\nbar\n' 1>&2", key="foobar", stderr=True)
        task.shell("/usr/bin/printf 'foo\nbar\n' 1>&2", key="foobar2", stderr=True)
        task.shell("/usr/bin/printf 'foo\nbar\n 1>&2'", key="foobar3", stderr=True)
        task.shell("/usr/bin/printf 'foo\nbar\nxxx\n' 1>&2", key="foobarX", stderr=True)
        task.shell("/usr/bin/printf 'foo\nfuu\n' 1>&2", key="foofuu", stderr=True)
        task.shell("/usr/bin/printf 'faa\nber\n' 1>&2", key="faaber", stderr=True)
        task.shell("/usr/bin/printf 'foo\nfuu\n' 1>&2", key="foofuu2", stderr=True)

        task.resume()

        cnt = 4
        for buf, keys in task.iter_errors():
            cnt -= 1
            if buf == "faa\nber\n":
                self.assertEqual(len(keys), 1)
                self.assert_(keys[0].startswith("faaber"))
            elif buf == "foo\nfuu\n":
                self.assertEqual(len(keys), 2)
                self.assert_(keys[0].startswith("foofuu"))
            elif buf == "foo\nbar\n":
                self.assertEqual(len(keys), 3)
                self.assert_(keys[0].startswith("foobar"))
            elif buf == "foo\nbar\nxxx\n":
                self.assertEqual(len(keys), 1)
                self.assert_(keys[0].startswith("foobarX"))

        self.assertEqual(cnt, 0)

    def testTaskPrintDebug(self):
        task = task_self()
        self.assert_(task != None)
        # simple test, just run a task with debug on to improve test
        # code coverage
        task.set_info("debug", True)
        worker = task.shell("/bin/echo test")
        self.assert_(worker != None)
        task.resume()
        task.set_info("debug", False)

    def testTaskAbortSelf(self):
        task = task_self()
        self.assert_(task != None)

        # abort(False) keeps current task_self() object
        task.abort()
        self.assert_(task == task_self())

        # abort(True) unbinds current task_self() object
        task.abort(True)
        self.assert_(task != task_self())

        # retry
        task = task_self()
        self.assert_(task != None)
        worker = task.shell("/bin/echo shouldnt see that")
        task.abort()
        self.assert_(task == task_self())

    def testTaskAbortHandler(self):

        class AbortOnReadTestHandler(EventHandler):
            def ev_read(self, worker):
                self.has_ev_read = True
                worker.task.abort()
                assert False, "Shouldn't reach this line"

        task = task_self()
        self.assert_(task != None)
        eh = AbortOnReadTestHandler()
        eh.has_ev_read = False
        task.shell("/bin/echo test", handler=eh)
        task.resume()
        self.assert_(eh.has_ev_read)

    def testWorkerSetKey(self):
        task = task_self()
        self.assert_(task != None)
        task.shell("/bin/echo foo", key="foo")
        worker = task.shell("/bin/echo foobar")
        worker.set_key("bar")
        task.resume()
        self.assert_(task.key_buffer("bar") == "foobar")

    def testWorkerSimplePipeStdout(self):
        task = task_self()
        rfd, wfd = os.pipe()
        os.write(wfd, "test\n")
        os.close(wfd)
        worker = WorkerSimple(os.fdopen(rfd), None, None, "pipe", None,
                              stderr=True, timeout=-1, autoclose=False,
                              closefd=False)
        self.assertEqual(worker.reader_fileno(), rfd)
        task.schedule(worker)
        task.resume()
        self.assertEqual(task.key_buffer("pipe"), 'test')
        dummy = os.fstat(rfd) # just to check that rfd is still valid here
                              # (worker keeps a reference of file object)
        # rfd will be closed when associated file is released

    def testWorkerSimplePipeStdErr(self):
        task = task_self()
        rfd, wfd = os.pipe()
        os.write(wfd, "test\n")
        os.close(wfd)
        # be careful, stderr is arg #3
        worker = WorkerSimple(None, None, os.fdopen(rfd), "pipe", None,
                              stderr=True, timeout=-1, autoclose=False,
                              closefd=False)
        self.assertEqual(worker.error_fileno(), rfd)
        task.schedule(worker)
        task.resume()
        self.assertEqual(task.key_error("pipe"), 'test')
        dummy = os.fstat(rfd) # just to check that rfd is still valid here
        # rfd will be closed when associated file is released

    def testWorkerSimplePipeStdin(self):
        task = task_self()
        rfd, wfd = os.pipe()
        # be careful, stdin is arg #2
        worker = WorkerSimple(None, os.fdopen(wfd, "w"), None, "pipe", None,
                              stderr=True, timeout=-1, autoclose=False,
                              closefd=False)
        self.assertEqual(worker.writer_fileno(), wfd)
        worker.write("write to stdin test\n")
        worker.set_write_eof() # close stream after write!
        task.schedule(worker)
        task.resume()
        self.assertEqual(os.read(rfd, 1024), "write to stdin test\n")
        os.close(rfd)
        # wfd will be closed when associated file is released

    # FIXME: reconsider this kind of test (which now must fail) especially
    #        when using epoll engine, as soon as testsuite is improved (#95).
    #def testWorkerSimpleFile(self):
    #    """test WorkerSimple (file)"""
    #    task = task_self()
    #    self.assert_(task != None)
    #    # use tempfile
    #    tmpfile = tempfile.TemporaryFile()
    #    tmpfile.write("one line without EOL")
    #    tmpfile.seek(0)
    #    worker = WorkerSimple(tmpfile, None, None, "file", None, 0, True)
    #    self.assert_(worker != None)
    #    task.schedule(worker)
    #    task.resume()
    #    self.assertEqual(worker.read(), "one line without EOL")

    def testInterruptEngine(self):
        class KillerThread(threading.Thread):
            def run(self):
                time.sleep(1)
                os.kill(self.pidkill, signal.SIGUSR1)
                task_wait()

        kth = KillerThread()
        kth.pidkill = os.getpid()

        task = task_self()
        signal.signal(signal.SIGUSR1, lambda x, y: None)
        task.shell("/bin/sleep 2", timeout=5)

        kth.start()
        task.resume()

    def testSignalWorker(self):
        class TestSignalHandler(EventHandler):
            def ev_read(self, worker):
                pid = int(worker.current_msg)
                os.kill(pid, signal.SIGTERM)
        task = task_self()
        wrk = task.shell("echo $$; /bin/sleep 2", handler=TestSignalHandler())
        task.resume()
        self.assertEqual(wrk.retcode(), 128 + signal.SIGTERM)

    def testShellDelayedIO(self):
        class TestDelayedHandler(EventHandler):
            def __init__(self, target_worker=None):
                self.target_worker = target_worker
                self.counter = 0
            def ev_read(self, worker):
                self.counter += 1
                if self.counter == 100:
                    worker.write("another thing to read\n")
                    worker.set_write_eof()
            def ev_timer(self, timer):
                self.target_worker.write("something to read\n" * 300)

        task = task_self()
        hdlr = TestDelayedHandler()
        reader = task.shell("cat", handler=hdlr)
        timer = task.timer(0.6, handler=TestDelayedHandler(reader))
        task.resume()
        self.assertEqual(hdlr.counter, 301)

    def testSimpleCommandReadNoEOL(self):
        task = task_self()
        self.assert_(task != None)
        # init worker
        worker = task.shell("/bin/echo -n okay")
        self.assert_(worker != None)
        # run task
        task.resume()
        self.assertEqual(worker.read(), "okay")

    def testLocalFanout(self):
        task = task_self()
        self.assert_(task != None)
        fanout = task.info("fanout")
        try:
            task.set_info("fanout", 3)

            # Test #1: simple
            for i in range(0, 10):
                worker = task.shell("/bin/echo test %d" % i)
                self.assert_(worker != None)
            task.resume()

            # Test #2: fanout change during run
            class TestFanoutChanger(EventHandler):
                def ev_timer(self, timer):
                    task_self().set_info("fanout", 1)
            timer = task.timer(2.0, handler=TestFanoutChanger())
            for i in range(0, 10):
                worker = task.shell("sleep 0.5")
                self.assert_(worker != None)
            task.resume()
        finally:
            # restore original fanout value
            task.set_info("fanout", fanout)

    def testPopenBadArgumentOption(self):
	    # Check code < 1.4 compatibility
        self.assertRaises(WorkerBadArgumentError, WorkerPopen, None, None)
	    # As of 1.4, ValueError is raised for missing parameter
        self.assertRaises(ValueError, WorkerPopen, None, None) # 1.4+

    def testWorkerAbort(self):
        task = task_self()
        self.assert_(task != None)

        class AbortOnTimer(EventHandler):
            def __init__(self, worker):
                EventHandler.__init__(self)
                self.ext_worker = worker
                self.testtimer = False
            def ev_timer(self, timer):
                self.ext_worker.abort()
                self.testtimer = True

        aot = AbortOnTimer(task.shell("sleep 10"))
        self.assertEqual(aot.testtimer, False)
        task.timer(1.0, handler=aot)
        task.resume()
        self.assertEqual(aot.testtimer, True)

    def testWorkerAbortSanity(self):
        task = task_self()
        worker = task.shell("sleep 1")
        worker.abort()

        # test noop abort() on unscheduled worker
        worker = WorkerPopen("sleep 1")
        worker.abort()

    def testKBI(self):
        class TestKBI(EventHandler):
            def ev_read(self, worker):
                raise KeyboardInterrupt
        task = task_self()
        self.assert_(task != None)
        ok = False
        try:
            task.run("echo test; sleep 5", handler=TestKBI())
        except KeyboardInterrupt:
            ok = True
            # We want to test here if engine clients are not properly
            # cleaned, or results are not cleaned on re-run()
            #
            # cannot assert on task.iter_retcodes() as we are not sure in
            # what order the interpreter will proceed
            #self.assertEqual(len(list(task.iter_retcodes())), 1)
            self.assertEqual(len(list(task.iter_buffers())), 1)
            # hard to test without really checking the number of clients of engine
            self.assertEqual(len(task._engine._clients), 0)
            task.run("echo newrun")
            self.assertEqual(len(task._engine._clients), 0)
            self.assertEqual(len(list(task.iter_retcodes())), 1)
            self.assertEqual(len(list(task.iter_buffers())), 1)
            self.assertEqual(str(list(task.iter_buffers())[0][0]), "newrun")
        self.assertTrue(ok, "KeyboardInterrupt not raised")

    # From old TaskAdvancedTest.py:

    def testTaskRun(self):
        wrk = task_self().shell("true")
        task_self().run()

    def testTaskRunTimeout(self):
        wrk = task_self().shell("sleep 1")
        self.assertRaises(TimeoutError, task_self().run, 0.3)

        wrk = task_self().shell("sleep 1")
        self.assertRaises(TimeoutError, task_self().run, timeout=0.3)

    def testTaskShellRunLocal(self):
        wrk = task_self().run("false")
        self.assertTrue(wrk)
        self.assertEqual(task_self().max_retcode(), 1)

        # Timeout in shell() fashion way.
        wrk = task_self().run("sleep 1", timeout=0.3)
        self.assertTrue(wrk)
        self.assertEqual(task_self().num_timeout(), 1)

    def testTaskEngineUserSelection(self):
        task_terminate()
        try:
            DEFAULTS.engine = 'select'
            self.assertEqual(task_self().info('engine'), 'select')
            task_terminate()
        finally:
            DEFAULTS.engine = 'auto'

    def testTaskEngineWrongUserSelection(self):
        try:
            task_terminate()
            DEFAULTS.engine = 'foobar'
            # Check for KeyError in case of wrong engine request
            self.assertRaises(KeyError, task_self)
        finally:
            DEFAULTS.engine = 'auto'

        task_terminate()

    def testTaskNewThread1(self):
        # create a task in a new thread
        task = Task()
        self.assert_(task != None)

        match = "test"

        # schedule a command in that task
        worker = task.shell("/bin/echo %s" % match)

        # run this task
        task.resume()

        # wait for the task to complete
        task_wait()

        # verify that the worker has completed
        self.assertEqual(worker.read(), match)

        # stop task
        task.abort()

    def testTaskInNewThread2(self):
        # create a task in a new thread
        task = Task()
        self.assert_(task != None)

        match = "again"

        # schedule a command in that task
        worker = task.shell("/bin/echo %s" % match)

        # run this task
        task.resume()

        # wait for the task to complete
        task_wait()

        # verify that the worker has completed
        self.assertEqual(worker.read(), match)

        # stop task
        task.abort()

    def testTaskInNewThread3(self):
        # create a task in a new thread
        task = Task()
        self.assert_(task != None)

        match = "once again"

        # schedule a command in that task
        worker = task.shell("/bin/echo %s" % match)

        # run this task
        task.resume()

        # wait for the task to complete
        task_wait()

        # verify that the worker has completed
        self.assertEqual(worker.read(), match)

        # stop task
        task.abort()

    def testLocalPickupHup(self):

        class PickupHupCounter(EventHandler):
            def __init__(self):
                self.pickup_count = 0
                self.hup_count = 0
            def ev_pickup(self, worker):
                self.pickup_count += 1
            def ev_hup(self, worker):
                self.hup_count += 1

        task = task_self()
        fanout = task.info("fanout")
        try:
            task.set_info("fanout", 3)

            # Test #1: simple
            chdlr = PickupHupCounter()
            for i in range(0, 10):
                task.shell("/bin/echo test %d" % i, handler=chdlr)
            task.resume()
            self.assertEqual(chdlr.pickup_count, 10)
            self.assertEqual(chdlr.hup_count, 10)

            # Test #2: fanout change during run
            chdlr = PickupHupCounter()
            class TestFanoutChanger(EventHandler):
                def ev_timer(self, timer):
                    task_self().set_info("fanout", 1)
            timer = task.timer(2.0, handler=TestFanoutChanger())
            for i in range(0, 10):
                task.shell("sleep 0.5", handler=chdlr)
            task.resume()
            self.assertEqual(chdlr.pickup_count, 10)
            self.assertEqual(chdlr.hup_count, 10)
        finally:
            # restore original fanout value
            task.set_info("fanout", fanout)
