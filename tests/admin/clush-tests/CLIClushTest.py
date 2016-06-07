#!/usr/bin/env python
# scripts/clush.py tool test suite
# Written by S. Thiell


"""Unit test for CLI/Clush.py"""

import errno
import os
import pwd
import re
import signal
import sys
import tempfile
import threading
import time
import unittest

from subprocess import Popen, PIPE

from TLib import *
import ClusterShell.CLI.Clush
from ClusterShell.CLI.Clush import main
from ClusterShell.NodeSet import NodeSet
from ClusterShell.Task import task_cleanup


class CLIClushTest_A(unittest.TestCase):
    """Unit test class for testing CLI/Clush.py"""

    def tearDown(self):
        """cleanup all tasks"""
        task_cleanup()

    def _clush_t(self, args, stdin, expected_stdout, expected_rc=0,
                  expected_stderr=None):
        """This new version allows code coverage checking by calling clush's
        main entry point."""
        def raw_input_mock(prompt):
            # trusty sleep
            wait_time = 60
            start = time.time()
            while (time.time() - start < wait_time):
                time.sleep(wait_time - (time.time() - start))
            return ""
        ClusterShell.CLI.Clush.raw_input = raw_input_mock
        try:
            CLI_main(self, main, [ 'clush' ] + args, stdin, expected_stdout,
                     expected_rc, expected_stderr)
        finally:
            ClusterShell.CLI.Clush.raw_input = raw_input

    def test_000_display(self):
        """test clush (display options)"""
        self._clush_t(["-w", HOSTNAME, "true"], None, "")
        self._clush_t(["-w", HOSTNAME, "echo", "ok"], None,
                      "%s: ok\n" % HOSTNAME)
        self._clush_t(["-w", HOSTNAME, "echo", "ok", "ok"], None,
                      "%s: ok ok\n" % HOSTNAME)
        self._clush_t(["-N", "-w", HOSTNAME, "echo", "ok", "ok"], None,
                      "ok ok\n")
        self._clush_t(["-w", "badhost,%s" % HOSTNAME, "-x", "badhost", "echo",
                       "ok"], None, "%s: ok\n" % HOSTNAME)
        self._clush_t(["-qw", HOSTNAME, "echo", "ok"], None,
                      "%s: ok\n" % HOSTNAME)
        self._clush_t(["-vw", HOSTNAME, "echo", "ok"], None,
                      "%s: ok\n" % HOSTNAME)
        self._clush_t(["-qvw", HOSTNAME, "echo", "ok"], None,
                      "%s: ok\n" % HOSTNAME)
        self._clush_t(["-Sw", HOSTNAME, "echo", "ok"], None,
                      "%s: ok\n" % HOSTNAME)
        self._clush_t(["-Sqw", HOSTNAME, "echo", "ok"], None,
                      "%s: ok\n" % HOSTNAME)
        self._clush_t(["-Svw", HOSTNAME, "echo", "ok"], None,
                      "%s: ok\n" % HOSTNAME)
        self._clush_t(["--nostdin", "-w", HOSTNAME, "echo", "ok"], None,
                      "%s: ok\n" % HOSTNAME)
        self._clush_t(["-w", HOSTNAME, "--color=always", "echo", "ok"], None,
                      "\x1b[34m%s: \x1b[0mok\n" % HOSTNAME)
        self._clush_t(["-w", HOSTNAME, "--color=never", "echo", "ok"], None,
                      "%s: ok\n" % HOSTNAME)

    def test_001_display_tty(self):
        """test clush (display options) [tty]"""
        setattr(ClusterShell.CLI.Clush, '_f_user_interaction', True)
        try:
            self.test_000_display()
        finally:
            delattr(ClusterShell.CLI.Clush, '_f_user_interaction')

    def test_002_fanout(self):
        """test clush (fanout)"""
        self._clush_t(["-f", "10", "-w", HOSTNAME, "true"], None, "")
        self._clush_t(["-f", "1", "-w", HOSTNAME, "true"], None, "")
        self._clush_t(["-f", "1", "-w", HOSTNAME, "echo", "ok"], None, \
            "%s: ok\n" % HOSTNAME)

    def test_003_fanout_tty(self):
        """test clush (fanout) [tty]"""
        setattr(ClusterShell.CLI.Clush, '_f_user_interaction', True)
        try:
            self.test_002_fanout()
        finally:
            delattr(ClusterShell.CLI.Clush, '_f_user_interaction')

    def test_004_ssh_options(self):
        """test clush (ssh options)"""
        self._clush_t(["-o", "-oStrictHostKeyChecking=no", "-w", HOSTNAME,
                       "echo", "ok"], None, "%s: ok\n" % HOSTNAME)
        self._clush_t(["-o", "-oStrictHostKeyChecking=no -oForwardX11=no",
                       "-w", HOSTNAME, "echo", "ok"], None,
                      "%s: ok\n" % HOSTNAME)
        self._clush_t(["-o", "-oStrictHostKeyChecking=no", "-o",
                       "-oForwardX11=no", "-w", HOSTNAME, "echo", "ok"], None,
                      "%s: ok\n" % HOSTNAME)
        self._clush_t(["-o-oStrictHostKeyChecking=no", "-o-oForwardX11=no",
                       "-w", HOSTNAME, "echo", "ok"], None,
                      "%s: ok\n" % HOSTNAME)
        self._clush_t(["-u", "30", "-w", HOSTNAME, "echo", "ok"], None,
                      "%s: ok\n" % HOSTNAME)
        self._clush_t(["-t", "30", "-u", "30", "-w", HOSTNAME, "echo", "ok"],
                      None, "%s: ok\n" % HOSTNAME)

    def test_005_ssh_options_tty(self):
        """test clush (ssh options) [tty]"""
        setattr(ClusterShell.CLI.Clush, '_f_user_interaction', True)
        try:
            self.test_004_ssh_options()
        finally:
            delattr(ClusterShell.CLI.Clush, '_f_user_interaction')

    def test_006_output_gathering(self):
        """test clush (output gathering)"""
        self._clush_t(["-w", HOSTNAME, "-L", "echo", "ok"], None, \
            "%s: ok\n" % HOSTNAME)
        self._clush_t(["-w", HOSTNAME, "-bL", "echo", "ok"], None, \
            "%s: ok\n" % HOSTNAME)
        self._clush_t(["-w", HOSTNAME, "-qbL", "echo", "ok"], None, \
            "%s: ok\n" % HOSTNAME)
        self._clush_t(["-w", HOSTNAME, "-BL", "echo", "ok"], None, \
            "%s: ok\n" % HOSTNAME)
        self._clush_t(["-w", HOSTNAME, "-qBL", "echo", "ok"], None, \
            "%s: ok\n" % HOSTNAME)
        self._clush_t(["-w", HOSTNAME, "-BLS", "echo", "ok"], None, \
            "%s: ok\n" % HOSTNAME)
        self._clush_t(["-w", HOSTNAME, "-qBLS", "echo", "ok"], None, \
            "%s: ok\n" % HOSTNAME)
        self._clush_t(["-w", HOSTNAME, "-vb", "echo", "ok"], None,
                      "%s: ok\n---------------\n%s\n---------------\nok\n"
                      % (HOSTNAME, HOSTNAME))

    def test_007_output_gathering_tty(self):
        """test clush (output gathering) [tty]"""
        setattr(ClusterShell.CLI.Clush, '_f_user_interaction', True)
        try:
            self.test_006_output_gathering()
        finally:
            delattr(ClusterShell.CLI.Clush, '_f_user_interaction')

    def test_008_file_copy(self):
        """test clush (file copy)"""
        content = "%f" % time.time()
        f = make_temp_file(content)
        self._clush_t(["-w", HOSTNAME, "-c", f.name], None, "")
        f.seek(0)
        self.assertEqual(f.read(), content)
        # test --dest option
        f2 = tempfile.NamedTemporaryFile()
        self._clush_t(["-w", HOSTNAME, "-c", f.name, "--dest", f2.name], \
            None, "")
        f2.seek(0)
        self.assertEqual(f2.read(), content)
        # test --user option
        f2 = tempfile.NamedTemporaryFile()
        self._clush_t(["--user", pwd.getpwuid(os.getuid())[0], "-w", \
            HOSTNAME, "--copy", f.name, "--dest", f2.name], None, "")
        f2.seek(0)
        self.assertEqual(f2.read(), content)
        # test --rcopy
        self._clush_t(["--user", pwd.getpwuid(os.getuid())[0], "-w", \
            HOSTNAME, "--rcopy", f.name, "--dest", \
                os.path.dirname(f.name)], None, "")
        f2.seek(0)
        self.assertEqual(open("%s.%s" % (f.name, HOSTNAME)).read(), content)

    def test_009_file_copy_tty(self):
        """test clush (file copy) [tty]"""
        setattr(ClusterShell.CLI.Clush, '_f_user_interaction', True)
        try:
            self.test_008_file_copy()
        finally:
            delattr(ClusterShell.CLI.Clush, '_f_user_interaction')

    def test_010_diff(self):
        """test clush (diff)"""
        self._clush_t(["-w", HOSTNAME, "--diff", "echo", "ok"], None, "")
        self._clush_t(["-w", "%s,localhost" % HOSTNAME, "--diff", "echo",
                       "ok"], None, "")

    def test_011_diff_tty(self):
        """test clush (diff) [tty]"""
        setattr(ClusterShell.CLI.Clush, '_f_user_interaction', True)
        try:
            self.test_010_diff()
        finally:
            delattr(ClusterShell.CLI.Clush, '_f_user_interaction')

    def test_012_diff_null(self):
        """test clush (diff w/o output)"""
        self._clush_t(["-w", "%s,localhost" % HOSTNAME, "--diff",
            'echo "${SSH_CLIENT%% *}" | egrep "^(127.0.0.1|::1)$" >/dev/null || echo ok'],
            None, re.compile(r"^--- %s\n\+\+\+ localhost\n@@ -1(,1)? \+[01],0 @@\n-ok\n$" % HOSTNAME))

    def test_013_stdin(self):
        """test clush (stdin)"""
        self._clush_t(["-w", HOSTNAME, "sleep 1 && cat"], "ok",
                      "%s: ok\n" % HOSTNAME)
        self._clush_t(["-w", HOSTNAME, "cat"], "ok\nok", "%s: ok\n%s: ok\n"
                      % (HOSTNAME, HOSTNAME))
        # write binary to stdin
        self._clush_t(["-w", HOSTNAME, "gzip -d"], \
            "1f8b0800869a744f00034bcbcf57484a2ce2020027b4dd1308000000".decode("hex"),
            "%s: foo bar\n" % HOSTNAME)

    def test_015_stderr(self):
        """test clush (stderr)"""
        self._clush_t(["-w", HOSTNAME, "echo err 1>&2"], None, "", 0,
                      "%s: err\n" % HOSTNAME)
        self._clush_t(["-b", "-w", HOSTNAME, "-q", "echo err 1>&2"], None, "",
                      0, "%s: err\n" % HOSTNAME)
        self._clush_t(["-B", "-w", HOSTNAME, "-q", "echo err 1>&2"], None,
            "---------------\n%s\n---------------\nerr\n" % HOSTNAME)

    def test_016_stderr_tty(self):
        """test clush (stderr) [tty]"""
        setattr(ClusterShell.CLI.Clush, '_f_user_interaction', True)
        try:
            self.test_015_stderr()
        finally:
            delattr(ClusterShell.CLI.Clush, '_f_user_interaction')

    def test_017_retcodes(self):
        """test clush (retcodes)"""
        self._clush_t(["-w", HOSTNAME, "/bin/false"], None, "", 0,
            "clush: %s: exited with exit code 1\n" % HOSTNAME)
        self._clush_t(["-w", HOSTNAME, "-b", "/bin/false"], None, "", 0,
            "clush: %s: exited with exit code 1\n" % HOSTNAME)
        self._clush_t(["-S", "-w", HOSTNAME, "/bin/false"], None, "", 1,
            "clush: %s: exited with exit code 1\n" % HOSTNAME)
        for i in (1, 2, 127, 128, 255):
            self._clush_t(["-S", "-w", HOSTNAME, "exit %d" % i], None, "", i, \
                "clush: %s: exited with exit code %d\n" % (HOSTNAME, i))
        self._clush_t(["-v", "-w", HOSTNAME, "/bin/false"], None, "", 0,
            "clush: %s: exited with exit code 1\n" % HOSTNAME)

        duo = str(NodeSet("%s,localhost" % HOSTNAME))
        self._clush_t(["-w", duo, "-b", "/bin/false"], None, "", 0,
            "clush: %s (%d): exited with exit code 1\n" % (duo, 2))
        self._clush_t(["-w", duo, "-b", "-q", "/bin/false"], None, "", 0,
            "clush: %s: exited with exit code 1\n" % duo)
        self._clush_t(["-w", duo, "-S", "-b", "/bin/false"], None, "", 1,
            "clush: %s (%d): exited with exit code 1\n" % (duo, 2))
        self._clush_t(["-w", duo, "-S", "-b", "-q", "/bin/false"], None, "", 1)

    def test_018_retcodes_tty(self):
        """test clush (retcodes) [tty]"""
        setattr(ClusterShell.CLI.Clush, '_f_user_interaction', True)
        try:
            self.test_017_retcodes()
        finally:
            delattr(ClusterShell.CLI.Clush, '_f_user_interaction')

    def test_019_timeout(self):
        """test clush (timeout)"""
        self._clush_t(["-w", HOSTNAME, "-u", "1", "sleep 3"], None,
                       "", 0, "clush: %s: command timeout\n" % HOSTNAME)
        self._clush_t(["-w", HOSTNAME, "-u", "1", "-b", "sleep 3"], None,
                       "", 0, "clush: %s: command timeout\n" % HOSTNAME)

    def test_020_timeout_tty(self):
        """test clush (timeout) [tty]"""
        setattr(ClusterShell.CLI.Clush, '_f_user_interaction', True)
        try:
            self.test_019_timeout()
        finally:
            delattr(ClusterShell.CLI.Clush, '_f_user_interaction')

    def test_021_file_copy_timeout(self):
        """test clush file copy (timeout)"""
        content = "%f" % time.time()
        f = make_temp_file(content)
        self._clush_t(["-w", HOSTNAME, "-u", "0.01", "-c", f.name], None,
                       "", 0, "clush: %s: command timeout\n" % HOSTNAME)

    def test_022_file_copy_timeout_tty(self):
        """test clush file copy (timeout) [tty]"""
        setattr(ClusterShell.CLI.Clush, '_f_user_interaction', True)
        try:
            self.test_021_file_copy_timeout()
        finally:
            delattr(ClusterShell.CLI.Clush, '_f_user_interaction')

    def test_023_load_workerclass(self):
        """test _load_workerclass()"""
        for name in ('rsh', 'ssh', 'pdsh'):
            cls = ClusterShell.CLI.Clush._load_workerclass(name)
            self.assertTrue(cls)

    def test_024_load_workerclass_error(self):
        """test _load_workerclass() bad use cases"""
        func = ClusterShell.CLI.Clush._load_workerclass
        # Bad module
        self.assertRaises(ImportError, func, 'not_a_module')
        # Worker module but not supported
        self.assertRaises(AttributeError, func, 'worker')

    def test_025_worker(self):
        """test clush (worker)"""
        self._clush_t(["-w", HOSTNAME, "--worker=ssh", "echo ok"], None,
                       "%s: ok\n" % HOSTNAME, 0)
        self._clush_t(["-w", HOSTNAME, "-R", "ssh", "echo ok"], None,
                       "%s: ok\n" % HOSTNAME, 0)
        # also test in debug mode...
        self._clush_t(["-w", HOSTNAME, "--worker=exec", "-d", "echo ok"], None,
                      re.compile(r'EXECCLIENT: echo ok\n%s: ok\n%s: ok\n'
                      % (HOSTNAME, HOSTNAME)), 0)
        self._clush_t(["-w", HOSTNAME, "-R", "exec", "-d", "echo ok"], None,
                      re.compile(r'EXECCLIENT: echo ok\n%s: ok\n%s: ok\n'
                      % (HOSTNAME, HOSTNAME)), 0)

    def test_026_keyboard_interrupt(self):
        """test clush on keyboard interrupt"""
        # Note: the scope of this test is still limited as we cannot force user
        # interaction (as clush is launched by subprocess). For replicated
        # observation, we use --nostdin and only check if Keyboard interrupt
        # message is printed...

        class KillerThread(threading.Thread):
            def run(self):
                time.sleep(1)
                # replace later by process.send_signal() [py2.6+]
                os.kill(self.pidkill, signal.SIGINT)

        kth = KillerThread()
        args = ["-w", HOSTNAME, "--worker=exec", "-q", "--nostdin", "-b",
                "echo start; sleep 10"]
        process = Popen(["../scripts/clush.py"] + args, stderr=PIPE,
                        stdout=PIPE, bufsize=0)
        kth.pidkill = process.pid
        kth.start()
        stderr = process.communicate()[1]
        self.assertEqual(stderr, "Keyboard interrupt.\n")

    def test_027_warn_shell_globbing_nodes(self):
        """test clush warning on shell globbing (-w)"""
        tdir = make_temp_dir()
        tfile = open(os.path.join(tdir, HOSTNAME), "w")
        curdir = os.getcwd()
        try:
            os.chdir(tdir)
            self._clush_t(["-w", HOSTNAME, "echo", "ok"], None,
                          "%s: ok\n" % HOSTNAME, 0, "Warning: using '-w %s' "
                          "and local path '%s' exists, was it expanded by "
                          "the shell?\n" % (HOSTNAME, HOSTNAME))
        finally:
            os.chdir(curdir)
            tfile.close()
            os.unlink(tfile.name)
            os.rmdir(tdir)

    def test_028_warn_shell_globbing_exclude(self):
        """test clush warning on shell globbing (-x)"""
        tdir = make_temp_dir()
        tfile = open(os.path.join(tdir, HOSTNAME), "w")
        curdir = os.getcwd()
        try:
            os.chdir(tdir)
            self._clush_t(["-S", "-w", "badhost,%s" % HOSTNAME, "-x", HOSTNAME,
                           "echo", "ok"], None, "", 255,
                          re.compile(r"^Warning: using '-x %s' and local path "
                                     "'%s' exists, was it expanded by the "
                                     "shell\?\n" % (HOSTNAME, HOSTNAME)))
        finally:
            os.chdir(curdir)
            tfile.close()
            os.unlink(tfile.name)
            os.rmdir(tdir)

    def test_029_hostfile(self):
        """test clush --hostfile"""
        f = make_temp_file(HOSTNAME)
        self._clush_t(["--hostfile", f.name, "echo", "ok"], None,
                      "%s: ok\n" % HOSTNAME)
        f2 = make_temp_file(HOSTNAME)
        self._clush_t(["--hostfile", f.name, "--hostfile", f2.name,
                       "echo", "ok"], None, "%s: ok\n" % HOSTNAME)
        self.assertRaises(OSError, self._clush_t,
                          ["--hostfile", "/I/do/NOT/exist", "echo", "ok"],
                          None, 1)

    def test_030_config_options(self):
        """test clush -O/--option"""
        self._clush_t(["--option", "color=never", "-w", HOSTNAME,
                       "echo", "ok"], None, "%s: ok\n" % HOSTNAME)
        self._clush_t(["--option", "color=always", "-w", HOSTNAME,
                       "echo", "ok"], None, "\x1b[34m%s: \x1b[0mok\n" % HOSTNAME)
        self._clush_t(["--option=color=never", "-w", HOSTNAME,
                       "echo", "ok"], None, "%s: ok\n" % HOSTNAME)
        self._clush_t(["--option=color=always", "-w", HOSTNAME,
                       "echo", "ok"], None, "\x1b[34m%s: \x1b[0mok\n" % HOSTNAME)
        self._clush_t(["-O", "fd_max=220", "--option", "color=never",
                       "-w", HOSTNAME, "echo", "ok"], None,
                      "%s: ok\n" % HOSTNAME)
        self._clush_t(["-O", "fd_max=220", "--option", "color=always",
                       "-w", HOSTNAME, "echo", "ok"], None,
                      "\x1b[34m%s: \x1b[0mok\n" % HOSTNAME)
        self._clush_t(["--option", "color=never", "-O", "fd_max=220",
                       "-w", HOSTNAME, "echo", "ok"], None,
                      "%s: ok\n" % HOSTNAME)
        self._clush_t(["--option", "color=always", "-O", "fd_max=220",
                       "-w", HOSTNAME, "echo", "ok"], None,
                      "\x1b[34m%s: \x1b[0mok\n" % HOSTNAME)
        self._clush_t(["--option", "color=never", "-O", "fd_max=220",
                       "-O", "color=always", "-w", HOSTNAME, "echo", "ok"],
                      None, "\x1b[34m%s: \x1b[0mok\n" % HOSTNAME)
        self._clush_t(["--option", "color=always", "-O", "fd_max=220",
                       "-O", "color=never", "-w", HOSTNAME, "echo", "ok"],
                      None, "%s: ok\n" % HOSTNAME)

    def test_031_progress(self):
        """test clush -P/--progress"""
        self._clush_t(["-w", HOSTNAME, "--progress", "echo", "ok"], None,
                      "%s: ok\n" % HOSTNAME)
        self._clush_t(["-w", HOSTNAME, "--progress", "sleep", "2"], None, '', 0,
                      re.compile(r'clush: 0/1\r.*'))
        self._clush_t(["-w", HOSTNAME, "--progress", "sleep", "2"], 'AAAAAAAA',
                      '', 0, re.compile(r'clush: 0/1 write: \d B/s\r.*'))
        self._clush_t(["-w", "%s,localhost" % HOSTNAME, "--progress",
                       "sleep", "2"], 'AAAAAAAAAAAAAA', '', 0,
                       re.compile(r'clush: 0/2 write: \d+ B/s\r.*'))
        self._clush_t(["-w", HOSTNAME, "-b", "--progress", "sleep", "2"],
                      None, '', 0, re.compile(r'clush: 0/1\r.*'))
        self._clush_t(["-w", HOSTNAME, "-b", "--progress", "sleep", "2"],
                      'AAAAAAAAAAAAAAAA', '', 0,
                      re.compile(r'clush: 0/1 write: \d+ B/s\r.*'))
        # -q and --progress: explicit -q wins
        self._clush_t(["-w", HOSTNAME, "--progress", "-q", "sleep", "2"], None,
                      '', 0)
        self._clush_t(["-w", HOSTNAME, "-b", "--progress", "-q", "sleep", "2"],
                      None, '', 0, '')
        self._clush_t(["-w", HOSTNAME, "-b", "--progress", "-q", "sleep", "2"],
                      'AAAAAAAAAAAAAAAA', '', 0, '')
        # cover stderr output and --progress
        self._clush_t(["-w", HOSTNAME, "--progress",
                       "echo foo >&2; echo bar; sleep 2"], None,
                      "%s: bar\n" % HOSTNAME, 0,
                      re.compile(r'%s: foo\nclush: 0/1\r.*'
                                 % HOSTNAME))



class CLIClushTest_B_StdinFailure(unittest.TestCase):
    """Unit test class for testing CLI/Clush.py and stdin failure"""

    def setUp(self):
        class BrokenStdinMock(object):
            def isatty(self):
                return False
            def read(self, bufsize=1024):
                raise IOError(errno.EINVAL, "Invalid argument")

        sys.stdin = BrokenStdinMock()

    def tearDown(self):
        """cleanup all tasks"""
        task_cleanup()
        sys.stdin = sys.__stdin__

    def _clush_t(self, args, stdin, expected_stdout, expected_rc=0,
                  expected_stderr=None):
        CLI_main(self, main, [ 'clush' ] + args, stdin, expected_stdout,
                 expected_rc, expected_stderr)

    def test_022_broken_stdin(self):
        """test clush with broken stdin"""
        self._clush_t(["-w", HOSTNAME, "-v", "sleep 1"], None,
                       "stdin: [Errno 22] Invalid argument\n", 0, "")
