
"""Unit test small library"""

__all__ = ['HOSTNAME', 'load_cfg', 'chrono', 'make_temp_filename',
           'make_temp_file', 'make_temp_dir', 'CLI_main']

import os
import socket
import sys
import tempfile
import time

from ConfigParser import ConfigParser
from StringIO import StringIO

# Get machine short hostname
HOSTNAME = socket.gethostname().split('.', 1)[0]


def load_cfg(name):
    """Load test configuration file as a new ConfigParser"""
    cfgparser = ConfigParser()
    cfgparser.read([ \
        os.path.expanduser('~/.clustershell/tests/%s' % name),
        '/etc/clustershell/tests/%s' % name])
    return cfgparser

def chrono(func):
    """chrono decorator"""
    def timing(*args):
        start = time.time()
        res = func(*args)
        print "execution time: %f s" % (time.time() - start)
        return res
    return timing

#
# Temp files and directories
#
def make_temp_filename(suffix=''):
    """Return a temporary name for a file."""
    if len(suffix) > 0 and suffix[0] != '-':
        suffix = '-' + suffix
    return (tempfile.mkstemp(suffix, prefix='cs-test-'))[1]

def make_temp_file(text, suffix='', dir=None):
    """Create a temporary file with the provided text."""
    tmp = tempfile.NamedTemporaryFile(prefix='cs-test-',
                                      suffix=suffix, dir=dir)
    tmp.write(text)
    tmp.flush()
    return tmp

def make_temp_dir(suffix=''):
    """Create a temporary directory."""
    if len(suffix) > 0 and suffix[0] != '-':
        suffix = '-' + suffix
    return tempfile.mkdtemp(suffix, prefix='cs-test-')

#
# CLI tests
#
def CLI_main(test, main, args, stdin, expected_stdout, expected_rc=0,
             expected_stderr=None):
    """Generic CLI main() direct calling function that allows code coverage
    checks."""
    rc = -1
    saved_stdin = sys.stdin
    saved_stdout = sys.stdout
    saved_stderr = sys.stderr
    if stdin is not None:
        sys.stdin = StringIO(stdin)
    sys.stdout = out = StringIO()
    sys.stderr = err = StringIO()
    sys.argv = args
    try:
        try:
            main()
        except SystemExit, exc:
            rc = int(str(exc))
    finally:
        sys.stdout = saved_stdout
        sys.stderr = saved_stderr
        sys.stdin = saved_stdin
    if expected_stdout is not None:
        # expected_stdout might be a compiled regexp or a string
        try:
            if not expected_stdout.search(out.getvalue()):
                # search failed; use assertEqual() to display expected/output
                test.assertEqual(out.getvalue(), expected_stdout.pattern)
        except AttributeError:
            # not a regexp
            test.assertEqual(out.getvalue(), expected_stdout)
    out.close()
    if expected_stderr is not None:
        # expected_stderr might be a compiled regexp or a string
        try:
            if not expected_stderr.match(err.getvalue()):
                # match failed; use assertEqual() to display expected/output
                test.assertEqual(err.getvalue(), expected_stderr.pattern)
        except AttributeError:
            # check the end as stderr messages are often prefixed with argv[0]
            test.assertTrue(err.getvalue().endswith(expected_stderr),
                            err.getvalue() + " != " + expected_stderr)
    if expected_rc is not None:
        test.assertEqual(rc, expected_rc, "rc=%d err=%s" % (rc, err.getvalue()))
    err.close()
