#!/usr/bin/env python
# ClusterShell.Worker.ExecWorker test suite
# First version by A. Degremont 2014-07-10

import os
import unittest

from TLib import HOSTNAME, make_temp_file, make_temp_filename, make_temp_dir

from ClusterShell.Worker.Exec import ExecWorker, WorkerError
from ClusterShell.Task import task_self

class ExecTest(unittest.TestCase):

    def execw(self, **kwargs):
        """helper method to spawn and run ExecWorker"""
        worker = ExecWorker(**kwargs)
        task_self().schedule(worker)
        task_self().run()
        return worker

    def test_no_nodes(self):
        """test ExecWorker with a simple command without nodes"""
        self.execw(nodes=None, handler=None, command="echo ok")
        self.assertEqual(task_self().max_retcode(), None)

    def test_shell_syntax(self):
        """test ExecWorker with a command using shell syntax"""
        cmd = "echo -n 1; echo -n 2"
        self.execw(nodes='localhost', handler=None, command=cmd)
        self.assertEqual(task_self().max_retcode(), 0)
        self.assertEqual(task_self().node_buffer('localhost'), '12')

    def test_one_node(self):
        """test ExecWorker with a simple command on localhost"""
        self.execw(nodes='localhost', handler=None, command="echo ok")
        self.assertEqual(task_self().max_retcode(), 0)
        self.assertEqual(task_self().node_buffer('localhost'), 'ok')

    def test_one_node_error(self):
        """test ExecWorker with an error command on localhost"""
        self.execw(nodes='localhost', handler=None, command="false")
        self.assertEqual(task_self().max_retcode(), 1)
        self.assertEqual(task_self().node_buffer('localhost'), '')

    def test_timeout(self):
        """test ExecWorker with a timeout"""
        nodes = "localhost,%s" % HOSTNAME
        self.execw(nodes=nodes, handler=None, command="sleep 1", timeout=0.2)
        self.assertEqual(task_self().max_retcode(), None)
        self.assertEqual(task_self().num_timeout(), 2)

    def test_node_placeholder(self):
        """test ExecWorker with several nodes and %h (host)"""
        nodes = "localhost,%s" % HOSTNAME
        self.execw(nodes=nodes, handler=None, command="echo %h")
        self.assertEqual(task_self().max_retcode(), 0)
        self.assertEqual(task_self().node_buffer('localhost'), 'localhost')
        self.assertEqual(task_self().node_buffer(HOSTNAME), HOSTNAME)

    def test_bad_placeholder(self):
        """test ExecWorker with unknown placeholder pattern"""
        self.assertRaises(WorkerError, self.execw,
                          nodes="localhost", handler=None, command="echo %x")

    def test_rank_placeholder(self):
        """test ExecWorker with several nodes and %n (rank)"""
        nodes = "localhost,%s" % HOSTNAME
        self.execw(nodes=nodes, handler=None, command="echo %n")
        self.assertEqual(task_self().max_retcode(), 0)
        self.assertEqual([str(msg) for msg, _ in task_self().iter_buffers()],
                         ['0', '1'])

    def test_copy(self):
        """test copying with an ExecWorker and host placeholder"""
        src = make_temp_file("data")
        dstdir = make_temp_dir()
        dstpath = os.path.join(dstdir, os.path.basename(src.name))
        try:
            pattern = dstpath + ".%h"
            self.execw(nodes='localhost', handler=None, source=src.name,
                       dest=pattern)
            self.assertEqual(task_self().max_retcode(), 0)
            self.assertTrue(os.path.isfile(dstpath + '.localhost'))
        finally:
            os.unlink(dstpath + '.localhost')
            os.rmdir(dstdir)

    def test_copy_preserve(self):
        """test copying with an ExecWorker (preserve=True)"""
        src = make_temp_file("data")
        past_time = 443757600
        os.utime(src.name, (past_time, past_time))
        dstpath = make_temp_filename()
        try:
            self.execw(nodes='localhost', handler=None, source=src.name,
                       dest=dstpath, preserve=True)
            self.assertEqual(task_self().max_retcode(), 0)
            self.assertTrue(os.stat(dstpath).st_mtime, past_time)
        finally:
            os.unlink(dstpath)

    def test_copy_directory(self):
        """test copying directory with an ExecWorker"""
        srcdir = make_temp_dir()
        dstdir = make_temp_dir()
        ref1 = make_temp_file("data1", dir=srcdir)
        pathdstsrcdir = os.path.join(dstdir, os.path.basename(srcdir))
        pathdst1 = os.path.join(pathdstsrcdir, os.path.basename(ref1.name))
        try:
            self.execw(nodes='localhost', handler=None, source=srcdir,
                       dest=dstdir)
            self.assertEqual(task_self().max_retcode(), 0)
            self.assertTrue(os.path.isdir(pathdstsrcdir))
            self.assertTrue(os.path.isfile(pathdst1))
            self.assertEqual(open(pathdst1).readlines()[0], "data1")
        finally:
            os.unlink(pathdst1)
            os.rmdir(pathdstsrcdir)
            del ref1
            os.rmdir(dstdir)
            os.rmdir(srcdir)

    def test_copy_wrong_directory(self):
        """test copying wrong directory with an ExecWorker"""
        srcdir = make_temp_dir()
        dst = make_temp_file("data")
        ref1 = make_temp_file("data1", dir=srcdir)
        try:
            self.execw(nodes='localhost', handler=None, source=srcdir,
                       dest=dst.name, stderr=True)
            self.assertEqual(task_self().max_retcode(), 1)
            self.assertTrue(len(task_self().node_error("localhost")) > 0)
            self.assertTrue(os.path.isfile(ref1.name))
        finally:
            del ref1
            os.rmdir(srcdir)

    def test_rcopy_wrong_directory(self):
        """test ExecWorker reverse copying with wrong directory"""
        dstbasedir = make_temp_dir()
        dstdir = os.path.join(dstbasedir, "wrong")
        src = make_temp_file("data")
        try:
            self.assertRaises(ValueError, self.execw, nodes='localhost',
                              handler=None, source=src.name, dest=dstdir,
                              stderr=True, reverse=True)
        finally:
            os.rmdir(dstbasedir)
