#!/usr/bin/python

import timeit

setup = '''
import random

from ClusterShell.NodeSet import NodeSet

a = list(NodeSet("node[0000-1000]"))

random.shuffle(a)
'''

print min(timeit.Timer('ns=NodeSet.fromlist(a)', setup=setup).repeat(3, 100))
print min(timeit.Timer('ns=NodeSet._fromlist1(a)', setup=setup).repeat(3, 100))
