#!/usr/bin/python

import timeit

setup = '''

a = 'cluster[1-10]'

'''

print min(timeit.Timer('if a.find("%") >= 0: a = a.replace("%", "%%")', setup=setup).repeat(3, 1000))
print min(timeit.Timer('a = a.replace("%", "%%")', setup=setup).repeat(3, 1000))
