#!/bin/python3

# To be able to use this script with python2 and python3 the following
# is needed (just for the last line: print('%s' % i, end=' '))
from __future__ import print_function

import sys


def topological_sort(source):
    # https://stackoverflow.com/questions/11557241/python-sorting-a-dependency-list
    """perform topo sort on elements.

    :arg source: list of ``(name, [list of dependancies])`` pairs
    :returns: list of names, with dependancies listed first
    """
    # copy deps so we can modify set in-place
    pending = [(name, set(deps)) for name, deps in source]
    emitted = []
    while pending:
        next_pending = []
        next_emitted = []
        for entry in pending:
            name, deps = entry
            # remove deps we emitted last pass
            deps.difference_update(emitted)
            if deps:
                # still has deps? recheck during next pass
                next_pending.append(entry)
            else:
                # no more deps? time to emit
                yield name
                # <-- not required, but helps preserve original ordering
                emitted.append(name)
                # remember what we emitted for difference_update() in next pass
                next_emitted.append(name)
        if not next_emitted:
            # all entries have unmet deps, one of two things is wrong...
            raise ValueError(
                "cyclic or missing dependancy detected: %r" %
                (next_pending,))
        pending = next_pending
        emitted = next_emitted


spec_dict = {}
dependency = {}

if len(sys.argv) != 2:
    print("Need dependency file as input")
    sys.exit(1)


for line in open(sys.argv[1]):
    line = line.rstrip().split(':')
    # The spec_dict is later used to translate
    # package names into spec files
    spec_dict[line[1]] = line[0]
    # Ignore the meta_packages
    if line[1] == 'meta-packages':
        continue
    # Ignore non ohpc (Build)Requires
    if line[2] == 'NA':
        continue
    # Ignore kernel modules
    if line[2].startswith('kmod'):
        continue
    # Ignore the nagios_plugins
    if ((line[2].startswith('nagios-plugins')) and
            (not line[2].startswith('nagios-plugins-ohpc'))):
        continue
    # This tries to filter out versions with a "." or _isa with a "("
    if "." in line[2] or "(" in line[2]:
        continue
    if line[0] in dependency:
        if line[2] not in dependency[line[0]]:
            dependency[line[0]].append(line[2])
    else:
        dependency[line[0]] = [line[2]]

additional = {}

# Add entries to the dict which have no dependencies
for v in dependency.values():
    for value in v:
        try:
            if spec_dict[value] not in dependency.keys():
               additional[spec_dict[value]] = []
        except KeyError as err:
            # Handle python_prefix rpm macro
            if '-numpy-' in value:
                spec_dict[value] = 'python-numpy.spec'
            elif '-Cython-' in value:
                spec_dict[value] = 'python-Cython.spec'
            elif '-scipy-' in value:
                spec_dict[value] = 'python-scipy.spec'
            elif '-mpi4py-' in value:
                spec_dict[value] = 'python-mpi4py.spec'
            else:
                print(err)

dependency.update(additional)

# Translate everything to spec file-names
for k, v in dependency.items():
    for i in range(len(v)):
        v[i] = spec_dict[v[i]]
    # remove cylic dependencies
    dependency[k] = [x for x in v if x != k]


# make a list of the dict
dep_list = [(k, set(v)) for (k, v) in dependency.items()]

# Sort and print
for i in topological_sort(dep_list):
    print('%s' % i, end=' ')
