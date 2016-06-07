#!/usr/bin/env python
##
# Copyright 2013-2015 Ghent University
#
# This file is part of EasyBuild,
# originally created by the HPC team of Ghent University (http://ugent.be/hpc/en),
# with support of Ghent University (http://ugent.be/hpc),
# the Flemish Supercomputer Centre (VSC) (https://vscentrum.be/nl/en),
# the Hercules foundation (http://www.herculesstichting.be/in_English)
# and the Department of Economy, Science and Innovation (EWI) (http://www.ewi-vlaanderen.be/en).
#
# http://github.com/hpcugent/easybuild
#
# EasyBuild is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation v2.
#
# EasyBuild is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with EasyBuild.  If not, see <http://www.gnu.org/licenses/>.
##

"""
Bootstrap script for EasyBuild

Installs distribute with included (patched) distribute_setup.py script to obtain easy_install,
and then performs a staged install of EasyBuild:
 * stage 1: install EasyBuild with easy_install to a temporary directory
 * stage 2: install EasyBuild with EasyBuild from stage 1 to a temporary directory
 * stage 3: install EasyBuild with EasyBuild from stage 2 to intended install directory
   (default or $EASYBUILD_INSTALLPATH)

Authors: Kenneth Hoste (UGent), Stijn Deweirdt (UGent)
License: GPLv2

inspired by https://bitbucket.org/pdubroy/pip/raw/tip/getpip.py
(via http://dubroy.com/blog/so-you-want-to-install-a-python-package/)
"""

import copy
import glob
import os
import re
import shutil
import site
import sys
import tempfile
from distutils.version import LooseVersion

PYPI_SOURCE_URL = 'https://pypi.python.org/packages/source'

VSC_BASE = 'vsc-base'
EASYBUILD_PACKAGES = [VSC_BASE, 'easybuild-framework', 'easybuild-easyblocks', 'easybuild-easyconfigs']

# set print_debug to True for detailed progress info
print_debug = os.environ.pop('EASYBUILD_BOOTSTRAP_DEBUG', False)

# don't add user site directory to sys.path (equivalent to python -s), see https://www.python.org/dev/peps/pep-0370/
os.environ['PYTHONNOUSERSITE'] = '1'
site.ENABLE_USER_SITE = False

# clean PYTHONPATH to avoid finding readily installed stuff
os.environ['PYTHONPATH'] = ''

EASYBUILD_BOOTSTRAP_SOURCEPATH = os.environ.pop('EASYBUILD_BOOTSTRAP_SOURCEPATH', None)
EASYBUILD_BOOTSTRAP_SKIP_STAGE0 = os.environ.pop('EASYBUILD_BOOTSTRAP_SKIP_STAGE0', False)

# keep track of original environment (after clearing PYTHONPATH)
orig_os_environ = copy.deepcopy(os.environ)

easybuild_modules_tool = None

#
# Utility functions
#
def debug(msg):
    """Print debug message."""

    if print_debug:
        print "[[DEBUG]]", msg

def info(msg):
    """Print info message."""

    print "[[INFO]]", msg

def error(msg, exit=True):
    """Print error message and exit."""

    print "[[ERROR]]", msg
    sys.exit(1)

def det_lib_path(libdir):
    """Determine relative path of Python library dir."""
    if libdir is None:
        libdir = 'lib'
    pyver = '.'.join([str(x) for x in sys.version_info[:2]])
    return os.path.join(libdir, 'python%s' % pyver, 'site-packages')

def find_egg_dir_for(path, pkg):
    """Find full path of egg dir for given package."""

    for libdir in ['lib', 'lib64']:
        full_libpath = os.path.join(path, det_lib_path(libdir))
        eggdir_regex = re.compile('%s-[0-9a-z.]+-py[0-9.]+.egg' % pkg.replace('-', '_'))
        subdirs = (os.path.exists(full_libpath) and os.listdir(full_libpath)) or []
        for subdir in subdirs:
            if eggdir_regex.match(subdir):
                eggdir = os.path.join(full_libpath, subdir)
                debug("Found egg dir for %s at %s" % (pkg, eggdir))
                return eggdir

    # no egg dir found
    debug("Failed to determine egg dir path for %s in %s (subdirs: %s)" % (pkg, path, subdirs))
    return None

def prep(path):
    """Prepare for installing a Python package in the specified path."""

    debug("Preparing for path %s" % path)

    # restore original environment first
    os.environ = copy.deepcopy(orig_os_environ)
    debug("os.environ['PYTHONPATH'] after reset: %s" % os.environ['PYTHONPATH'])

    # update PATH
    os.environ['PATH'] = os.pathsep.join([os.path.join(path, 'bin')] +
                                         [x for x in os.environ.get('PATH', '').split(os.pathsep) if len(x) > 0])
    # update actual Python search path
    sys.path.insert(0, path)

    # make sure directory exists (this is required by setuptools)
    # usually it's 'lib', but can be 'lib64' as well
    for libdir in ['lib', 'lib64']:
        full_libpath = os.path.join(path, det_lib_path(libdir))
        if not os.path.exists(full_libpath):
            os.makedirs(full_libpath)
        # PYTHONPATH needs to be set as well, otherwise setuptools will fail
        pythonpaths = [x for x in os.environ.get('PYTHONPATH', '').split(os.pathsep) if len(x) > 0]
        os.environ['PYTHONPATH'] = os.pathsep.join([full_libpath] + pythonpaths)

    os.environ['EASYBUILD_MODULES_TOOL'] = easybuild_modules_tool
    debug("$EASYBUILD_MODULES_TOOL set to %s" % os.environ['EASYBUILD_MODULES_TOOL'])

def check_module_command(tmpdir):
    """Check which module command is available, and prepare for using it."""

    # order matters, so we can't use the keys from modules_tools which are unordered
    known_module_commands = ['modulecmd', 'lmod', 'modulecmd.tcl']
    modules_tools = {
        'modulecmd': 'EnvironmentModulesC',
        'lmod': 'Lmod',
        'modulecmd.tcl': 'EnvironmentModulesTcl',
    }
    out = os.path.join(tmpdir, 'module_command.out')
    modtool = None
    for modcmd in known_module_commands:
        cmd = "%s python help" % modcmd
        os.system("%s > %s 2>&1" % (cmd, out))
        modcmd_re = re.compile('module\s.*command\s')
        txt = open(out, "r").read()
        debug("Output from %s: %s" % (cmd, txt))
        if modcmd_re.search(txt):
            modtool = modules_tools[modcmd]
            global easybuild_modules_tool
            easybuild_modules_tool = modtool
            info("Found module command '%s' (%s), so using it." % (modcmd, modtool))
            break

    if modtool is None:
        msg = [
            "Could not find any module command, make sure one available in your $PATH.",
            "Known module commands are checked in order, and include: %s" % ', '.join(known_module_commands),
            "Check the output of 'type module' to determine the location of the module command you are using.",
        ]
        error('\n'.join(msg))

    return modtool

#
# Stage functions
#

def stage0(tmpdir):
    """STAGE 0: Prepare and install distribute via included (patched) distribute_setup.py script."""

    info("\n\n+++ STAGE 0: installing distribute via included (patched) distribute_setup.py...\n\n")

    txt = DISTRIBUTE_SETUP_PY
    if not print_debug:
        # silence distribute_setup.py by redirecting output to /dev/null
        txt = re.sub(r'([^\n]*)(return subprocess.call\(args)(\) == 0)',
                     r"\1f = open(os.devnull, 'w'); \2, stdout=f, stderr=f\3",
                     txt)
        # silence distribute_setup.py completely by setting high log level threshold
        txt = re.sub(r'([^\n]*)(# extracting the tarball[^\n]*)', r'\1log.set_verbosity(1000)\n\1\2', txt)

    # write distribute_setup.py to file (with correct header)
    distribute_setup = os.path.join(tmpdir, 'distribute_setup.py')
    f = open(distribute_setup, "w")
    f.write(txt)
    f.close()

    # create expected directories, set Python search path
    debug("preparing environment...")
    prep(tmpdir)

    import distribute_setup
    debug("distribute_setup.__file__: %s" % distribute_setup.__file__)

    # install easy_install to temporary directory
    from distribute_setup import main as distribute_setup_main
    orig_sys_argv = sys.argv[:]  # make a copy
    sys.argv.append('--prefix=%s' % tmpdir)
    distribute_setup_main()
    sys.argv = orig_sys_argv

    # sanity check
    if os.path.exists(os.path.join(tmpdir, 'bin', 'easy_install')):
        debug("easy_install sanity check OK")
    else:
        error("Installing distribute which should deliver easy_install failed?")

    # prepend distribute egg dir to sys.path, so we know which setuptools we're using
    distribute_egg_dir = find_egg_dir_for(tmpdir, 'distribute')
    if distribute_egg_dir is None:
        error("Failed to determine egg dir path for distribute_egg_dir in %s" % tmpdir)
    else:
        sys.path.insert(0, distribute_egg_dir)

    # make sure we're getting the setuptools we expect
    import setuptools
    if not tmpdir in setuptools.__file__:
        error("Found another setuptools than expected: %s" % setuptools.__file__)
    else:
        debug("Found setuptools in expected path, good!")

    return distribute_egg_dir


def stage1(tmpdir, sourcepath):
    """STAGE 1: temporary install EasyBuild using distribute's easy_install."""

    info("\n\n+++ STAGE 1: installing EasyBuild in temporary dir with easy_install...\n\n")

    # determine locations of source tarballs, if sources path is specified
    source_tarballs = {}
    if sourcepath is not None:
        info("Fetching sources from %s..." % sourcepath)
        for pkg in EASYBUILD_PACKAGES:
            pkg_tarball_glob = os.path.join(sourcepath, '%s*.tar.gz' % pkg)
            pkg_tarball_paths = glob.glob(pkg_tarball_glob)
            if len(pkg_tarball_paths) > 1:
                error("Multiple tarballs found for %s: %s" % (pkg, pkg_tarball_paths))
            elif len(pkg_tarball_paths) == 0:
                if pkg != VSC_BASE:
                    # vsc-base package is not strictly required
                    # it's only a dependency since EasyBuild v2.0;
                    # with EasyBuild v2.0, it will be pulled in from PyPI when installing easybuild-framework
                    error("Missing source tarball: %s" % pkg_tarball_glob)
            else:
                info("Found %s for %s package" % (pkg_tarball_paths[0], pkg))
                source_tarballs.update({pkg: pkg_tarball_paths[0]})

    from setuptools.command import easy_install

    # prepare install dir
    targetdir_stage1 = os.path.join(tmpdir, 'eb_stage1')
    prep(targetdir_stage1)  # set PATH, Python search path

    # install latest EasyBuild with easy_install from PyPi
    cmd = []
    cmd.append('--upgrade')  # make sure the latest version is pulled from PyPi
    cmd.append('--prefix=%s' % targetdir_stage1)

    if source_tarballs:
        # install provided source tarballs (order matters)
        cmd.extend([source_tarballs[pkg] for pkg in EASYBUILD_PACKAGES if pkg in source_tarballs])
    else:
        # install meta-package easybuild from PyPI
        cmd.append('easybuild')

    if not print_debug:
        cmd.insert(0, '--quiet')
    info("installing EasyBuild with 'easy_install %s'" % (' '.join(cmd)))
    easy_install.main(cmd)

    # clear the Python search path, we only want the individual eggs dirs to be in the PYTHONPATH (see below)
    # this is needed to avoid easy-install.pth controlling what Python packages are actually used
    os.environ['PYTHONPATH'] = ''

    # template string to inject in template easyconfig
    templates = {}

    for pkg in EASYBUILD_PACKAGES:
        templates.update({pkg: ''})

        pkg_egg_dir = find_egg_dir_for(targetdir_stage1, pkg)
        if pkg_egg_dir is None:
            if pkg == VSC_BASE:
                # vsc-base is optional in older EasyBuild versions
                continue

        # prepend EasyBuild egg dirs to Python search path, so we know which EasyBuild we're using
        sys.path.insert(0, pkg_egg_dir)
        pythonpaths = [x for x in os.environ.get('PYTHONPATH', '').split(os.pathsep) if len(x) > 0]
        os.environ['PYTHONPATH'] = os.pathsep.join([pkg_egg_dir] + pythonpaths)

        if source_tarballs:
            if pkg in source_tarballs:
                templates.update({pkg: "'%s'," % os.path.basename(source_tarballs[pkg])})
        else:
            # determine per-package versions based on egg dirs, to use them in easyconfig template
            version_regex = re.compile('%s-([0-9a-z.-]*)-py[0-9.]*.egg' % pkg.replace('-', '_'))
            pkg_egg_dirname = os.path.basename(pkg_egg_dir)
            res = version_regex.search(pkg_egg_dirname)
            if res is not None:
                pkg_version = res.group(1)
                debug("Found version for easybuild-%s: %s" % (pkg, pkg_version))
                templates.update({pkg: "'%s-%s.tar.gz'," % (pkg, pkg_version)})
            else:
                tup = (pkg, pkg_egg_dirname, version_regex.pattern)
                error("Failed to determine version for easybuild-%s package from %s with %s" % tup)

    # figure out EasyBuild version via eb command line
    # note: EasyBuild uses some magic to determine the EasyBuild version based on the versions of the individual packages
    pattern = "This is EasyBuild (?P<version>%(v)s) \(framework: %(v)s, easyblocks: %(v)s\)" % {'v': '[0-9.]*[a-z0-9]*'}
    version_re = re.compile(pattern)
    version_out_file = os.path.join(tmpdir, 'eb_version.out')
    eb_version_cmd = 'from easybuild.tools.version import this_is_easybuild; print this_is_easybuild()'
    cmd = "python -c '%s' > %s 2>&1" % (eb_version_cmd, version_out_file)
    debug("Determining EasyBuild version using command '%s'" % cmd)
    os.system(cmd)
    txt = open(version_out_file, "r").read()
    res = version_re.search(txt)
    if res:
        eb_version = res.group(1)
        debug("installing EasyBuild v%s" % eb_version)
    else:
        error("Stage 1 failed, could not determine EasyBuild version (txt: %s)." % txt)

    templates.update({'version': eb_version})

    # clear PYTHONPATH before we go to stage2
    # PYTHONPATH doesn't need to (and shouldn't) include the stage1 egg dirs
    os.environ['PYTHONPATH'] = ''

    # make sure we're getting the expected EasyBuild packages
    import easybuild.framework
    if not tmpdir in easybuild.framework.__file__:
        error("Found another easybuild-framework than expected: %s" % easybuild.framework.__file__)
    else:
        debug("Found easybuild-framework in expected path, good!")

    import easybuild.easyblocks
    if not tmpdir in easybuild.easyblocks.__file__:
        error("Found another easybuild-easyblocks than expected: %s" % easybuild.easyblocks.__file__)
    else:
        debug("Found easybuild-easyblocks in expected path, good!")

    debug("templates: %s" % templates)
    return templates

def stage2(tmpdir, templates, install_path, distribute_egg_dir, sourcepath):
    """STAGE 2: install EasyBuild to temporary dir with EasyBuild from stage 1."""

    info("\n\n+++ STAGE 2: installing EasyBuild in %s with EasyBuild from stage 1...\n\n" % install_path)

    # inject path to distribute installed in stage 1 into $PYTHONPATH via preinstallopts
    # other approaches are not reliable, since EasyBuildMeta easyblock unsets $PYTHONPATH
    if distribute_egg_dir is None:
        preinstallopts = ''
    else:
        preinstallopts = 'PYTHONPATH=%s:$PYTHONPATH' % distribute_egg_dir
    templates.update({
        'preinstallopts': preinstallopts,
    })

    # create easyconfig file
    ebfile = os.path.join(tmpdir, 'EasyBuild-%s.eb' % templates['version'])
    f = open(ebfile, "w")
    templates.update({
        'source_urls': '\n'.join(["'%s/%s/%s'," % (PYPI_SOURCE_URL, pkg[0], pkg) for pkg in EASYBUILD_PACKAGES]),
        'sources': "%(vsc-base)s%(easybuild-framework)s%(easybuild-easyblocks)s%(easybuild-easyconfigs)s" % templates,
        'pythonpath': distribute_egg_dir,
    })
    f.write(EASYBUILD_EASYCONFIG_TEMPLATE % templates)
    f.close()

    # unset $MODULEPATH, we don't care about already installed modules
    os.environ['MODULEPATH'] = ''

    # set command line arguments for eb
    eb_args = ['eb', ebfile, '--allow-modules-tool-mismatch']
    if print_debug:
        eb_args.extend(['--debug', '--logtostdout'])

    # make sure we don't leave any stuff behind in default path $HOME/.local/easybuild
    # and set build and install path explicitely
    if LooseVersion(templates['version']) < LooseVersion('1.3.0'):
        os.environ['EASYBUILD_PREFIX'] = tmpdir
        os.environ['EASYBUILD_BUILDPATH'] = tmpdir
        if install_path is not None:
            os.environ['EASYBUILD_INSTALLPATH'] = install_path
    else:
        # only for v1.3 and up
        eb_args.append('--prefix=%s' % tmpdir)
        eb_args.append('--buildpath=%s' % tmpdir)
        if install_path is not None:
            eb_args.append('--installpath=%s' % install_path)
        if sourcepath is not None:
            eb_args.append('--sourcepath=%s' % sourcepath)

    # make sure parent modules path already exists (Lmod trips over a non-existing entry in $MODULEPATH)
    if install_path is not None:
        modules_path = os.path.join(install_path, 'modules', 'all')
        if not os.path.exists(modules_path):
            os.makedirs(modules_path)
        debug("Created path %s" % modules_path)

    debug("Running EasyBuild with arguments '%s'" % ' '.join(eb_args))
    sys.argv = eb_args

    # install EasyBuild with EasyBuild
    from easybuild.main import main as easybuild_main
    easybuild_main()

def main():
    """Main script: bootstrap EasyBuild in stages."""

    # disallow running as root, since stage 2 will fail
    if os.getuid() == 0:
        error("Don't run the EasyBuild bootstrap script as root, "
              "since stage 2 (installing EasyBuild with EasyBuild) will fail.")

    # see if an install dir was specified
    if not len(sys.argv) == 2:
        error("Usage: %s <install path>" % sys.argv[0])
    install_path = os.path.abspath(sys.argv[1])

    sourcepath = EASYBUILD_BOOTSTRAP_SOURCEPATH
    if sourcepath is not None:
        info("Fetching sources from %s..." % sourcepath)

    # create temporary dir for temporary installations
    tmpdir = tempfile.mkdtemp()
    debug("Going to use %s as temporary directory" % tmpdir)
    os.chdir(tmpdir)

    # check whether a module command is available, we need that
    modtool = check_module_command(tmpdir)

    # clean sys.path, remove paths that may contain EasyBuild packages or stuff installed with easy_install
    orig_sys_path = sys.path[:]
    sys.path = []
    for path in orig_sys_path:
        include_path = True
        # exclude path if it's potentially an EasyBuild/VSC package, providing the 'easybuild'/'vsc' namespace, resp.
        if any([os.path.exists(os.path.join(path, pkg, '__init__.py')) for pkg in ['easyblocks', 'easybuild', 'vsc']]):
            include_path = False
        # exclude any .egg paths
        if path.endswith('.egg'):
            include_path = False
        # exclude any path that contains an easy-install.pth file
        if os.path.exists(os.path.join(path, 'easy-install.pth')):
            include_path = False

        if include_path:
            sys.path.append(path)
        else:
            debug("Excluding %s from sys.path" % path)

    debug("sys.path after cleaning: %s" % sys.path)

    # install EasyBuild in stages

    # STAGE 0: install distribute, which delivers easy_install
    if EASYBUILD_BOOTSTRAP_SKIP_STAGE0:
        distribute_egg_dir = None
        info("Skipping stage0, using local distribute/setuptools providing easy_install")
    else:
        distribute_egg_dir = stage0(tmpdir)

    # STAGE 1: install EasyBuild using easy_install to tmp dir
    templates = stage1(tmpdir, sourcepath)

    # STAGE 2: install EasyBuild using EasyBuild (to final target installation dir)
    stage2(tmpdir, templates, install_path, distribute_egg_dir, sourcepath)

    # clean up the mess
    debug("Cleaning up %s..." % tmpdir)
    shutil.rmtree(tmpdir)

    info('Done!')

    info('')
    if install_path is not None:
        info('EasyBuild v%s was installed to %s, so make sure your $MODULEPATH includes %s' % \
             (templates['version'], install_path, os.path.join(install_path, 'modules', 'all')))
    else:
        info('EasyBuild v%s was installed to configured install path, make sure your $MODULEPATH is set correctly.' % \
             templates['version'])
        info('(default config => add "$HOME/.local/easybuild/modules/all" in $MODULEPATH)')

    info('')
    info("Run 'module load EasyBuild', and run 'eb --help' to get help on using EasyBuild.")
    info("Set $EASYBUILD_MODULES_TOOL to '%s' to use the same modules tool as was used now." % modtool)
    info('')
    info("By default, EasyBuild will install software to $HOME/.local/easybuild.")
    info("To install software with EasyBuild to %s, make sure $EASYBUILD_INSTALLPATH is set accordingly." % install_path)
    info("See http://easybuild.readthedocs.org/en/latest/Configuration.html for details on configuring EasyBuild.")

# template easyconfig file for EasyBuild
EASYBUILD_EASYCONFIG_TEMPLATE = """
easyblock = 'EB_EasyBuildMeta'

name = 'EasyBuild'
version = '%(version)s'

homepage = 'http://hpcugent.github.com/easybuild/'
description = \"\"\"EasyBuild is a software build and installation framework
written in Python that allows you to install software in a structured,
repeatable and robust way.\"\"\"

toolchain = {'name': 'dummy', 'version': 'dummy'}

source_urls = [%(source_urls)s]
sources = [%(sources)s]

# EasyBuild is a (set of) Python packages, so it depends on Python
# usually, we want to use the system Python, so no actual Python dependency is listed
allow_system_deps = [('Python', SYS_PYTHON_VERSION)]

preinstallopts = '%(preinstallopts)s'
"""

# distribute_setup.py script (https://pypi.python.org/pypi/distribute)
#
# A compressed copy of a patched distribute_setup.py (version 0.6.34), generated like so:
# >>> import base64
# >>> import zlib
# >>> base64.b64encode(zlib.compress(open("distribute_setup.py").read()))
# compressed copy below is for setuptools 0.6c11, after applying patch:
#$ diff -u distribute_setup.py.orig distribute_setup.py
#--- distribute_setup.py.orig    2013-02-07 23:27:01.000000000 +0100
#+++ distribute_setup.py 2013-02-07 23:27:32.000000000 +0100
#@@ -518,6 +518,8 @@
#             log.warn("--user requires Python 2.6 or later")
#             raise SystemExit(1)
#         install_args.append('--user')
#+    if options.prefix_install:
#+        install_args.append('--prefix=%s' % options.prefix_install)
#     return install_args
#
# def _parse_args():
#@@ -529,6 +531,8 @@
#         '--user', dest='user_install', action='store_true', default=False,
#         help='install in user site package (requires Python 2.6 or later)')
#     parser.add_option(
#+        '--prefix', dest='prefix_install', metavar="PATH", help='install in prefix')
#+    parser.add_option(
#         '--download-base', dest='download_base', metavar="URL",
#         default=DEFAULT_URL,
#         help='alternative URL from where to download the distribute package')
DISTRIBUTE_SETUP_PY = """
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
7wKYhF5wByb8D1XCC1U=

""".decode("base64").decode("zlib")

# run main function as body of script
main()
