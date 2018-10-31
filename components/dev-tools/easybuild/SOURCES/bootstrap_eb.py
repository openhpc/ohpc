#!/usr/bin/env python
##
# Copyright 2013-2018 Ghent University
#
# This file is part of EasyBuild,
# originally created by the HPC team of Ghent University (http://ugent.be/hpc/en),
# with support of Ghent University (http://ugent.be/hpc),
# the Flemish Supercomputer Centre (VSC) (https://www.vscentrum.be),
# Flemish Research Foundation (FWO) (http://www.fwo.be/en)
# and the Department of Economy, Science and Innovation (EWI) (http://www.ewi-vlaanderen.be/en).
#
# https://github.com/easybuilders/easybuild
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
 * stage 0: install setuptools (which provides easy_install), unless already available
 * stage 1: install EasyBuild with easy_install to a temporary directory
 * stage 2: install EasyBuild with EasyBuild from stage 1 to specified install directory

Authors: Kenneth Hoste (UGent), Stijn Deweirdt (UGent), Ward Poelmans (UGent)
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
import traceback
import urllib2
from distutils.version import LooseVersion
from hashlib import md5


EB_BOOTSTRAP_VERSION = '20180925.01'

# argparse preferrred, optparse deprecated >=2.7
HAVE_ARGPARSE = False
try:
    import argparse
    HAVE_ARGPARSE = True
except ImportError:
    import optparse

PYPI_SOURCE_URL = 'https://pypi.python.org/packages/source'

VSC_BASE = 'vsc-base'
VSC_INSTALL = 'vsc-install'
EASYBUILD_PACKAGES = [VSC_INSTALL, VSC_BASE, 'easybuild-framework', 'easybuild-easyblocks', 'easybuild-easyconfigs']

STAGE1_SUBDIR = 'eb_stage1'

# set print_debug to True for detailed progress info
print_debug = os.environ.pop('EASYBUILD_BOOTSTRAP_DEBUG', False)

# install with --force in stage2?
forced_install = os.environ.pop('EASYBUILD_BOOTSTRAP_FORCED', False)

# don't add user site directory to sys.path (equivalent to python -s), see https://www.python.org/dev/peps/pep-0370/
os.environ['PYTHONNOUSERSITE'] = '1'
site.ENABLE_USER_SITE = False

# clean PYTHONPATH to avoid finding readily installed stuff
os.environ['PYTHONPATH'] = ''

EASYBUILD_BOOTSTRAP_SOURCEPATH = os.environ.pop('EASYBUILD_BOOTSTRAP_SOURCEPATH', None)
EASYBUILD_BOOTSTRAP_SKIP_STAGE0 = os.environ.pop('EASYBUILD_BOOTSTRAP_SKIP_STAGE0', False)
EASYBUILD_BOOTSTRAP_FORCE_VERSION = os.environ.pop('EASYBUILD_BOOTSTRAP_FORCE_VERSION', None)

# keep track of original environment (after clearing PYTHONPATH)
orig_os_environ = copy.deepcopy(os.environ)

# If the modules tool is specified, use it
easybuild_modules_tool = os.environ.get('EASYBUILD_MODULES_TOOL', None)
easybuild_module_syntax = os.environ.get('EASYBUILD_MODULE_SYNTAX', None)

# If modules subdir specifications are defined, use them
easybuild_installpath_modules = os.environ.get('EASYBUILD_INSTALLPATH_MODULES', None)
easybuild_subdir_modules = os.environ.get('EASYBUILD_SUBDIR_MODULES', 'modules')
easybuild_suffix_modules_path = os.environ.get('EASYBUILD_SUFFIX_MODULES_PATH', 'all')

#
# Utility functions
#
def debug(msg):
    """Print debug message."""

    if print_debug:
        print("[[DEBUG]] " + msg)


def info(msg):
    """Print info message."""

    print("[[INFO]] " + msg)


def error(msg, exit=True):
    """Print error message and exit."""

    print("[[ERROR]] " + msg)
    sys.exit(1)


def mock_stdout_stderr():
    """Mock stdout/stderr channels"""
    # cStringIO is only available in Python 2
    from cStringIO import StringIO
    orig_stdout, orig_stderr = sys.stdout, sys.stderr
    sys.stdout.flush()
    sys.stdout = StringIO()
    sys.stderr.flush()
    sys.stderr = StringIO()

    return orig_stdout, orig_stderr


def restore_stdout_stderr(orig_stdout, orig_stderr):
    """Restore stdout/stderr channels after mocking"""
    # collect output
    sys.stdout.flush()
    stdout = sys.stdout.getvalue()
    sys.stderr.flush()
    stderr = sys.stderr.getvalue()

    # restore original stdout/stderr
    sys.stdout = orig_stdout
    sys.stderr = orig_stderr

    return stdout, stderr


def det_lib_path(libdir):
    """Determine relative path of Python library dir."""
    if libdir is None:
        libdir = 'lib'
    pyver = '.'.join([str(x) for x in sys.version_info[:2]])
    return os.path.join(libdir, 'python%s' % pyver, 'site-packages')


def det_modules_path(install_path):
    """Determine modules path."""
    if easybuild_installpath_modules is not None:
        modules_path = os.path.join(easybuild_installpath_modules, easybuild_suffix_modules_path)
    else:
        modules_path = os.path.join(install_path, easybuild_subdir_modules, easybuild_suffix_modules_path)

    return modules_path


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
    debug("$PATH: %s" % os.environ['PATH'])

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

    debug("$PYTHONPATH: %s" % os.environ['PYTHONPATH'])

    os.environ['EASYBUILD_MODULES_TOOL'] = easybuild_modules_tool
    debug("$EASYBUILD_MODULES_TOOL set to %s" % os.environ['EASYBUILD_MODULES_TOOL'])

    if easybuild_module_syntax:
        # if module syntax is specified, use it
        os.environ['EASYBUILD_MODULE_SYNTAX'] = easybuild_module_syntax
        debug("Using specified module syntax: %s" % os.environ['EASYBUILD_MODULE_SYNTAX'])
    elif easybuild_modules_tool != 'Lmod':
        # Lua is the default module syntax, but that requires Lmod
        # if Lmod is not being used, use Tcl module syntax
        os.environ['EASYBUILD_MODULE_SYNTAX'] = 'Tcl'
        debug("$EASYBUILD_MODULE_SYNTAX set to %s" % os.environ['EASYBUILD_MODULE_SYNTAX'])



def check_module_command(tmpdir):
    """Check which module command is available, and prepare for using it."""
    global easybuild_modules_tool

    if easybuild_modules_tool is not None:
        info("Using modules tool specified by $EASYBUILD_MODULES_TOOL: %s" % easybuild_modules_tool)
        return easybuild_modules_tool

    def check_cmd_help(modcmd):
        """Check 'help' output for specified command."""
        modcmd_re = re.compile(r'module\s.*command')
        cmd = "%s python help" % modcmd
        os.system("%s > %s 2>&1" % (cmd, out))
        txt = open(out, 'r').read()
        debug("Output from %s: %s" % (cmd, txt))
        return modcmd_re.search(txt)

    def is_modulecmd_tcl_modulestcl():
        """Determine if modulecmd.tcl is EnvironmentModulesTcl."""
        modcmd_re = re.compile('Modules Release Tcl')
        cmd = "modulecmd.tcl python --version"
        os.system("%s > %s 2>&1" % (cmd, out))
        txt = open(out, 'r').read()
        debug("Output from %s: %s" % (cmd, txt))
        return modcmd_re.search(txt)

    # order matters, which is why we don't use a dict
    known_module_commands = [
        ('lmod', 'Lmod'),
        ('modulecmd.tcl', 'EnvironmentModules'),
        ('modulecmd', 'EnvironmentModulesC'),
    ]
    out = os.path.join(tmpdir, 'module_command.out')
    modtool = None
    for modcmd, modtool in known_module_commands:
        if check_cmd_help(modcmd):
            # distinguish between EnvironmentModulesTcl and EnvironmentModules
            if modcmd == 'modulecmd.tcl' and is_modulecmd_tcl_modulestcl():
                modtool = 'EnvironmentModulesTcl'
            easybuild_modules_tool = modtool
            info("Found module command '%s' (%s), so using it." % (modcmd, modtool))
            break
        elif modcmd == 'lmod':
            # check value of $LMOD_CMD as fallback
            modcmd = os.environ.get('LMOD_CMD')
            if modcmd and check_cmd_help(modcmd):
                easybuild_modules_tool = modtool
                info("Found module command '%s' via $LMOD_CMD (%s), so using it." % (modcmd, modtool))
                break
        elif modtool == 'EnvironmentModules':
            # check value of $MODULESHOME as fallback
            moduleshome = os.environ.get('MODULESHOME', 'MODULESHOME_NOT_DEFINED')
            modcmd = os.path.join(moduleshome, 'libexec', 'modulecmd.tcl')
            if os.path.exists(modcmd) and check_cmd_help(modcmd):
                easybuild_modules_tool = modtool
                info("Found module command '%s' via $MODULESHOME (%s), so using it." % (modcmd, modtool))
                break

    if easybuild_modules_tool is None:
        mod_cmds = [m for (m, _) in known_module_commands]
        msg = [
            "Could not find any module command, make sure one available in your $PATH.",
            "Known module commands are checked in order, and include: %s" % ', '.join(mod_cmds),
            "Check the output of 'type module' to determine the location of the module command you are using.",
        ]
        error('\n'.join(msg))

    return modtool


def check_setuptools():
    """Check whether a suitable setuptools installation is already available."""

    debug("Checking whether suitable setuptools installation is available...")
    res = None

    _, outfile = tempfile.mkstemp()

    # note: we need to be very careful here, because switching to a different setuptools installation (e.g. in stage0)
    #       after the setuptools module was imported is very tricky...
    #       So, we'll check things by running commands through os.system rather than importing setuptools directly.
    cmd_tmpl = "%s -c '%%s' > %s 2>&1" % (sys.executable, outfile)

    # check setuptools version
    try:
        os.system(cmd_tmpl % "import setuptools; print setuptools.__version__")
        setuptools_version = LooseVersion(open(outfile).read().strip())
        debug("Found setuptools version %s" % setuptools_version)

        min_setuptools_version = '0.6c11'
        if setuptools_version < LooseVersion(min_setuptools_version):
            debug("Minimal setuptools version %s not satisfied, found '%s'" % (min_setuptools_version, setuptools_version))
            res = False
    except Exception as err:
        debug("Failed to check setuptools version: %s" % err)
        res = False

    os.system(cmd_tmpl % "from setuptools.command import easy_install; print easy_install.__file__")
    out = open(outfile).read().strip()
    debug("Location of setuptools' easy_install module: %s" % out)
    if 'setuptools/command/easy_install' not in out:
        debug("Module 'setuptools.command.easy_install not found")
        res = False

    if res is None:
        os.system(cmd_tmpl % "import setuptools; print setuptools.__file__")
        setuptools_loc = open(outfile).read().strip()
        res = os.path.dirname(os.path.dirname(setuptools_loc))
        debug("Location of setuptools installation: %s" % res)

    try:
        os.remove(outfile)
    except Exception:
        pass

    return res


def run_easy_install(args):
    """Run easy_install with specified list of arguments"""
    import setuptools
    debug("Active setuptools installation: %s" % setuptools.__file__)
    from setuptools.command import easy_install

    orig_stdout, orig_stderr = mock_stdout_stderr()
    try:
        easy_install.main(args)
        easy_install_stdout, easy_install_stderr = restore_stdout_stderr(orig_stdout, orig_stderr)
    except (Exception, SystemExit) as err:
        easy_install_stdout, easy_install_stderr = restore_stdout_stderr(orig_stdout, orig_stderr)
        error("Running 'easy_install %s' failed: %s\n%s" % (' '.join(args), err, traceback.format_exc()))

    debug("stdout for 'easy_install %s':\n%s" % (' '.join(args), easy_install_stdout))
    debug("stderr for 'easy_install %s':\n%s" % (' '.join(args), easy_install_stderr))


def check_easy_install_cmd():
    """Try to make sure available 'easy_install' command matches active 'setuptools' installation."""

    debug("Checking whether available 'easy_install' command matches active 'setuptools' installation...")

    _, outfile = tempfile.mkstemp()

    import setuptools
    debug("Location of active setuptools installation: %s" % setuptools.__file__)

    easy_install_regex = re.compile('^(setuptools|distribute) %s' % setuptools.__version__)
    debug("Pattern for 'easy_install --version': %s" % easy_install_regex.pattern)

    pythonpath = os.getenv('PYTHONPATH', '')
    cmd = "PYTHONPATH='%s' %s -m easy_install --version" % (pythonpath, sys.executable)
    os.system("%s > %s 2>&1" % (cmd, outfile))
    outtxt = open(outfile).read().strip()
    debug("Output of '%s':\n%s" % (cmd, outtxt))
    res = bool(easy_install_regex.match(outtxt))
    debug("Result: %s" % res)
    if res:
        debug("Found right 'easy_install' command")
        return

    error("Failed to find right 'easy_install' command!")


#
# Stage functions
#
def stage0(tmpdir):
    """STAGE 0: Prepare and install distribute via included (patched) distribute_setup.py script."""

    print('\n')
    info("+++ STAGE 0: installing distribute via included (patched) distribute_setup.py...\n")

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
    # We download a custom version of distribute: it uses a newer version of markerlib to avoid a bug (#1099)
    # It's is the source of distribute 0.6.49 with the file _markerlib/markers.py replaced by the 0.6 version of
    # markerlib which can be found at https://pypi.python.org/pypi/markerlib/0.6.0
    sys.argv.append('--download-base=https://easybuilders.github.io/easybuild/files/')
    distribute_setup_main(version="0.6.49-patched1")
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
    from setuptools.command import easy_install

    for mod, path in [('setuptools', setuptools.__file__), ('easy_install', easy_install.__file__)]:
        if tmpdir not in path:
            error("Found another %s module than expected: %s" % (mod, path))
        else:
            debug("Found %s in expected path, good!" % mod)

    info("Installed setuptools version %s (%s)" % (setuptools.__version__, setuptools.__file__))

    return distribute_egg_dir


def stage1(tmpdir, sourcepath, distribute_egg_dir, forcedversion):
    """STAGE 1: temporary install EasyBuild using distribute's easy_install."""

    print('\n')
    info("+++ STAGE 1: installing EasyBuild in temporary dir with easy_install...\n")

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
                if pkg not in [VSC_BASE, VSC_INSTALL]:
                    # vsc-base package is not strictly required
                    # it's only a dependency since EasyBuild v2.0;
                    # with EasyBuild v2.0, it will be pulled in from PyPI when installing easybuild-framework;
                    # vsc-install is an optional dependency, only required to run unit tests
                    error("Missing source tarball: %s" % pkg_tarball_glob)
            else:
                info("Found %s for %s package" % (pkg_tarball_paths[0], pkg))
                source_tarballs.update({pkg: pkg_tarball_paths[0]})

    if print_debug:
        debug("$ easy_install --help")
        run_easy_install(['--help'])

    # prepare install dir
    targetdir_stage1 = os.path.join(tmpdir, STAGE1_SUBDIR)
    prep(targetdir_stage1)  # set PATH, Python search path

    # install latest EasyBuild with easy_install from PyPi
    cmd = [
        '--upgrade',  # make sure the latest version is pulled from PyPi
        '--prefix=%s' % targetdir_stage1,
    ]

    post_vsc_base = []
    if source_tarballs:
        # install provided source tarballs (order matters)
        cmd.extend([source_tarballs[pkg] for pkg in EASYBUILD_PACKAGES if pkg in source_tarballs])
        # add vsc-base again at the end, to avoid that the one available on the system is used instead
        if VSC_BASE in source_tarballs:
            cmd.append(source_tarballs[VSC_BASE])
    else:
        # install meta-package easybuild from PyPI
        if forcedversion:
            cmd.append('easybuild==%s' % forcedversion)
        else:
            cmd.append('easybuild')

        # install vsc-base again at the end, to avoid that the one available on the system is used instead
        post_vsc_base = cmd[:]
        post_vsc_base[-1] = VSC_BASE

    if not print_debug:
        cmd.insert(0, '--quiet')

    info("installing EasyBuild with 'easy_install %s'" % (' '.join(cmd)))
    run_easy_install(cmd)

    if post_vsc_base:
        info("running post install command 'easy_install %s'" % (' '.join(post_vsc_base)))
        run_easy_install(post_vsc_base)

        pkg_egg_dir = find_egg_dir_for(targetdir_stage1, VSC_BASE)
        if pkg_egg_dir is None:
            # if vsc-base available on system is the same version as the one being installed,
            # the .egg directory may not get installed...
            # in that case, try to have it *copied* by also including --always-copy;
            # using --always-copy should be used as a last resort, since it can result in all kinds of problems
            info(".egg dir for vsc-base not found, trying again with --always-copy...")
            post_vsc_base.insert(0, '--always-copy')
            info("running post install command 'easy_install %s'" % (' '.join(post_vsc_base)))
            run_easy_install(post_vsc_base)

    # clear the Python search path, we only want the individual eggs dirs to be in the PYTHONPATH (see below)
    # this is needed to avoid easy-install.pth controlling what Python packages are actually used
    if distribute_egg_dir is not None:
        os.environ['PYTHONPATH'] = distribute_egg_dir
    else:
        del os.environ['PYTHONPATH']

    # template string to inject in template easyconfig
    templates = {}

    for pkg in EASYBUILD_PACKAGES:
        templates.update({pkg: ''})

        pkg_egg_dir = find_egg_dir_for(targetdir_stage1, pkg)
        if pkg_egg_dir is None:
            if pkg in [VSC_BASE, VSC_INSTALL]:
                # vsc-base is optional in older EasyBuild versions
                continue

        # prepend EasyBuild egg dirs to Python search path, so we know which EasyBuild we're using
        sys.path.insert(0, pkg_egg_dir)
        pythonpaths = [x for x in os.environ.get('PYTHONPATH', '').split(os.pathsep) if len(x) > 0]
        os.environ['PYTHONPATH'] = os.pathsep.join([pkg_egg_dir] + pythonpaths)
        debug("$PYTHONPATH: %s" % os.environ['PYTHONPATH'])

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
    eb_version_cmd = 'from easybuild.tools.version import this_is_easybuild; print(this_is_easybuild())'
    cmd = "%s -c '%s' > %s 2>&1" % (sys.executable, eb_version_cmd, version_out_file)
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
    import easybuild.easyblocks
    import vsc.utils.fancylogger
    for pkg in [easybuild.framework, easybuild.easyblocks, vsc.utils.fancylogger]:
        if tmpdir not in pkg.__file__:
            error("Found another %s than expected: %s" % (pkg.__name__, pkg.__file__))
        else:
            debug("Found %s in expected path, good!" % pkg.__name__)

    debug("templates: %s" % templates)
    return templates


def stage2(tmpdir, templates, install_path, distribute_egg_dir, sourcepath):
    """STAGE 2: install EasyBuild to temporary dir with EasyBuild from stage 1."""

    print('\n')
    info("+++ STAGE 2: installing EasyBuild in %s with EasyBuild from stage 1...\n" % install_path)

    preinstallopts = ''

    if distribute_egg_dir is not None:
        # inject path to distribute installed in stage 0 into $PYTHONPATH via preinstallopts
        # other approaches are not reliable, since EasyBuildMeta easyblock unsets $PYTHONPATH;
        # this is required for the easy_install from stage 0 to work
        preinstallopts += "export PYTHONPATH=%s:$PYTHONPATH && " % distribute_egg_dir

        # ensure that (latest) setuptools is installed as well alongside EasyBuild,
        # since it is a required runtime dependency for recent vsc-base and EasyBuild versions
        # this is necessary since we provide our own distribute installation during the bootstrap (cfr. stage0)
        preinstallopts += "%s -m easy_install -U --prefix %%(installdir)s setuptools && " % sys.executable

    # vsc-install is a runtime dependency for the EasyBuild unit test suite,
    # and is easily picked up from stage1 rather than being actually installed, so force it
    vsc_install = 'vsc-install'
    if sourcepath:
        vsc_install_tarball_paths = glob.glob(os.path.join(sourcepath, 'vsc-install*.tar.gz'))
        if len(vsc_install_tarball_paths) == 1:
            vsc_install = vsc_install_tarball_paths[0]
    preinstallopts += "%s -m easy_install -U --prefix %%(installdir)s %s && " % (sys.executable, vsc_install)

    templates.update({
        'preinstallopts': preinstallopts,
    })

    # determine PyPI URLs for individual packages
    pkg_urls = []
    for pkg in EASYBUILD_PACKAGES:
        # format of pkg entries in templates: "'<pkg_filename>',"
        pkg_filename = templates[pkg][1:-2]

        # the lines below implement a simplified version of the 'pypi_source_urls' and 'derive_alt_pypi_url' functions,
        # which we can't leverage here, partially because of transitional changes in PyPI (#md5= -> #sha256=)

        # determine download URL via PyPI's 'simple' API
        pkg_simple = None
        try:
            pkg_simple = urllib2.urlopen('https://pypi.python.org/simple/%s' % pkg, timeout=10).read()
        except (urllib2.URLError, urllib2.HTTPError) as err:
            # failing to figure out the package download URl may be OK when source tarballs are provided
            if sourcepath:
                info("Ignoring failed attempt to determine '%s' download URL since source tarballs are provided" % pkg)
            else:
                raise err

        if pkg_simple:
            pkg_url_part_regex = re.compile('/(packages/[^#]+)/%s#' % pkg_filename)
            res = pkg_url_part_regex.search(pkg_simple)
            if res:
                pkg_url_part = res.group(1)
            else:
                error("Failed to determine PyPI package URL for %s: %s\n" % (pkg, pkg_simple))

            pkg_url = 'https://pypi.python.org/' + pkg_url_part
            pkg_urls.append(pkg_url)

    templates.update({
        'source_urls': '\n'.join(["'%s'," % pkg_url for pkg_url in pkg_urls]),
        'sources': "%(vsc-install)s%(vsc-base)s%(easybuild-framework)s%(easybuild-easyblocks)s%(easybuild-easyconfigs)s" % templates,
        'pythonpath': distribute_egg_dir,
    })

    # create easyconfig file
    ebfile = os.path.join(tmpdir, 'EasyBuild-%s.eb' % templates['version'])
    handle = open(ebfile, 'w')
    ebfile_txt = EASYBUILD_EASYCONFIG_TEMPLATE % templates
    handle.write(ebfile_txt)
    handle.close()
    debug("Contents of generated easyconfig file:\n%s" % ebfile_txt)

    # set command line arguments for eb
    eb_args = ['eb', ebfile, '--allow-modules-tool-mismatch']
    if print_debug:
        eb_args.extend(['--debug', '--logtostdout'])
    if forced_install:
        info("Performing FORCED installation, as requested...")
        eb_args.append('--force')

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
        modules_path = det_modules_path(install_path)
        if not os.path.exists(modules_path):
            os.makedirs(modules_path)
        debug("Created path %s" % modules_path)

    debug("Running EasyBuild with arguments '%s'" % ' '.join(eb_args))
    sys.argv = eb_args

    # location to 'eb' command (from stage 1) may be expected to be included in $PATH
    # it usually is there after stage1, unless 'prep' is called again with another location
    # (only when stage 0 is not skipped)
    # cfr. https://github.com/easybuilders/easybuild-framework/issues/2279
    curr_path = [x for x in os.environ.get('PATH', '').split(os.pathsep) if len(x) > 0]
    os.environ['PATH'] = os.pathsep.join([os.path.join(tmpdir, STAGE1_SUBDIR, 'bin')] + curr_path)
    debug("$PATH: %s" % os.environ['PATH'])

    # install EasyBuild with EasyBuild
    from easybuild.main import main as easybuild_main
    easybuild_main()

    if print_debug:
        os.environ['EASYBUILD_DEBUG'] = '1'

    # make sure the EasyBuild module was actually installed
    # EasyBuild configuration options that are picked up from configuration files/environment may break the bootstrap,
    # for example by having $EASYBUILD_VERSION defined or via a configuration file specifies a value for 'stop'...
    from easybuild.tools.config import build_option, install_path, get_module_syntax
    from easybuild.framework.easyconfig.easyconfig import ActiveMNS
    eb_spec = {
        'name': 'EasyBuild',
        'hidden': False,
        'toolchain': {'name': 'dummy', 'version': 'dummy'},
        'version': templates['version'],
        'versionprefix': '',
        'versionsuffix': '',
        'moduleclass': 'tools',
    }

    mod_path = os.path.join(install_path('mod'), build_option('suffix_modules_path'))
    debug("EasyBuild module should have been installed to %s" % mod_path)

    eb_mod_name = ActiveMNS().det_full_module_name(eb_spec)
    debug("EasyBuild module name: %s" % eb_mod_name)

    eb_mod_path = os.path.join(mod_path, eb_mod_name)
    if get_module_syntax() == 'Lua':
        eb_mod_path += '.lua'

    if os.path.exists(eb_mod_path):
        info("EasyBuild module installed: %s" % eb_mod_path)
    else:
        error("EasyBuild module not found at %s, define $EASYBUILD_BOOTSTRAP_DEBUG to debug" % eb_mod_path)


def main():
    """Main script: bootstrap EasyBuild in stages."""

    self_txt = open(__file__).read()
    info("EasyBuild bootstrap script (version %s, MD5: %s)" % (EB_BOOTSTRAP_VERSION, md5(self_txt).hexdigest()))
    info("Found Python %s\n" % '; '.join(sys.version.split('\n')))

    # disallow running as root, since stage 2 will fail
    if os.getuid() == 0:
        error("Don't run the EasyBuild bootstrap script as root, "
              "since stage 2 (installing EasyBuild with EasyBuild) will fail.")

    # general option/argument parser
    if HAVE_ARGPARSE:
        bs_argparser = argparse.ArgumentParser()
        bs_argparser.add_argument("prefix", help="Installation prefix directory",
                                  type=str)
        bs_args = bs_argparser.parse_args()

        # prefix specification
        install_path = os.path.abspath(bs_args.prefix)
    else:
        bs_argparser = optparse.OptionParser(usage="usage: %prog [options] prefix")
        (bs_opts, bs_args) = bs_argparser.parse_args()

        # poor method, but should prefer argparse module for better pos arg support.
        if len(bs_args) < 1:
            error("Too few arguments\n" + bs_argparser.get_usage())
        elif len(bs_args) > 1:
            error("Too many arguments\n" + bs_argparser.get_usage())

        # prefix specification
        install_path = os.path.abspath(str(bs_args[0]))

    info("Installation prefix %s" % install_path)

    sourcepath = EASYBUILD_BOOTSTRAP_SOURCEPATH
    if sourcepath is not None:
        info("Fetching sources from %s..." % sourcepath)

    forcedversion = EASYBUILD_BOOTSTRAP_FORCE_VERSION
    if forcedversion:
        info("Forcing specified version %s..." % forcedversion)

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
    distribute_egg_dir = None
    if EASYBUILD_BOOTSTRAP_SKIP_STAGE0:
        info("Skipping stage 0, using local distribute/setuptools providing easy_install")
    else:
        setuptools_loc = check_setuptools()
        if setuptools_loc:
            info("Suitable setuptools installation already found, skipping stage 0...")
            sys.path.insert(0, setuptools_loc)
        else:
            info("No suitable setuptools installation found, proceeding with stage 0...")
            distribute_egg_dir = stage0(tmpdir)

    # STAGE 1: install EasyBuild using easy_install to tmp dir
    templates = stage1(tmpdir, sourcepath, distribute_egg_dir, forcedversion)

    # add location to easy_install provided through stage0 to $PATH
    # this must be done *after* stage1, since $PATH is reset during stage1
    if distribute_egg_dir:
        prep(tmpdir)

    # make sure the active 'easy_install' is the right one (i.e. it matches the active setuptools installation)
    check_easy_install_cmd()

    # STAGE 2: install EasyBuild using EasyBuild (to final target installation dir)
    stage2(tmpdir, templates, install_path, distribute_egg_dir, sourcepath)

    # clean up the mess
    debug("Cleaning up %s..." % tmpdir)
    shutil.rmtree(tmpdir)

    print('')
    info('Bootstrapping EasyBuild completed!\n')

    if install_path is not None:
        info('EasyBuild v%s was installed to %s, so make sure your $MODULEPATH includes %s' %
             (templates['version'], install_path, det_modules_path(install_path)))
    else:
        info('EasyBuild v%s was installed to configured install path, make sure your $MODULEPATH is set correctly.' %
             templates['version'])
        info('(default config => add "$HOME/.local/easybuild/modules/all" in $MODULEPATH)')

    print('')
    info("Run 'module load EasyBuild', and run 'eb --help' to get help on using EasyBuild.")
    info("Set $EASYBUILD_MODULES_TOOL to '%s' to use the same modules tool as was used now." % modtool)
    print('')
    info("By default, EasyBuild will install software to $HOME/.local/easybuild.")
    info("To install software with EasyBuild to %s, make sure $EASYBUILD_INSTALLPATH is set accordingly." % install_path)
    info("See http://easybuild.readthedocs.org/en/latest/Configuration.html for details on configuring EasyBuild.")

# template easyconfig file for EasyBuild
EASYBUILD_EASYCONFIG_TEMPLATE = """
easyblock = 'EB_EasyBuildMeta'

name = 'EasyBuild'
version = '%(version)s'

homepage = 'http://easybuilders.github.com/easybuild/'
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

pyshortver = '.'.join(SYS_PYTHON_VERSION.split('.')[:2])
sanity_check_paths = {
    'files': ['bin/eb'],
    'dirs': [('lib/python%%s/site-packages' %% pyshortver, 'lib64/python%%s/site-packages' %% pyshortver)],
}

moduleclass = 'tools'
"""

# check Python version
if sys.version_info[0] != 2 or sys.version_info[1] < 6:
    pyver = sys.version.split(' ')[0]
    sys.stderr.write("ERROR: Incompatible Python version: %s (should be Python 2 >= 2.6)\n" % pyver)
    sys.stderr.write("Please try again using 'python2 %s <prefix>'\n" % os.path.basename(__file__))
    sys.exit(1)

# distribute_setup.py script (https://pypi.python.org/pypi/distribute)
#
# A compressed copy of a patched distribute_setup.py (version 0.6.49), generated like so:
# >>> import base64
# >>> import zlib
# >>> base64.b64encode(zlib.compress(open("distribute_setup.py").read()))
# compressed copy below is for setuptools 0.6c11, after applying patch:
#
# --- distribute_setup.py.orig	2013-07-05 03:50:13.000000000 +0200
# +++ distribute_setup.py	2015-11-27 12:20:12.040032041 +0100
# @@ -528,6 +528,8 @@
#              log.warn("--user requires Python 2.6 or later")
#              raise SystemExit(1)
#          install_args.append('--user')
# +    if options.prefix_install:
# +        install_args.append('--prefix=%s' % options.prefix_install)
#      return install_args
#
#  def _parse_args():
# @@ -539,6 +541,8 @@
#          '--user', dest='user_install', action='store_true', default=False,
#          help='install in user site package (requires Python 2.6 or later)')
#      parser.add_option(
# +        '--prefix', dest='prefix_install', metavar="PATH", help='install in prefix')
# +    parser.add_option(
#          '--download-base', dest='download_base', metavar="URL",
#          default=DEFAULT_URL,
#          help='alternative URL from where to download the distribute package')
# @@ -549,7 +553,7 @@
#  def main(version=DEFAULT_VERSION):
#      """Install or upgrade setuptools and EasyInstall"""
#      options = _parse_args()
# -    tarball = download_setuptools(download_base=options.download_base)
# +    tarball = download_setuptools(version=version, download_base=options.download_base)
#      return _install(tarball, _build_install_args(options))
#
#  if __name__ == '__main__':
#
DISTRIBUTE_SETUP_PY = """
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
7lwukTHLpfpLDH2GT+yCCf8D2cp1xw==

""".decode("base64").decode("zlib")

# run main function as body of script
main()
