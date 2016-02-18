#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

#
# spec file for package python-setuptools
#
# Copyright (c) 2015 SUSE LINUX GmbH, Nuernberg, Germany.
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.

# Please submit bugfixes or comments via http://bugs.opensuse.org/
#


Name:           python-setuptools
Version:        18.8
Release:        62.1
Url:            http://pypi.python.org/pypi/setuptools
Summary:        Easily download, build, install, upgrade, and uninstall Python packages
License:        Python Software Foundation License
Group:          Development/Languages/Python
Source:         https://pypi.python.org/packages/source/s/setuptools/setuptools-%{version}.tar.gz
Source1:        psfl.txt
Source2:        zpl.txt
Patch1:         setuptools-5.4.1-create-sitedir.patch
# NOTE(toabctl): Fix for SLE11SP3 test failures
Patch3:         fix-sle11-test-failure.patch
BuildRoot:      %{_tmppath}/%{name}-%{version}-build
BuildRequires:  python-devel
BuildRequires:  python-xml
# for tests
#BuildRequires:  python-mock
#BuildRequires:  python-pytest
#BuildRequires:  python-pytest-runner
# needed for SLE
Requires:       python
Requires:       python-xml
Requires(post): update-alternatives
Requires(postun): update-alternatives
# NOTE(saschpe): Distribute was merged into 0.7.x, so even though distribute
# obsoletes setuptools < 0.6.45, current setuptools obsoletes distribute again
Provides:       python-distribute = %{version}
Obsoletes:      python-distribute < %{version}
%if 0%{?suse_version} && 0%{?suse_version} <= 1110
%{!?python_sitelib: %global python_sitelib %(python -c "from distutils.sysconfig import get_python_lib; print get_python_lib()")}
%else
BuildArch:      noarch
%endif

%description
setuptools is a collection of enhancements to the Python distutils that
allow you to more easily build and distribute Python packages,
especially ones that have dependencies on other packages.

%prep
%setup -q -n setuptools-%{version}
%patch1 -p1
%if 0%{?suse_version} && 0%{?suse_version} <= 1220
%patch3 -p1
%endif
find . -type f -name "*.orig" -delete

%build
chmod -x *txt
python setup.py build

%install
python setup.py install --prefix=%{_prefix} --root=%{buildroot}
rm %{buildroot}%{_bindir}/easy_install
mkdir -p %{buildroot}%{_sysconfdir}/alternatives
touch %{buildroot}%{_sysconfdir}/alternatives/easy_install
ln -sf %{_sysconfdir}/alternatives/easy_install %{buildroot}/%{_bindir}/easy_install

%check
# Can not run testsuite as this introduces build cycle
#export LANG="en_US.UTF-8"
#python setup.py ptr --addopts='-rxs'

%post
update-alternatives \
    --install %{_bindir}/easy_install easy_install %{_bindir}/easy_install-%{py_ver} 20

%postun
if [ $1 -eq 0 ] ; then
    update-alternatives --remove easy_install %{_bindir}/easy_install-%{py_ver}
fi

%files
%defattr(-,root,root,-)
%doc CHANGES.txt README.txt
%{_bindir}/easy_install
%{_bindir}/easy_install-%{py_ver}
%ghost %{_sysconfdir}/alternatives/easy_install
%{python_sitelib}/_markerlib
%{python_sitelib}/setuptools
%{python_sitelib}/setuptools-%{version}-py%{py_ver}.egg-info
%{python_sitelib}/easy_install.py*
%{python_sitelib}/pkg_resources

%changelog
* Mon Jul 20 2015 dmueller@suse.com
- update to 18.0.1:
  * Issue #401: Fix failure in test suite.
  * Dropped support for builds with Pyrex. Only Cython is supported.
  * Issue #288: Detect Cython later in the build process, after
    ``setup_requires`` dependencies are resolved.
  * Issue #396: Fixed test failure on OS X.
  * Pull Request #136: Remove excessive quoting from shebang headers
    for Jython.
* Mon Jun 22 2015 tbechtold@suse.com
- update to 17.1.1:
  * Backed out unintended changes to pkg_resources, restoring removal of
    deprecated imp module
* Mon Jun  8 2015 tbechtold@suse.com
- update to 17.1:
  * Issue #380: Add support for range operators on environment
    marker evaluation.
  * Issue #378: Do not use internal importlib._bootstrap module.
  * Issue #390: Disallow console scripts with path separators in
    the name. Removes unintended functionality and brings behavior
    into parity with pip.
  * Pull Request #130: Better error messages for errors in
    parsed requirements.
  * Pull Request #133: Removed ``setuptools.tests`` from the
    installed packages.
  * Issue #373: Provisionally expose
    ``pkg_resources._initialize_master_working_set``, allowing for
    imperative re-initialization of the master working set.
* Mon Apr 27 2015 mcihar@suse.cz
- Disable testsuite as this causes build cycle
* Wed Apr 22 2015 tbechtold@suse.com
- update to 15.1:
  * Updated Packaging to 15.1 to address Packaging #28.
  * Fix ``setuptools.sandbox._execfile()`` with Python 3.1.
- Remove fix-type-error.patch . Applied upstream
* Sat Apr 11 2015 hpj@urpla.net
- openSUSE versions up to 12.2 are affected as well from:
  AssertionError: error: must supply either home or prefix/exec-prefix -- not both
  raise version check of applying fix-sle11-test-failure.patch accordingly
* Fri Mar 27 2015 tbechtold@suse.com
- update to 15.0:
  * Pull Request #126: DistributionNotFound message now lists the package or
    packages that required it. E.g.::
    pkg_resources.DistributionNotFound: The 'colorama>=0.3.1' distribution
    was not found and is required by smlib.log.
    Note that zc.buildout once dependended on the string rendering of this
    message to determine the package that was not found. This expectation
    has since been changed, but older versions of buildout may experience
    problems. See Buildout #242 for details.
  * Issue #307: Removed PEP-440 warning during parsing of versions
    in ``pkg_resources.Distribution``.
  * Issue #364: Replace deprecated usage with recommended usage of
    ``EntryPoint.load``.
  * Issue #254: When creating temporary egg cache on Unix, use mode 755
    for creating the directory to avoid the subsequent warning if
    the directory is group writable.
  * Issue #137: Update ``Distribution.hashcmp`` so that Distributions with
    None for pyversion or platform can be compared against Distributions
    defining those attributes.
  * Issue #360: Removed undesirable behavior from test runs, preventing
    write tests and installation to system site packages.
  * Pull Request #125: Add ``__ne__`` to Requirement class.
  * Various refactoring of easy_install.
  * Bootstrap script now accepts ``--to-dir`` to customize save directory or
    allow for re-use of existing repository of setuptools versions. See
    Pull Request #112 for background.
  * Issue #285: ``easy_install`` no longer will default to installing
    packages to the "user site packages" directory if it is itself installed
    there. Instead, the user must pass ``--user`` in all cases to install
    packages to the user site packages.
    This behavior now matches that of "pip install". To configure
    an environment to always install to the user site packages, consider
    using the "install-dir" and "scripts-dir" parameters to easy_install
    through an appropriate distutils config file.
  * Issue #359: Include pytest.ini in the sdist so invocation of py.test on the
    sdist honors the pytest configuration.
    Re-release of 13.0. Intermittent connectivity issues caused the release
    process to fail and PyPI uploads no longer accept files for 13.0.
  * Issue #356: Back out Pull Request #119 as it requires Setuptools 10 or later
    as the source during an upgrade.
  * Removed build_py class from setup.py. According to 892f439d216e, this
    functionality was added to support upgrades from old Distribute versions,
    0.6.5 and 0.6.6.
  * Pull Request #119: Restore writing of ``setup_requires`` to metadata
    (previously added in 8.4 and removed in 9.0).
  * Documentation is now linked using the rst.linker package.
  * Fix ``setuptools.command.easy_install.extract_wininst_cfg()``
    with Python 2.6 and 2.7.
  * Issue #354. Added documentation on building setuptools
    documentation.
  * Issue #345: Unload all modules under pkg_resources during
    ``ez_setup.use_setuptools()``.
  * Issue #336: Removed deprecation from ``ez_setup.use_setuptools``,
    as it is clearly still used by buildout's bootstrap. ``ez_setup``
    remains deprecated for use by individual packages.
  * Simplified implementation of ``ez_setup.use_setuptools``.
  * Pull Request #118: Soften warning for non-normalized versions in
    Distribution.
  * Issue #339: Correct Attribute reference in ``cant_write_to_target``.
  * Issue #336: Deprecated ``ez_setup.use_setuptools``.
  * Issue #335: Fix script header generation on Windows.
  * Fixed incorrect class attribute in ``install_scripts``. Tests would be nice.
  * Issue #331: Fixed ``install_scripts`` command on Windows systems corrupting
    the header.
  * Restore ``setuptools.command.easy_install.sys_executable`` for pbr
    compatibility. For the future, tools should construct a CommandSpec
    explicitly.
  * Issue #188: Setuptools now support multiple entities in the value for
    ``build.executable``, such that an executable of "/usr/bin/env my-python" may
    be specified. This means that systems with a specified executable whose name
    has spaces in the path must be updated to escape or quote that value.
  * Deprecated ``easy_install.ScriptWriter.get_writer``, replaced by ``.best()``
    with slightly different semantics (no force_windows flag).
  * Issue #327: Formalize and restore support for any printable character in an
    entry point name.
  * Expose ``EntryPoint.resolve`` in place of EntryPoint._load, implementing the
    simple, non-requiring load. Deprecated all uses of ``EntryPoint._load``
    except for calling with no parameters, which is just a shortcut for
    ``ep.require(); ep.resolve();``.
    Apps currently invoking ``ep.load(require=False)`` should instead do the
    following if wanting to avoid the deprecating warning::
    getattr(ep, "resolve", lambda: ep.load(require=False))()
  * Pip #2326: Report deprecation warning at stacklevel 2 for easier diagnosis.
  * Issue #281: Since Setuptools 6.1 (Issue #268), a ValueError would be raised
    in certain cases where VersionConflict was raised with two arguments, which
    occurred in ``pkg_resources.WorkingSet.find``. This release adds support
    for indicating the dependent packages while maintaining support for
    a VersionConflict when no dependent package context is known. New unit tests
    now capture the expected interface.
  * Interop #3: Upgrade to Packaging 15.0; updates to PEP 440 so that >1.7 does
    not exclude 1.7.1 but does exclude 1.7.0 and 1.7.0.post1.
  * Issue #323: Fix regression in entry point name parsing.
  * Deprecated use of EntryPoint.load(require=False). Passing a boolean to a
    function to select behavior is an anti-pattern. Instead use
    ``Entrypoint._load()``.
  * Substantial refactoring of all unit tests. Tests are now much leaner and
    re-use a lot of fixtures and contexts for better clarity of purpose.
  * Issue #320: Added a compatibility implementation of
    ``sdist._default_revctrl``
    so that systems relying on that interface do not fail (namely, Ubuntu 12.04
    and similar Debian releases).
  * Issue #319: Fixed issue installing pure distutils packages.
  * Issue #313: Removed built-in support for subversion. Projects wishing to
    retain support for subversion will need to use a third party library. The
    extant implementation is being ported to `setuptools_svn
    <https://pypi.python.org/pypi/setuptools_svn>`_.
  * Issue #315: Updated setuptools to hide its own loaded modules during
    installation of another package. This change will enable setuptools to
    upgrade (or downgrade) itself even when its own metadata and implementation
    change.
  * Prefer vendored packaging library `as recommended
    <https://github.com/jaraco/setuptools/commit/170657b68f4b92e7e1bf82f5e19a831f57
    44af67#commitcomment-9109448>`_.
  * Issue #312: Restored presence of pkg_resources API tests (doctest) to sdist.
  * Issue #314: Disabled support for ``setup_requires`` metadata to avoid issue
    where Setuptools was unable to upgrade over earlier versions.
  * Pull Request #106: Now write ``setup_requires`` metadata.
  * Issue #311: Decoupled pkg_resources from setuptools once again.
    ``pkg_resources`` is now a package instead of a module.
  * Issue #306: Suppress warnings about Version format except in select scenarios
    (such as installation).
  * Pull Request #85: Search egg-base when adding egg-info to manifest.
  * Upgrade ``packaging`` to 14.5, giving preference to "rc" as designator for
    release candidates over "c".
  * PEP-440 warnings are now raised as their own class,
    ``pkg_resources.PEP440Warning``, instead of RuntimeWarning.
  * Disabled warnings on empty versions.
  * Upgrade ``packaging`` to 14.4, fixing an error where there is a
    different result for if 2.0.5 is contained within >2.0dev and >2.0.dev even
    though normalization rules should have made them equal.
  * Issue #296: Add warning when a version is parsed as legacy. This warning will
    make it easier for developers to recognize deprecated version numbers.
  * Issue #296: Restored support for ``__hash__`` on parse_version results.
  * Issue #296: Restored support for ``__getitem__`` and sort operations on
    parse_version result.
  * Issue #296: Restore support for iteration over parse_version result, but
    deprecated that usage with a warning. Fixes failure with buildout.
  * Implement `PEP 440 <http://legacy.python.org/dev/peps/pep-0440/>`_ within
    pkg_resources and setuptools. This change
    deprecates some version numbers such that they will no longer be installable
    without using the ``===`` escape hatch. See `the changes to test_resources
    <https://bitbucket.org/pypa/setuptools/commits/dcd552da643c4448056de84c73d56da6
    d70769d5#chg-setuptools/tests/test_resources.py>`_
    for specific examples of version numbers and specifiers that are no longer
    supported. Setuptools now "vendors" the `packaging
    <https://github.com/pypa/packaging>`_ library.
  * Issue #80, Issue #209: Eggs that are downloaded for ``setup_requires``,
    ``test_requires``, etc. are now placed in a ``./.eggs`` directory instead of
    directly in the current directory. This choice of location means the files
    can be readily managed (removed, ignored). Additionally,
    later phases or invocations of setuptools will not detect the package as
    already installed and ignore it for permanent install (See #209).
    This change is indicated as backward-incompatible as installations that
    depend on the installation in the current directory will need to account for
    the new location. Systems that ignore ``*.egg`` will probably need to be
    adapted to ignore ``.eggs``. The files will need to be manually moved or
    will be retrieved again. Most use cases will require no attention.
  * Issue #268: When resolving package versions, a VersionConflict now reports
    which package previously required the conflicting version.
  * Issue #262: Fixed regression in pip install due to egg-info directories
    being omitted. Re-opens Issue #118.
  * Issue #259: Fixed regression with namespace package handling on ``single
    version, externally managed`` installs.
  * Issue #100: When building a distribution, Setuptools will no longer match
    default files using platform-dependent case sensitivity, but rather will
    only match the files if their case matches exactly. As a result, on Windows
    and other case-insensitive file systems, files with names such as
    'readme.txt' or 'README.TXT' will be omitted from the distribution and a
    warning will be issued indicating that 'README.txt' was not found. Other
    filenames affected are:
  - README.rst
  - README
  - setup.cfg
  - setup.py (or the script name)
  - test/test*.py
    Any users producing distributions with filenames that match those above
    case-insensitively, but not case-sensitively, should rename those files in
    their repository for better portability.
  * Pull Request #72: When using ``single_version_externally_managed``, the
    exclusion list now includes Python 3.2 ``__pycache__`` entries.
  * Pull Request #76 and Pull Request #78: lines in top_level.txt are now
    ordered deterministically.
  * Issue #118: The egg-info directory is now no longer included in the list
    of outputs.
  * Issue #258: Setuptools now patches distutils msvc9compiler to
    recognize the specially-packaged compiler package for easy extension module
    support on Python 2.6, 2.7, and 3.2.
  * Issue #237: ``pkg_resources`` now uses explicit detection of Python 2 vs.
    Python 3, supporting environments where builtins have been patched to make
    Python 3 look more like Python 2.
  * Issue #240: Based on real-world performance measures against 5.4, zip
    manifests are now cached in all circumstances. The
    ``PKG_RESOURCES_CACHE_ZIP_MANIFESTS`` environment variable is no longer
    relevant. The observed "memory increase" referenced in the 5.4 release
    notes and detailed in Issue #154 was likely not an increase over the status
    quo, but rather only an increase over not storing the zip info at all.
  * Issue #242: Use absolute imports in svn_utils to avoid issues if the
    installing package adds an xml module to the path.
  * Issue #239: Fix typo in 5.5 such that fix did not take.
  * Issue #239: Setuptools now includes the setup_requires directive on
    Distribution objects and validates the syntax just like install_requires
    and tests_require directives.
  * Issue #236: Corrected regression in execfile implementation for Python 2.6.
- Enable testsuite run during build. Added BuildRequires needed for that
- Refresh setuptools-5.4.1-create-sitedir.patch
- Add fix-type-error.patch: Fix error during test run
- Add fix-sle11-test-failure.patch: Fix error during test run on SLE11SP3
* Wed Jul 23 2014 sleep_walker@suse.cz
- bump to 5.4.1
  Changes between 5.4.1 and 3.6
  * Python #7776: (ssl_support) Correct usage of host for validation when
    tunneling for HTTPS.
  * Issue #154: pkg_resources will now cache the zip manifests rather than
    re-processing the same file from disk multiple times, but only if the
    environment variable PKG_RESOURCES_CACHE_ZIP_MANIFESTS is set. Clients
    that package many modules in the same zip file will see some improvement
    in startup time by enabling this feature. This feature is not enabled by
    default because it causes a substantial increase in memory usage.
  * Issue #185: Make svn tagging work on the new style SVN metadata. Thanks
    cazabon!
  * Prune revision control directories (e.g .svn) from base path as well as
    sub-directories.
  * Added a Developer Guide to the official documentation.
  * Some code refactoring and cleanup was done with no intended behavioral
    changes.
  * During install_egg_info, the generated lines for namespace package .pth
    files are now processed even during a dry run.
  * Issue #202: Implemented more robust cache invalidation for the
    ZipImporter, building on the work in Issue #168. Special thanks to Jurko
    Gospodnetic and PJE.
  * Issue #220: Restored script templates.
  * Renamed script templates to end with .tmpl now that they no longer need to
    be processed by 2to3. Fixes spurious syntax errors during build/install.
  * Issue #218: Re-release of 3.8.1 to signal that it supersedes 4.x.
  * Incidentally, script templates were updated not to include the
    triple-quote escaping.
  * Issue #213: Use legacy StringIO behavior for compatibility under pbr.
  * Issue #218: Setuptools 3.8.1 superseded 4.0.1, and 4.x was removed from
    the available versions to install.
  * Issue #210: setup.py develop now copies scripts in binary mode rather than
    text mode, matching the behavior of the install command.
  * Extend Issue #197 workaround to include all Python 3 versions prior to
  * Issue #193: Improved handling of Unicode filenames when building
    manifests.
- drop not applying patch setuptools-0.6c9-create-sitedir.patch
- introduce applying patch with the same effect setuptools-5.4.1-create-sitedir.patch
* Thu May  8 2014 toddrme2178@gmail.com
- Update to version 3.6
  * Issue #203: Honor proxy settings for Powershell downloader in the bootstrap
    routine.
- Update to version 3.5.2
  * Issue #168: More robust handling of replaced zip files and stale caches.
    Fixes ZipImportError complaining about a 'bad local header'.
- Update to version 3.5.1
  * Issue #199: Restored ``install._install`` for compatibility with earlier
    NumPy versions.
- Update to version 3.5
  * Issue #195: Follow symbolic links in find_packages (restoring behavior
    broken in 3.4).
  * Issue #197: On Python 3.1, PKG-INFO is now saved in a UTF-8 encoding instead
    of ``sys.getpreferredencoding`` to match the behavior on Python 2.6-3.4.
  * Issue #192: Preferred bootstrap location is now
    https://bootstrap.pypa.io/ez_setup.py (mirrored from former location).
* Thu Apr 17 2014 toddrme2178@gmail.com
- Update to version 3.4.4
  * Issue #184: Correct failure where find_package over-matched
    packages when directory traversal isn't short-circuited.
- Update to version 3.4.3
  * Issue #183: Really fix test command with Python 3.1.
- Update to version 3.4.2
  * Issue #183: Fix additional regression in test command on
    Python 3.1.
- Update to version 3.4.1
  * Issue #180: Fix regression in test command not caught
    by py.test-run tests.
- Update to version 3.4
  * Issue #176: Add parameter to the test command to support a
    custom test runner: --test-runner or -r.
  * Issue #177: Now assume most common invocation to install
    command on platforms/environments without stack support
    (issuing a warning). Setuptools now installs naturally on
    IronPython. Behavior on CPython should be unchanged.
- Remove %%check, which now depends on pytest and thus introduces
  a dependency loop (setuptools->pytest->py->setuptools)
* Thu Mar 20 2014 speilicke@suse.com
- Update to version 3.3:
  * Add ``include`` parameter to ``setuptools.find_packages()``.
- Changes from version 3.2:
  * Pull Request #39: Add support for C++ targets from Cython ``.pyx`` files.
  * Issue #162: Update dependency on certifi to 1.0.1.
  * Issue #164: Update dependency on wincertstore to 0.2.
* Fri Jan 31 2014 speilicke@suse.com
- Update to version 2.1:
  * Issue #129: Suppress inspection of '*.whl' files when searching for files
    in a zip-imported file.
  * Issue #131: Fix RuntimeError when constructing an egg fetcher.
- Changes from version 2.0.2:
  * Fix NameError during installation with Python implementations (e.g. Jython)
    not containing parser module.
  * Fix NameError in sdist:re_finder.
- Changes from version 2.0.1:
  * Issue #124: Fixed error in list detection in upload_docs.
- Changes from version 2.0:
  * Issue #121: Exempt lib2to3 pickled grammars from DirectorySandbox.
  * Issue #41: Dropped support for Python 2.4 and Python 2.5. Clients requiring
    setuptools for those versions of Python should use setuptools 1.x.
  * Removed setuptools.command.easy_install.HAS_USER_SITE. Clients
    expecting this boolean variable should use site.ENABLE_USER_SITE
    instead.
  * Removed pkg_resources.ImpWrapper. Clients that expected this class
    should use pkgutil.ImpImporter instead.
- Changes from version 1.4.2:
  * Issue #116: Correct TypeError when reading a local package index on Python
    3.
- Changes from version 1.4.1:
  * Issue #114: Use sys.getfilesystemencoding for decoding config in
    bdist_wininst distributions.
  * Issue #105 and Issue #113: Establish a more robust technique for
    determining the terminal encoding
- Changes from version 1.4:
  * Issue #27: easy_install will now use credentials from .pypirc if
    present for connecting to the package index.
  * Pull Request #21: Omit unwanted newlines in package_index._encode_auth
    when the username/password pair length indicates wrapping.
- Changes from version 1.3.2:
  * Issue #99: Fix filename encoding issues in SVN support.
- Changes from version 1.3.1:
  * Remove exuberant warning in SVN support when SVN is not used.
- Changes from version 1.3:
  * Address security vulnerability in SSL match_hostname check as reported in
    Python #17997.
  * Prefer backports.ssl_match_hostname
    <https://pypi.python.org/pypi/backports.ssl_match_hostname>_ for backport
    implementation if present.
  * Correct NameError in ssl_support module (socket.error).
- Changes from version 1.2:
  * Issue #26: Add support for SVN 1.7. Special thanks to Philip Thiem for the
    contribution.
  * Issue #93: Wheels are now distributed with every release. Note that as
    reported in Issue #108, as of Pip 1.4, scripts aren't installed properly
    from wheels. Therefore, if using Pip to install setuptools from a wheel,
    the easy_install command will not be available.
  * Setuptools "natural" launcher support, introduced in 1.0, is now officially
    supported.
- Changes from version 1.1.7:
  * Fixed behavior of NameError handling in 'script template (dev).py' (script
    launcher for 'develop' installs).
  * ez_setup.py now ensures partial downloads are cleaned up following
    a failed download.
  * Distribute #363 and Issue #55: Skip an sdist test that fails on locales
    other than UTF-8.
- New dependency on python-xml
- Fix update-alternatives usage
* Fri Nov  8 2013 aj@ajaissle.de
- New upstream version
  * Fixed behavior of NameError handling in 'script template (dev).py' (script launcher for 'develop' installs).
  * ez_setup.py now ensures partial downloads are cleaned up following a failed download.
  * Distribute #363 and Issue #55: Skip an sdist test that fails on locales other than UTF-8.
* Fri Oct 11 2013 speilicke@suse.com
- Update to version 1.1.6:
  + Distribute #349: sandbox.execfile now opens the target file in binary
    mode, thus honoring a BOM in the file when compiled.
- Changes from version 1.1.5:
  + Issue #69: Second attempt at fix (logic was reversed).
- Changes from version 1.1.4:
  + Issue #77: Fix error in upload command (Python 2.4).
- Changes from version 1.1.3:
  + Fix NameError in previous patch.
- Changes from version 1.1.2:
  + Issue #69: Correct issue where 404 errors are returned for URLs with
    fragments in them (such as #egg=).
- Changes from version 1.1.1:
  + Issue #75: Add --insecure option to ez_setup.py to accommodate
    environments where a trusted SSL connection cannot be validated.
  + Issue #76: Fix AttributeError in upload command with Python 2.4.
- Changes from version 1.1:
  + Issue #71 (Distribute #333): EasyInstall now puts less emphasis on the
    condition when a host is blocked via --allow-hosts.
  + Issue #72: Restored Python 2.4 compatibility in ez_setup.py.
- Changes from version 1.0:
  + Issue #60: On Windows, Setuptools supports deferring to another launcher,
    such as Vinay Sajip's pylauncher <https://bitbucket.org/pypa/pylauncher>_
    (included with Python 3.3) to launch console and GUI scripts and not install
    its own launcher executables. This experimental functionality is currently
    only enabled if  the SETUPTOOLS_LAUNCHER environment variable is set to
    "natural". In the future, this behavior may become default, but only after
    it has matured and seen substantial adoption. The SETUPTOOLS_LAUNCHER
    also accepts "executable" to force the default behavior of creating launcher
    executables.
  + Issue #63: Bootstrap script (ez_setup.py) now prefers Powershell, curl, or
    wget for retrieving the Setuptools tarball for improved security of the
    install. The script will still fall back to a simple urlopen on
    platforms that do not have these tools.
  + Issue #65: Deprecated the Features functionality.
  + Issue #52: In VerifyingHTTPSConn, handle a tunnelled (proxied)
    connection.
  + Backward-Incompatible Changes:
    This release includes a couple of backward-incompatible changes, but most if
    not all users will find 1.0 a drop-in replacement for 0.9.
  - Issue #50: Normalized API of environment marker support. Specifically,
    removed line number and filename from SyntaxErrors when returned from
    pkg_resources.invalid_marker. Any clients depending on the specific
    string representation of exceptions returned by that function may need to
    be updated to account for this change.
  - Issue #50: SyntaxErrors generated by pkg_resources.invalid_marker are
    normalized for cross-implementation consistency.
  - Removed --ignore-conflicts-at-my-risk and --delete-conflicting
    options to easy_install. These options have been deprecated since 0.6a11.
- Unify changes format
* Sun Aug 18 2013 toddrme2178@gmail.com
- Re-add Requires: python.  Not needed for recent releases, but a
  lot of SLE packages fail without it.
* Mon Jul 29 2013 speilicke@suse.com
- Update to version 0.9.8:
  + Issue #53: Fix NameErrors in _vcs_split_rev_from_url.
- Changes from version 0.9.7:
  + Issue #49: Correct AttributeError on PyPy where a hashlib.HASH object does
    not have a .name attribute.
  + Issue #34: Documentation now refers to bootstrap script in code repository
    referenced by bookmark.
  + Add underscore-separated keys to environment markers (markerlib).
- Changes from version 0.9.6:
  + Issue #44: Test failure on Python 2.4 when MD5 hash doesn't have a .name
    attribute.
- Changes from version 0.9.5:
  + Python #17980: Fix security vulnerability in SSL certificate validation.
- Changes from version 0.9.4:
  + Issue #43: Fix issue (introduced in 0.9.1) with version resolution when
    upgrading over other releases of Setuptools.
- Changes from version 0.9.3:
  + Issue #42: Fix new AttributeError introduced in last fix.
- Changes from version 0.9.2:
  + Issue #42: Fix regression where blank checksums would trigger an
    AttributeError.
- Changes from version 0.9.1:
  + Distribute #386: Allow other positional and keyword arguments to os.open.
  + Corrected dependency on certifi mis-referenced in 0.9.
- Changes from version 0.9:
  + package_index now validates hashes other than MD5 in download links.
- Changes from version 0.8:
  + Code base now runs on Python 2.4 - Python 3.3 without Python 2to3
    conversion.
* Thu Jul 18 2013 speilicke@suse.com
- Use update-alternatives to be parallel-installable with python3-setuptools
* Tue Jun 25 2013 speilicke@suse.com
- Update to version 0.7.4:
  + Issue #20: Fix comparison of parsed SVN version on Python 3.
- Changes from version 0.7.3:
  + Issue #1: Disable installation of Windows-specific files on non-Windows systems.
  + Use new sysconfig module with Python 2.7 or >=3.2.
- Changes from version 0.7.2:
  + Issue #14: Use markerlib when the parser module is not available.
  + Issue #10: ez_setup.py now uses HTTPS to download setuptools from PyPI.
- Changes from version 0.7.1:
  + Fix NameError (Issue #3) again - broken in bad merge.
- Changes from version 0.7:
  + Merged Setuptools and Distribute. See docs/merge.txt for details.
  + Index URL now defaults to HTTPS.
  + Added experimental environment marker support. Now clients may designate a
    PEP-426 environment marker for "extra" dependencies. Setuptools uses this
    feature in setup.py for optional SSL and certificate validation support
    on older platforms. Based on Distutils-SIG discussions, the syntax is
    somewhat tentative. There should probably be a PEP with a firmer spec before
    the feature should be considered suitable for use.
  + Added support for SSL certificate validation when installing packages from
    an HTTPS service.
- Use upstream URL, SDPX style licenses
- Provide/obsolete python-distribute, which merged into setuptools-0.7.x
* Sat Dec 11 2010 saschpe@gmx.de
- re-generated spec file with py2pack based on old spec file:
  + now builds on Fedora and Mandriva
* Wed Sep  1 2010 jmatejek@novell.com
- update to dev snapshot 0.6c12 - RPM version is set
  to 0.6c11.99.r84273 so that version 0.6c12 will be an upgrade when
  it comes
- this should fix "AttributeError: 'NoneType' object has no attribute 'clone'"
  errors with python 2.7
* Tue Jan 12 2010 alexandre@exatati.com.br
- Removed obsolete python-setuptools-distutils-log.diff from sources.
* Tue Nov 10 2009 alexandre@exatati.com.br
- update to version 0.6c11
  + Fix "bdist_wininst upload" trying to upload same file twice
- additional changes from 0.6c10
  + Fix for the Python 2.6.3 build_ext API change
  + Ensure C libraries (as opposed to extensions) are also built when doing bdist_egg
  + Support for SVN 1.6
* Wed Nov  4 2009 matejcik@suse.cz
- removed obsolete python-setuptools-distutils-log patch
* Wed Aug 12 2009 matejcik@suse.cz
- improved noarch selection macro
* Fri Jul 31 2009 matejcik@suse.cz
- added noarch for suse > 11.1
* Fri Apr  3 2009 matejcik@suse.cz
- fixed spec compatibility with older versions
* Mon Mar 30 2009 matejcik@suse.cz
- update to 0.6c9
  + python 2.6 compatibility fixes
- added patch to create install path if it doesn't exist
- removed python-devel from requires
* Wed Oct 22 2008 skh@suse.de
- add python-setuptools-distutils-log.diff:
  setuptools/command/sdist.py: import distutils.log [bnc#428177]
- add python-setuptools-svn15.diff:
  setuptools/command/egg_info.py: recognize svn format version 1.5 magic
  number [also bnc#428177]
* Mon Aug 18 2008 cthiel@suse.de
- update to version 0.6c8
  + Prevent --help-commands and other junk from showing under Python 2.5
    when running easy_install --help.
  + Fixed GUI scripts sometimes not executing on Windows
  + Fixed not picking up dependency links from recursive dependencies.
  + Only make .py, .dll and .so files executable when unpacking
    eggs
  + Changes for Jython compatibility
- changes in version 0.6c7
  + ftp: download URLs now work correctly.
  + The default --index-url is now http://pypi.python.org/simple, to
    use the Python Package Index's new simpler (and faster!) REST API.
* Tue Aug  7 2007 cthiel@suse.de
- update to version 0.6c6
  + EasyInstall no longer aborts the installation process if a URL it wants
    to retrieve can't be downloaded, unless the URL is an actual package
    download.  Instead, it issues a warning and tries to keep going.
  + Fixed distutils-style scripts originally built on Windows having their
    line endings doubled when installed on any platform.
  + Added --local-snapshots-ok flag, to allow building eggs from
    projects installed using setup.py develop.
  + Fixed not HTML-decoding URLs scraped from web pages
* Fri Feb 16 2007 cthiel@suse.de
- initial package (version 0.6c5)
