#----------------------------------------------------------------------------bh-
# This RPM .spec file is part of the OpenHPC project.
#
# It may have been modified from the default version supplied by the underlying
# release package (if available) in order to apply patches, perform customized
# build/install configurations, and supply additional files to support
# desired integration conventions.
#
#----------------------------------------------------------------------------eh-

%include %{_sourcedir}/OHPC_macros
%{!?PROJ_DELIM: %global PROJ_DELIM -ohpc}

#
# spec file for package python-Cython
#
# Copyright (c) 2016 SUSE LINUX GmbH, Nuernberg, Germany.
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

%define pname Cython
Name:           python-%{pname}%{PROJ_DELIM}
Version:        0.23.4
Release:        76.1
Url:            http://www.cython.org
Summary:        The Cython compiler for writing C extensions for the Python language
License:        Apache-2.0
Group:          Development/Languages/Python
Source:         http://pypi.python.org/packages/source/C/Cython/Cython-%{version}.tar.gz
Source1:        python-Cython-rpmlintrc
Patch1:         python-Cython-c++11.patch
BuildRoot:      %{_tmppath}/%{name}-%{version}-build
%if 0%{?sles_version} || 0%{?suse_version}
BuildRequires:  fdupes
BuildRequires:  python-xml
Requires:       python-xml
Requires(post): update-alternatives
Requires(postun): update-alternatives
%else
BuildRequires:  libxml2-python
Requires:       libxml2-python
%endif
BuildRequires:  gcc-c++
BuildRequires:  python-devel
Requires:       python-devel
Provides:       python-cython = %{version}
Obsoletes:      python-cython < %{version}
%if 0%{?suse_version} && 0%{?suse_version} <= 1110
%{!?python_sitearch: %global python_sitearch %(python -c "from distutils.sysconfig import get_python_lib; print get_python_lib(1)")}
%endif

%description
The Cython language makes writing C extensions for the Python language as
easy as Python itself.  Cython is a source code translator based on the
well-known Pyrex, but supports more cutting edge functionality and
optimizations.

The Cython language is very close to the Python language (and most Python
code is also valid Cython code), but Cython additionally supports calling C
functions and declaring C types on variables and class attributes. This
allows the compiler to generate very efficient C code from Cython code.

This makes Cython the ideal language for writing glue code for external C
libraries, and for fast C modules that speed up the execution of Python
code.

%prep
%setup -q -n %{pname}-%{version}
%patch1
# Fix non-executable scripts
sed -i "s|^#!.*||" Cython/Debugger/{libpython,Cygdb}.py cython.py
# Fix EOL encoding
sed -i "s|\r||" Demos/callback/{README.txt,cheesefinder.h} Demos/embed/Makefile.{unix,msc.static} Doc/primes.c

%build
CFLAGS="%{optflags}" python setup.py build

%install
python setup.py install --prefix=%{_prefix} --root=%{buildroot}

# Prepare for update-alternatives usage
mkdir -p %{buildroot}%{_sysconfdir}/alternatives
for p in cython cythonize cygdb ; do
    mv %{buildroot}%{_bindir}/$p %{buildroot}%{_bindir}/$p-%{py_ver}
    ln -s -f %{_sysconfdir}/alternatives/$p %{buildroot}%{_bindir}/$p
    # create a dummy target for /etc/alternatives/$p
    touch %{buildroot}%{_sysconfdir}/alternatives/$p
done

%if 0%{?sles_version} || 0%{?suse_version}
%fdupes -s %{buildroot}%{python_sitearch} %{buildroot}%{_docdir}
%endif
rm -rf %{buildroot}%{python_sitearch}/__pycache__/

%post
%_sbindir/update-alternatives \
   --install %{_bindir}/cython cython %{_bindir}/cython-%{py_ver} 30 \
   --slave %{_bindir}/cythonize cythonize %{_bindir}/cythonize-%{py_ver} \
   --slave %{_bindir}/cygdb cygdb %{_bindir}/cygdb-%{py_ver}

%postun
if [ $1 -eq 0 ] ; then
    %_sbindir/update-alternatives --remove cython %{_bindir}/cython-%{py_ver}
fi

%check
%if 0%{?suse_version} && 0%{?suse_version} <= 1140
sed -i.SLES11.SP4.bak -e 's/const char/char/' ./tests/run/cpdef_extern_func.pyx
#mv ./tests/run/cpdef_extern_func.pxd ./tests/run/cpdef_extern_func.pxd.TNT.txt
#mv ./tests/run/cpdef_extern_func.pyx ./tests/run/cpdef_extern_func.pyx.TNT.txt
#sleep 60
%endif
python runtests.py -vv

%files
%defattr(-,root,root,-)
%doc COPYING.txt LICENSE.txt README.txt ToDo.txt USAGE.txt Doc Demos
%{_bindir}/cygdb
%{_bindir}/cython
%{_bindir}/cythonize
%{_bindir}/cygdb-%{py_ver}
%{_bindir}/cython-%{py_ver}
%{_bindir}/cythonize-%{py_ver}
%ghost %{_sysconfdir}/alternatives/cygdb
%ghost %{_sysconfdir}/alternatives/cython
%ghost %{_sysconfdir}/alternatives/cythonize
%{python_sitearch}/Cython/
%{python_sitearch}/Cython-%{version}-py*.egg-info
%{python_sitearch}/cython.py*
%{python_sitearch}/pyximport/

%changelog
* Mon Feb  1 2016 toddrme2178@gmail.com
- update to version 0.23.4:
  * Memory leak when calling Python functions in PyPy.
  * Compilation problem with MSVC in C99-ish mode.
  * Warning about unused values in a helper macro.
- update to version 0.23.3:
  * Invalid C code for some builtin methods. This fixes ticket 856
    again.
  * Incorrect C code in helper functions for PyLong conversion and
    string decoding. This fixes ticket 863, ticket 864 and ticket
    865. Original patch by Nikolaus Rath.
  * Large folded or inserted integer constants could use too small C
    integer types and thus trigger a value wrap-around.
  * The coroutine and generator types of Cython now also register
    directly with the Coroutine and Generator ABCs in the
    backports_abc module if it can be imported. This fixes ticket 870.
- update to version 0.23.2:
  * Compiler crash when analysing some optimised expressions.
  * Coverage plugin was adapted to coverage.py 4.0 beta 2.
  * C++ destructor calls could fail when '&' operator is overwritten.
  * Incorrect C literal generation for large integers in compile-time
    evaluated DEF expressions and constant folded expressions.
  * Byte string constants could end up as Unicode strings when
    originating from compile-time evaluated DEF expressions.
  * Invalid C code when caching known builtin methods. This fixes
    ticket 860.
  * ino_t in posix.types was not declared as unsigned.
  * Declarations in libcpp/memory.pxd were missing operator!(). Patch
    by Leo Razoumov.
  * Static cdef methods can now be declared in .pxd files.
- update to version 0.23.1:
  * Invalid C code for generators. This fixes ticket 858.
  * Invalid C code for some builtin methods. This fixes ticket 856.
  * Invalid C code for unused local buffer variables. This fixes
    ticket 154.
  * Test failures on 32bit systems. This fixes ticket 857.
  * Code that uses "from xyz import *" and global C struct/union/array
    variables could fail to compile due to missing helper
    functions. This fixes ticket 851.
  * Misnamed PEP 492 coroutine property cr_yieldfrom renamed to
    cr_await to match CPython.
  * Missing deallocation code for C++ object attributes in certain
    extension class hierarchies.
  * Crash when async coroutine was not awaited.
  * Compiler crash on yield in signature annotations and default
    argument values. Both are forbidden now.
  * Compiler crash on certain constructs in finally clauses.
  * Cython failed to build when CPython's pgen is installed.
- update to version 0.23:
  * Features added
    + PEP 492 (async/await) was implemented. See
    https://www.python.org/dev/peps/pep-0492/
    + PEP 448 (Additional Unpacking Generalizations) was
    implemented. See https://www.python.org/dev/peps/pep-0448/
    + Support for coverage.py 4.0+ can be enabled by adding the plugin
    "Cython.Coverage" to the ".coveragerc" config file.
    + Annotated HTML source pages can integrate (XML) coverage
    reports.
    + Tracing is supported in nogil functions/sections and module init
    code.
    + When generators are used in a Cython module and the module
    imports the modules "inspect" and/or "asyncio", Cython enables
    interoperability by patching these modules during the import to
    recognise Cython's internal generator and coroutine types. This
    can be disabled by C compiling the module with "-D
    CYTHON_PATCH_ASYNCIO=0" or "-D CYTHON_PATCH_INSPECT=0"
    + When generators or coroutines are used in a Cython module, their
    types are registered with the Generator and Coroutine ABCs in
    the collections or collections.abc stdlib module at import time
    to enable interoperability with code that needs to detect and
    process Python generators/coroutines. These ABCs were added in
    CPython 3.5 and are available for older Python versions through
    the backports_abc module on PyPI. See
    https://bugs.python.org/issue24018
    + Adding/subtracting/dividing/modulus and equality comparisons
    with constant Python floats and small integers are faster.
    + Binary and/or/xor/rshift operations with small constant Python
    integers are faster.
    + When called on generator expressions, the builtins all(), any(),
    dict(), list(), set(), sorted() and unicode.join() avoid the
    generator iteration overhead by inlining a part of their
    functionality into the for-loop.
    + Keyword argument dicts are no longer copied on function entry
    when they are not being used or only passed through to other
    function calls (e.g. in wrapper functions).
    + The PyTypeObject declaration in cpython.object was extended.
    + The builtin type type is now declared as PyTypeObject in source,
    allowing for extern functions taking type parameters to have the
    correct C signatures. Note that this might break code that uses
    type just for passing around Python types in typed
    variables. Removing the type declaration provides a backwards
    compatible fix.
    + wraparound() and boundscheck() are available as no-ops in pure
    Python mode.
    + Const iterators were added to the provided C++ STL declarations.
    + Smart pointers were added to the provided C++ STL
    declarations. Patch by Daniel Filonik.
    + NULL is allowed as default argument when embedding
    signatures. This fixes ticket 843.
    + When compiling with --embed, the internal module name is changed
    to __main__ to allow arbitrary program names, including those
    that would be invalid for modules. Note that this prevents reuse
    of the generated C code as an importable module.
    + External C++ classes that overload the assignment operator can
    be used. Patch by Ian Henriksen.
    + Support operator bool() for C++ classes so they can be used in
    if statements.
  * Bugs fixed
    + Calling "yield from" from Python on a Cython generator that
    returned a value triggered a crash in CPython. This is now being
    worked around. See https://bugs.python.org/issue23996
    + Language level 3 did not enable true division (a.k.a. float
    division) for integer operands.
    + Functions with fused argument types that included a generic
    'object' fallback could end up using that fallback also for
    other explicitly listed object types.
    + Relative cimports could accidentally fall back to trying an
    absolute cimport on failure.
    + The result of calling a C struct constructor no longer requires
    an intermediate assignment when coercing to a Python dict.
    + C++ exception declarations with mapping functions could fail to
    compile when pre-declared in .pxd files.
    + cpdef void methods are now permitted.
    + abs(cint) could fail to compile in MSVC and used sub-optimal
    code in C++. Patch by David Vierra, original patch by Michael
    Enßlin.
    + Buffer index calculations using index variables with small C
    integer types could overflow for large buffer sizes. Original
    patch by David Vierra.
    + C unions use a saner way to coerce from and to Python dicts.
    + When compiling a module foo.pyx, the directories in sys.path are
    no longer searched when looking for foo.pxd. Patch by Jeroen
    Demeyer.
    + Memory leaks in the embedding main function were fixed. Original
    patch by Michael Enßlin.
    + Some complex Python expressions could fail to compile inside of
    finally clauses.
    + Unprefixed 'str' literals were not supported as C varargs
    arguments.
    + Fixed type errors in conversion enum types to/from Python. Note
    that this imposes stricter correctness requirements on enum
    declarations.
  * Other changes
    + Changed mangling scheme in header files generated by cdef api
    declarations.
    + Installation under CPython 3.3+ no longer requires a pass of the
    2to3 tool. This also makes it possible to run Cython in Python
    3.3+ from a source checkout without installing it first. Patch
    by Petr Viktorin.
    + jedi-typer.py (in Tools/) was extended and renamed to
    jedityper.py (to make it importable) and now works with and
    requires Jedi 0.9. Patch by Tzer-jen Wei.
* Thu Jan 28 2016 rguenther@suse.com
- Add python-Cython-c++11.patch to fix complex math testcase compile
  with GCC 6 defaulting to C++14.  (bnc#963974)
* Wed Aug 26 2015 ted.nokonechny@uregina.ca
- "mangle" tests/run/cpdef_extern_func.pyx to allow package to build
  for SLE_11_SP3 and SLE_11_SP4.
  * cpdef const char* strchr(const char *haystack, int needle);
    does not seem to match any from /usr/include/string.h
    and fails for cpp tests via python runtests.py -vv
  * cpdef_extern_func.pyx was not present in 0.21.1
* Wed Aug 19 2015 bwiedemann@suse.com
- Require python-devel for Python.h (bnc#942385)
* Wed Jul 29 2015 toddrme2178@gmail.com
- Remove unneeded numpy dependency to avoid dependency loop.
- create dummy alternative to avoid 13.1's post-build-check bug
- specfile:
  * fixing update_alternatives
* Fri Jul 10 2015 termim@gmail.com
- Update to 0.22.1:
  Bugs fixed
  * Crash when returning values on generator termination.
  * In some cases, exceptions raised during internal isinstance()
    checks were not propagated.
  * Runtime reported file paths of source files (e.g for profiling
    and tracing) are now relative to the build root directory instead
    of the main source file.
  * Tracing exception handling code could enter the trace function with an
    active exception set.
  * The internal generator function type was not shared across modules.
  * Comparisons of (inferred) ctuples failed to compile.
  * Closures inside of cdef functions returning ``void`` failed to compile.
  * Using ``const`` C++ references in intermediate parts of longer
    expressions could fail to compile.
  * C++ exception declarations with mapping functions could fail to compile
    when pre-declared in .pxd files.
  * C++ compilation could fail with an ambiguity error in recent MacOS-X
    Xcode versions.
  * C compilation could fail in pypy3.
  * Fixed a memory leak in the compiler when compiling multiple modules.
  * When compiling multiple modules, external library dependencies could
    leak into later compiler runs.  Fix by Jeroen Demeyer.  This fixes
    ticket 845.
- removed patch fix-32bit.patch as applied upstream
* Wed Apr 22 2015 mcihar@suse.cz
- Use Source URL from cython.org
* Wed Apr 22 2015 mcihar@suse.cz
- Add python-numpy as BuildRequires to have more complete test coverage
* Wed Apr 22 2015 mcihar@suse.cz
- Fix doctests in 32-bit environment (fix-32bit.patch)
* Wed Apr 22 2015 mcihar@suse.cz
- Update to 0.22:
  Features added
  * C functions can coerce to Python functions, which allows passing them
    around as callable objects.
  * C arrays can be assigned by value and auto-coerce from Python iterables
    and to Python lists (and tuples).
  * Extern C functions can now be declared as cpdef to export them to
    the module's Python namespace.  Extern C functions in pxd files export
    their values to their own module, iff it exists.
  * Anonymous C tuple types can be declared as (ctype1, ctype2, ...).
  * PEP 479: turn accidental StopIteration exceptions that exit generators
    into a RuntimeError, activated with future import "generator_stop".
    See http://legacy.python.org/dev/peps/pep-0479/
  * Looping over ``reversed(range())`` is optimised in the same way as
    ``range()``.  Patch by Favian Contreras.
  Bugs fixed
  * Mismatching 'except' declarations on signatures in .pxd and .pyx files failed
    to produce a compile error.
  * Failure to find any files for the path pattern(s) passed into ``cythonize()``
    is now an error to more easily detect accidental typos.
  * The ``logaddexp`` family of functions in ``numpy.math`` now has correct
    declarations.
  * In Py2.6/7 and Py3.2, simple Cython memory views could accidentally be
    interpreted as non-contiguous by CPython, which could trigger a CPython
    bug when copying data from them, thus leading to data corruption.
    See CPython issues 12834 and 23349.
  Other changes
  * Preliminary support for defining the Cython language with a formal grammar.
    To try parsing your files against this grammar, use the --formal_grammar directive.
    Experimental.
  * ``_`` is no longer considered a cacheable builtin as it could interfere with
    gettext.
  * Cythonize-computed metadata now cached in the generated C files.
* Thu Feb  5 2015 hpj@urpla.net
- fix update-alternatives handling in a distribution backwards compatible way
* Fri Jan  9 2015 dimstar@opensuse.org
- Re-enable test-suite.
- Add gcc-c++ BuildRequires: needed for the test-suite to be able
  to pass.
* Thu Jan  8 2015 dimstar@opensuse.org
- Fix usage of update-alternatives.
* Thu Dec 18 2014 p.drouand@gmail.com
- Improve update-alternatives.
- Remove Cython-fix-version-detection.patch
  (got fixed upstream)
- update to version 0.21.1:
  * Features added
  - New cythonize option -a to generate the annotated HTML source view.
  - Missing C-API declarations in cpython.unicode were added.
  - Passing language='c++' into cythonize() globally enables C++ mode
    for all modules that were not passed as Extension objects
    (i.e. only source files and file patterns).
  - Py_hash_t is a known type (used in CPython for hash values).
  - PySlice_*() C-API functions are available from the cpython.slice module.
  - Allow arrays of C++ classes.
  * Bugs fixed
  - Reference leak for non-simple Python expressions in boolean and/or expressions.
  - To fix a name collision and to reflect availability on host
    platforms, standard C declarations [ clock(), time(), struct tm
    and tm* functions ] were moved from posix/time.pxd to a new
    libc/time.pxd. Patch by Charles Blake.
  - Rerunning unmodified modules in IPython's cython support
    failed. Patch by Matthias Bussonier.
  - Casting C++ std::string to Python byte strings failed when
    auto-decoding was enabled.
  - Fatal exceptions in global module init code could lead to
    crashes if the already created module was used later on
    (e.g. through a stale reference in sys.modules or elsewhere).
  - cythonize.py script was not installed on MS-Windows.
  * Other changes
  - Compilation no longer fails hard when unknown compilation
    options are passed. Instead, it raises a warning and ignores
    them (as it did silently before 0.21). This will be changed back
    to an error in a future release.
* Sun Nov  9 2014 Led <ledest@gmail.com>
- fix bashisms in pre script
* Fri Sep 12 2014 toddrme2178@gmail.com
- Add Cython-fix-version-detection.patch
  This is a patch from upstream that restores version information
  whose removal is preventing several packages from correctly
  detecting Cython's presence.  It is already merged upstream and
  so should be in the next release.
  Note that despite what upstream says,
  python-tables/python3-tables is NOT the only package affected by
  this, which is why the patch is going here instead of
  python-tables/python3-tables.
  python-bcolz/python3-bcolz is an example of another package
  affected.
* Thu Sep 11 2014 toddrme2178@gmail.com
- Update to 0.21 (2014-09-10)
  * Features added
  * C (cdef) functions allow inner Python functions.
  * Enums can now be declared as cpdef to export their values to
    the module's Python namespace.  Cpdef enums in pxd files export
    their values to their own module, iff it exists.
  * Allow @staticmethod decorator to declare static cdef methods.
    This is especially useful for declaring "constructors" for
    cdef classes that can take non-Python arguments.
  * Taking a ``char*`` from a temporary Python string object is safer
    in more cases and can be done inside of non-trivial expressions,
    including arguments of a function call.  A compile time error
    is raised only when such a pointer is assigned to a variable and
    would thus exceed the lifetime of the string itself.
  * Generators have new properties ``__name__`` and ``__qualname__``
    that provide the plain/qualified name of the generator function
    (following CPython 3.5).  See http://bugs.python.org/issue21205
  * The ``inline`` function modifier is available as a decorator
    ``@cython.inline`` in pure mode.
  * When cygdb is run in a virtualenv, it enables the same virtualenv
    inside of the debugger. Patch by Marc Abramowitz.
  * PEP 465: dedicated infix operator for matrix multiplication (A @ B).
  * HTML output of annotated code uses Pygments for code highlighting
    and generally received a major overhaul by Matthias Bussonier.
  * IPython magic support is now available directly from Cython with
    the command "%%load_ext cython".  Cython code can directly be
    executed in a cell when marked with "%%%%cython".  Code analysis
    is available with "%%%%cython -a".  Patch by Martín Gaitán.
  * Simple support for declaring Python object types in Python signature
    annotations.  Currently requires setting the compiler directive
    ``annotation_typing=True``.
  * New directive ``use_switch`` (defaults to True) to optionally disable
    the optimization of chained if statement to C switch statements.
  * Defines dynamic_cast et al. in ``libcpp.cast`` and C++ heap data
    structure operations in ``libcpp.algorithm``.
  * Shipped header declarations in ``posix.*`` were extended to cover
    more of the POSIX API.  Patches by Lars Buitinck and Mark Peek.
  * Optimizations
  * Simple calls to C implemented Python functions/methods are faster.
    This also speeds up many operations on builtins that Cython cannot
    otherwise optimise.
  * The "and"/"or" operators try to avoid unnecessary coercions of their
    arguments.  They now evaluate the truth value of each argument
    independently and only coerce the final result of the whole expression
    to the target type (e.g. the type on the left side of an assignment).
    This also avoids reference counting overhead for Python values during
    evaluation and generally improves the code flow in the generated C code.
  * The Python expression "2 ** N" is optimised into bit shifting.
    See http://bugs.python.org/issue21420
  * Cascaded assignments (a = b = ...) try to minimise the number of
    type coercions.
  * Calls to ``slice()`` are translated to a straight C-API call.
  * Bugs fixed
  * Crash when assigning memory views from ternary conditional expressions.
  * Nested C++ templates could lead to unseparated ">>" characters being
    generated into the C++ declarations, which older C++ compilers could
    not parse.
  * Sending SIGINT (Ctrl-C) during parallel cythonize() builds could
    hang the child processes.
  * No longer ignore local setup.cfg files for distutils in pyximport.
    Patch by Martin Teichmann.
  * Taking a ``char*`` from an indexed Python string generated unsafe
    reference counting code.
  * Set literals now create all of their items before trying to add them
    to the set, following the behaviour in CPython.  This makes a
    difference in the rare case that the item creation has side effects
    and some items are not hashable (or if hashing them has side effects,
    too).
  * Cython no longer generates the cross product of C functions for code
    that uses memory views of fused types in function signatures (e.g.
    ``cdef func(floating[:] a, floating[:] b)``).  This is considered the
    expected behaviour by most users and was previously inconsistent with
    other structured types like C arrays.  Code that really wants all type
    combinations can create the same fused memoryview type under different
    names and use those in the signature to make it clear which types are
    independent.
  * Names that were unknown at compile time were looked up as builtins at
    runtime but not as global module names.  Trying both lookups helps with
    globals() manipulation.
  * Fixed stl container conversion for typedef element types.
  * ``obj.pop(x)`` truncated large C integer values of x to ``Py_ssize_t``.
  * ``__init__.pyc`` is recognised as marking a package directory
    (in addition to .py, .pyx and .pxd).
  * Syntax highlighting in ``cython-mode.el`` for Emacs no longer
    incorrectly highlights keywords found as part of longer names.
  * Correctly handle ``from cython.submodule cimport name``.
  * Fix infinite recursion when using super with cpdef methods.
  * No-args ``dir()`` was not guaranteed to return a sorted list.
  * Other changes
  * The header line in the generated C files no longer contains the
    timestamp but only the Cython version that wrote it.  This was
    changed to make builds more reproducible.
  * Removed support for CPython 2.4, 2.5 and 3.1.
  * The licensing implications on the generated code were clarified
    to avoid legal constraints for users.
* Thu Jul 31 2014 dimstar@opensuse.org
- Rename rpmlintrc to %%{name}-rpmlintrc.
  Follow the packaging guidelines.
* Thu Jul 24 2014 mcihar@suse.cz
- Update to version 0.20.2:
  * Some optimisations for set/frozenset instantiation.
  * Support for C++ unordered_set and unordered_map.
  * Access to attributes of optimised builtin methods (e.g.
    ``[].append.__name__``) could fail to compile.
  * Memory leak when extension subtypes add a memory view as attribute
    to those of the parent type without having Python object attributes
    or a user provided dealloc method.
  * Compiler crash on readonly properties in "binding" mode.
  * Auto-encoding with ``c_string_encoding=ascii`` failed in Py3.3.
  * Crash when subtyping freelist enabled Cython extension types with
    Python classes that use ``__slots__``.
  * Freelist usage is restricted to CPython to avoid problems with other
    Python implementations.
  * Memory leak in memory views when copying overlapping, contiguous slices.
  * Format checking when requesting non-contiguous buffers from
    ``cython.array`` objects was disabled in Py3.
  * C++ destructor calls in extension types could fail to compile in clang.
  * Buffer format validation failed for sequences of strings in structs.
  * Docstrings on extension type attributes in .pxd files were rejected.
- add python-xml to build requirements for testsuite
* Thu May  8 2014 toddrme2178@gmail.com
- Update to version 0.20.1
  * List/Tuple literals multiplied by more than one factor were only multiplied
    by the last factor instead of all.
  * Lookups of special methods (specifically for context managers) could fail
    in Python <= 2.6/3.1.
  * Local variables were erroneously appended to the signature introspection
    of Cython implemented functions with keyword-only arguments under Python 3.
  * In-place assignments to variables with inferred Python builtin/extension
    types could fail with type errors if the result value type was incompatible
    with the type of the previous value.
  * The C code generation order of cdef classes, closures, helper code,
    etc. was not deterministic, thus leading to high code churn.
  * Type inference could fail to deduce C enum types.
  * Type inference could deduce unsafe or inefficient types from integer
    assignments within a mix of inferred Python variables and integer
    variables.
* Mon Feb  3 2014 jengelh@inai.de
- Cython grew a dependency on saxutils (and since then, libplist
  failed to build). Add python-xml as a Requires to avoid:
  [...]
  File "/usr/lib64/python2.7/site-packages/Cython/Compiler/Annotate.py",
  line 6, in <module> from xml.sax.saxutils import escape as
  html_escape.
  ImportError: No module named xml.sax.saxutils
* Fri Jan 31 2014 speilicke@suse.com
- Update to version 0.20:
  * Support for CPython 3.4.
  * Support for calling C++ template functions.
  * yield is supported in finally clauses.
  * The C code generated for finally blocks is duplicated for each exit
    case to allow for better optimisations by the C compiler.
  * Cython tries to undo the Python optimisationism of assigning a bound
    method to a local variable when it can generate better code for the
    direct call.
  * Constant Python float values are cached.
  * String equality comparisons can use faster type specific code in
    more cases than before.
  * String/Unicode formatting using the '%%' operator uses a faster
    C-API call.
  * bytearray has become a known type and supports coercion from and
    to C strings.  Indexing, slicing and decoding is optimised. Note that
    this may have an impact on existing code due to type inference.
  * Using cdef basestring stringvar and function arguments typed as
    basestring is now meaningful and allows assigning exactly
    str and unicode objects, but no subtypes of these types.
  * Support for the __debug__ builtin.
  * Assertions in Cython compiled modules are disabled if the running
    Python interpreter was started with the "-O" option.
  * Some types that Cython provides internally, such as functions and
    generators, are now shared across modules if more than one Cython
    implemented module is imported.
  * The type inference algorithm works more fine granular by taking the
    results of the control flow analysis into account.
  * A new script in bin/cythonize provides a command line frontend
    to the cythonize() compilation function (including distutils build).
  * The new extension type decorator @cython.no_gc_clear prevents
    objects from being cleared during cyclic garbage collection, thus
    making sure that object attributes are kept alive until deallocation.
  * During cyclic garbage collection, attributes of extension types that
    cannot create reference cycles due to their type (e.g. strings) are
    no longer considered for traversal or clearing.  This can reduce the
    processing overhead when searching for or cleaning up reference cycles.
  * Package compilation (i.e. __init__.py files) now works, starting
    with Python 3.3.
  * The cython-mode.el script for Emacs was updated.  Patch by Ivan Andrus.
  * An option common_utility_include_dir was added to cythonize() to save
    oft-used utility code once in a separate directory rather than as
    part of each generated file.
  * unraisable_tracebacks directive added to control printing of
    tracebacks of unraisable exceptions.
* Mon Oct 21 2013 dmueller@suse.com
- update to 0.19.2:
  * Some standard declarations were fixed or updated, including the previously
    incorrect declaration of PyBuffer_FillInfo() and some missing bits in
    libc.math.
  * Heap allocated subtypes of type used the wrong base type struct at the
    C level.
  * Calling the unbound method dict.keys/value/items() in dict subtypes could
    call the bound object method instead of the unbound supertype method.
  * "yield" wasn't supported in "return" value expressions.
  * Using the "bint" type in memory views lead to unexpected results.
    It is now an error.
  * Assignments to global/closure variables could catch them in an illegal state
    while deallocating the old value.
* Fri Oct 11 2013 p.drouand@gmail.com
- Implement update-alternatives
* Mon May 13 2013 dmueller@suse.com
- update to 0.19.1:
  * Completely empty C-API structs for extension type slots (protocols like
  number/mapping/sequence) are no longer generated into the C code.
  * Docstrings that directly follow a public/readonly attribute declaration
  in a cdef class will be used as docstring of the auto-generated property.
  This fixes ticket 206.
  * The automatic signature documentation tries to preserve more semantics
  of default arguments and argument types.  Specifically, bint arguments
  now appear as type bool.
  * A warning is emitted when negative literal indices are found inside of
  a code section that disables wraparound handling.  This helps with
  fixing invalid code that might fail in the face of future compiler
  optimisations.
  * Constant folding for boolean expressions (and/or) was improved.
  * Added a build_dir option to cythonize() which allows one to place
  the generated .c files outside the source tree.
* Mon Apr 29 2013 dmueller@suse.com
- Update to version 0.19:
  + Please see http://wiki.cython.org/ReleaseNotes-0.19
* Wed Mar 27 2013 speilicke@suse.com
- Update to version 0.18:
  + Please see http://wiki.cython.org/ReleaseNotes-0.18
* Thu Nov 22 2012 saschpe@suse.de
- Update to version 0.17.2:
  + Please see http://wiki.cython.org/ReleaseNotes-0.17.2
- Drop excessive macro usage
- No need for "-fno-strict-aliasing" anymore
- One rpmlintrc is enough
* Fri May 25 2012 toddrme2178@gmail.com
- Fix .py/.pyc issues
* Fri May 18 2012 toddrme2178@gmail.com
- Add python 3 package
- Clean up spec file formatting
- Remove setbadness from rplintrc files
* Mon Apr 23 2012 vdziewiecki@suse.com
-Update to 0.16:
  http://wiki.cython.org/ReleaseNotes-0.16
* Wed Feb 29 2012 pascal.bleser@opensuse.org
- add rpmlintrc to mask false positives
* Wed Sep 21 2011 saschpe@suse.de
- Update to version 0.15.1:
  * Please see http://wiki.cython.org/ReleaseNotes-0.15.1
* Tue Sep  6 2011 saschpe@suse.de
- Add Provides/Obsoletes for python-cython
* Fri Sep  2 2011 saschpe@suse.de
- Fixed a typo
- Removed testsuite again, fixes SLE build
* Fri Sep  2 2011 saschpe@suse.de
- Update to version 0.15:
  * For loop docs fix and pointer iteration.
  * Pure decorators now implemented.
  * fix bug #707: optimised dict iteration over non-trivial expressions fail...
  * optimise object.pop() for sets
  * Py2.4 fix: PySet_Pop() appeared in Py2.5
  * Py3.3 test fix
  * Support module level control flow and Entry-level error on uninitialized
- Spec file cleanup:
  - Fixed wrong EOL encodings and non-excutable scripts
  - Set license to Apache-2.0
  - Run testsuite
* Wed Apr 20 2011 prusnak@opensuse.org
- updated to 0.14.1
  - changes too numerous to list, see the following for more details:
  * http://wiki.cython.org/ReleaseNotes-0.13
  * http://wiki.cython.org/ReleaseNotes-0.14
  * http://wiki.cython.org/ReleaseNotes-0.14.1
* Sun Jun  6 2010 dimstar@opensuse.org
- Use renewed python-macros, also for compatibility with other
  build targets.
* Sun Jun  6 2010 dimstar@opensuse.org
- Initial package of Cython 0.12.1
