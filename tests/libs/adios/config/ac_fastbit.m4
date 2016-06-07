AC_DEFUN([AC_FASTBIT], [

dnl Enable the --with-fastbit=path configure argument
AC_ARG_WITH(
  [fastbit],
  [AS_HELP_STRING(
    [--with-fastbit=DIR],
    [Location of the FastBit library]
  )]dnl
)

dnl If the lib was specified, verify that it exists and can compile
if test "x$with_fastbit" != xno; then
    FASTBIT_CPPFLAGS="-I$with_fastbit/include"
    FASTBIT_LDFLAGS="-L$with_fastbit/lib"
    FASTBIT_LIBS="-lfastbit"

    saveCPPFLAGS="$CPPFLAGS"
    saveLDFLAGS="$LDFLAGS"

    AC_LANG_PUSH([C++])

    CPPFLAGS="$CPPFLAGS $FASTBIT_CPPFLAGS"
    LDFLAGS="$LDFLAGS $FASTBIT_LDFLAGS $FASTBIT_LIBS"

    AC_CHECK_HEADERS(
      [iapi.h],
      [HAVE_FASTBIT=y],
      [HAVE_FASTBIT="";dnl
       AC_MSG_RESULT(
        [Cannot find iapi.h from the FastBit lib. Make sure it has been properly installed at the path specified ($with_fastbit).]dnl
      )]dnl
    )

dnl Removed this test because FastBit is all C++, and autoconf chokes on C++ lib linking tests
dnl    AC_CHECK_LIB(
dnl      [fastbit],
dnl      [ibis::gParameters],
dnl      [AC_DEFINE(
dnl        [HAVE_FASTBIT],
dnl        [1],
dnl        [Define if you have FastBit]
dnl      )],
dnl      [AC_MSG_FAILURE(
dnl        [Cannot successfully link with the FastBit lib. Make sure it has been properly installed at the path specified ($with_fastbit).]dnl
dnl      )],dnl
dnl    )
    
    CPPFLAGS="$saveCPPFLAGS"
    LDFLAGS="$saveLDFLAGS"

    AC_LANG_POP([C++])

    if test -z "$HAVE_FASTBIT"; then
      AM_CONDITIONAL(HAVE_FASTBIT,false)
      AC_MSG_RESULT([Not building with FastBit library])
    else
      AC_SUBST(FASTBIT_CPPFLAGS)
      AC_SUBST(FASTBIT_LDFLAGS)
      AC_SUBST(FASTBIT_LIBS)

      AM_CONDITIONAL(HAVE_FASTBIT,true)
      AC_DEFINE([HAVE_FASTBIT], [1], [Define if we have libfastbit])

      AC_MSG_RESULT([FastBit library found at $with_fastbit])
    fi
else
  AM_CONDITIONAL(HAVE_FASTBIT,false)

  AC_MSG_RESULT([Not building with FastBit library])
fi

]) dnl End of DEFUN
