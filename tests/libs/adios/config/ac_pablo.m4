dnl @synopsis AC_PABLO([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl
dnl This macro tries to find out how to compile programs that
dnl use the Pablo API.
dnl
dnl On success, it defines HAVE_PABLO and sets PABLO_LIBS
dnl to any libraries that are needed for linking
dnl Pablo (e.g. -lp3utcp, -lp3lib,...).
dnl
dnl If you want to compile everything with Pablo, you should set:
dnl
dnl     LIBS="$PABLO_LIBS $LIBS"
dnl
dnl ACTION-IF-FOUND is a list of shell commands to run if a Portals
dnl library is found, and ACTION-IF-NOT-FOUND is a list of commands
dnl to run it if it is not found.  If ACTION-IF-FOUND is not specified,
dnl the default action will define HAVE_PORTALS.
dnl
dnl @version $Id: acx_mpi.m4 676 2006-05-16 20:44:08Z raoldfi $
dnl @author Ron A. Oldfield <raoldfi@sandia.gov>

AC_DEFUN([AC_PABLO], [

AM_CONDITIONAL(HAVE_PABLO,true)

AC_ARG_WITH(pablo,
	[  --with-pablo=DIR      Location of Pablo library],
	[PABLO_LDFLAGS="-L$withval/lib";
	 PABLO_CPPFLAGS="-I$withval/include";])

dnl AC_LANG_PUSH([C++])

save_CPPFLAGS="$CPPFLAGS"
save_LIBS="$LIBS"
save_LDFLAGS="$LDFLAGS"
LIBS="$LIBS -lPablo"
LDFLAGS="$LDFLAGS $PABLO_LDFLAGS"
CPPFLAGS="$CPPFLAGS $PABLO_CPPFLAGS"

if test -z "${HAVE_PABLO_TRUE}"; then
        AC_CHECK_HEADERS(Attributes.h,
		,
		[AM_CONDITIONAL(HAVE_PABLO,false)])
fi

# Check for the Pablo library and headers for SDDF
AC_TRY_COMPILE([#include <Attributes.h>],
	[Attributes attr;attr.clearEntries();],
	[PABLO_LIBS="-lPablo"],
	[AM_CONDITIONAL(HAVE_PABLO,false)])

LIBS="$save_LIBS"
LDFLAGS="$save_LDFLAGS"
CPPFLAGS="$save_CPPFLAGS"

AC_SUBST(PABLO_LIBS)
AC_SUBST(PABLO_LDFLAGS)
AC_SUBST(PABLO_CPPFLAGS)

dnl AC_LANG_POP([C++])

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test -z "${HAVE_PABLO_TRUE}"; then
        ifelse([$1],,[AC_DEFINE(HAVE_PABLO,1,[Define if you have the Pablo.])],[$1])
        :
else
        $2
        :
fi
])dnl AC_PABLO
