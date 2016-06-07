dnl @synopsis AC_NSSI([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl
dnl This macro tries to find out how to compile programs that
dnl use the NSSI API.
dnl
dnl On success, it defines HAVE_NSSI and sets NSSI_LIBS
dnl to any libraries that are needed for linking
dnl NSSI (e.g. -lp3utcp, -lp3lib,...).
dnl
dnl If you want to compile everything with NSSI, you should set:
dnl
dnl     LIBS="$NSSI_LIBS $LIBS"
dnl
dnl ACTION-IF-FOUND is a list of shell commands to run if a NSSI
dnl library is found, and ACTION-IF-NOT-FOUND is a list of commands
dnl to run it if it is not found.  If ACTION-IF-FOUND is not specified,
dnl the default action will define HAVE_NSSI.
dnl
dnl @version $Id: acx_mpi.m4 676 2006-05-16 20:44:08Z raoldfi $
dnl @author Ron A. Oldfield <raoldfi@sandia.gov>

AC_DEFUN([AC_NSSI], [
AC_REQUIRE([AC_CANONICAL_HOST])
AC_REQUIRE([AC_PORTALS])
AC_REQUIRE([AC_INFINIBAND])
AC_REQUIRE([AC_PABLO])

AC_LANG_PUSH([C++])

dnl AC_LANG_SAVE
dnl AC_LANG_CPP

ac_nssi_ok=yes
ac_nssi_path="."


AM_CONDITIONAL(HAVE_NSSI,false)

NSSI_SRCDIR=""
NSSI_BUILDDIR=""
NSSI_CPPFLAGS=""
NSSI_LDFLAGS=""
NSSI_LIBS="-lnssi_client -lnssi_support"
NSSI_SERVER_LIBS="-lnssi_server -lnssi_support"

AC_ARG_WITH(nssi,
        [  --with-nssi=DIR      Location of NSSI],
        [NSSI_LDFLAGS="-L$withval/lib";
         NSSI_CPPFLAGS="-I$withval/include";
         ac_nssi_path=$withval])

dnl Look for a header file
if test x"$ac_nssi_ok" = xyes; then
        save_CPPFLAGS="$CPPFLAGS"
        CPPFLAGS="${CPPFLAGS} ${NSSI_CPPFLAGS}"
        AC_CHECK_HEADER(nssi_client.h,
                [],
                [ac_nssi_ok=no])
        CPPFLAGS="$save_CPPFLAGS"
fi

if test x"$ac_nssi_ok" = xyes; then

        AC_CHECK_LIB(rt, clock_gettime)

        AC_CHECK_FUNCS(clock_gettime gettimeofday fdatasync pthread_yield sched_yield)

        ac_nssi_ok=no

        dnl Determine which rpc libs to use with NSSI
        ac_nssi_flags="none"
        if test x"$ac_with_portals" = xyes; then
                ac_nssi_flags="$ac_nssi_flags all_portals portals_plus_libcpp portals_plus_pablo"
        fi
        if test x"$ac_with_infiniband" = xyes; then
                ac_nssi_flags="$ac_nssi_flags infiniband"
        fi

        for flag in $ac_nssi_flags; do

                case $flag in
                        none)
                        AC_MSG_CHECKING([whether nssi works without any additional flags])
                        ;;

                        -l*)
                        AC_MSG_CHECKING([whether nssi works with library $flag])
                        EXTRA_LIBS="$flag"
                        ;;

                        -*)
                        AC_MSG_CHECKING([whether nssi works with $flag])
                        EXTRA_LIBS="$flag"
                        ;;

                        all_portals)
                        AC_MSG_CHECKING([whether nssi works with ${PORTALS_LIBS}])
                        EXTRA_LIBS="${PORTALS_LIBS}"
                        ;;

                        portals_plus_libcpp)
                        AC_MSG_CHECKING([whether nssi works with ${PORTALS_LIBS}])
                        EXTRA_LIBS="${PORTALS_LIBS}"
                        ;;

                        portals_plus_pablo)
                        AC_MSG_CHECKING([whether nssi works with ${PORTALS_LIBS} ${PABLO_LIBS}])
                        EXTRA_LIBS="${PORTALS_LIBS} ${PABLO_LIBS}"
                        ;;

                        infiniband)
                        AC_MSG_CHECKING([whether nssi works with ${INFINIBAND_LIBS}])
                        EXTRA_LIBS="${INFINIBAND_LIBS}"
                        ;;
                esac


                save_LDFLAGS="$LDFLAGS"
                save_LIBS="$LIBS"
                save_CPPFLAGS="$CPPFLAGS"
                save_CFLAGS="$CFLAGS"
                LDFLAGS="$LDFLAGS $NSSI_LDFLAGS"
                LIBS="$LIBS $NSSI_LIBS $EXTRA_LIBS"
                CPPFLAGS="$CPPFLAGS $NSSI_CPPFLAGS"
                CFLAGS="$CFLAGS $NSSI_CFLAGS $EXTRA_FLAGS"

                dnl AC_MSG_CHECKING([CPPFLAGS=$CPPFLAGS])
                dnl AC_MSG_CHECKING([LDFLAGS=$LDFLAGS])
                dnl AC_MSG_CHECKING([LIBS=$LIBS])

                dnl Check for various functions.
                AC_LINK_IFELSE([AC_LANG_PROGRAM(
                            [[#include "nssi_client.h"]],
                            [[nssi_remote_pid server_id;]
                             [uint64_t timeout;]
                             [nssi_service svc;]
                             [nssi_get_service(server_id, timeout, &svc);]])],
                        [ac_nssi_ok=yes;],
                        [ac_nssi_ok=no;])


                LDFLAGS="$save_LDFLAGS"
                LIBS="$save_LIBS"
                CFLAGS="$save_CFLAGS"
                CPPFLAGS="$save_CPPFLAGS"

                AC_MSG_RESULT($ac_nssi_ok)
                if test "x$ac_nssi_ok" = xyes; then
                        NSSI_LIBS="$NSSI_LIBS $EXTRA_LIBS";
                        NSSI_SERVER_LIBS="$NSSI_SERVER_LIBS $EXTRA_LIBS"
                        NSSI_CFLAGS="$NSSI_CFLAGS $EXTRA_CFLAGS";
                        break;
                fi

                EXTRA_LIBS=""
                EXTRA_CFLAGS=""
        done
fi

AM_CONDITIONAL(HAVE_NSSI, test x$ac_nssi_ok = xyes)

AC_SUBST(NSSI_LIBS)
AC_SUBST(NSSI_SERVER_LIBS)
AC_SUBST(NSSI_CFLAGS)
AC_SUBST(NSSI_CPPFLAGS)
AC_SUBST(NSSI_LDFLAGS)
AC_SUBST(NSSI_SRCDIR)
AC_SUBST(NSSI_BUILDDIR)

AC_LANG_POP([C++])

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$ac_nssi_ok" = xyes; then
        ifelse([$1],,[AC_DEFINE(HAVE_NSSI,1,[Define if you have NSSI.])],[$1])
        :
else
        $2
        :
fi

dnl AC_LANG_RESTORE

])dnl AC_NSSI
