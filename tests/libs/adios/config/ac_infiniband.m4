dnl @synopsis AC_INFINIBAND([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl
dnl This macro tries to find out how to compile programs that
dnl use the Infiniband ibverbs API.
dnl
dnl On success, it defines HAVE_INFINIBAND and sets INFINIBAND_LIBS
dnl to any libraries that are needed for linking
dnl Infiniband (e.g. -libverbs,...).
dnl
dnl If you want to compile everything with Portals, you should set:
dnl
dnl     LIBS="$PTL_LIBS $LIBS"
dnl
dnl ACTION-IF-FOUND is a list of shell commands to run if a Infiniband
dnl library is found, and ACTION-IF-NOT-FOUND is a list of commands
dnl to run it if it is not found.  If ACTION-IF-FOUND is not specified,
dnl the default action will define HAVE_INFINIBAND.
dnl
dnl @version $Id: ac_infiniband.m4 676 2006-05-16 20:44:08Z thkorde $
dnl @author Todd H. Kordenbrock <thkorde@sandia.gov>

AC_DEFUN([AC_INFINIBAND], [
AC_REQUIRE([AC_CANONICAL_HOST])
AC_REQUIRE([ACX_PTHREAD])
AC_LANG_SAVE
AC_LANG_C
ac_infiniband_hdr_ok=no
ac_infiniband_lib_ok=no
ac_with_infiniband=no


INFINIBAND_CFLAGS=""
INFINIBAND_CPPFLAGS=""
INFINIBAND_LDFLAGS=""
INFINIBAND_LIBS=""

AC_ARG_WITH(infiniband,
        [  --with-infiniband=DIR      Location of Infiniband],
        [ INFINIBAND_DIR=$withval;], [with_infiniband=yes])



if test x"$with_infiniband" = xno; then

        ac_with_infiniband=no;

elif test x"$with_infiniband" = xyes -o x"$with_infiniband" = x; then

        INFINIBAND_CPPFLAGS="";
        INFINIBAND_LDFLAGS="";
        ac_with_infiniband=yes;

else

        INFINIBAND_CPPFLAGS="-I$withval/include";
        INFINIBAND_LDFLAGS="-L$withval/lib64 -L$withval/lib";
        ac_with_infiniband=yes;

fi

AM_CONDITIONAL(HAVE_INFINIBAND,test x$ac_with_infiniband = xyes)


dnl Check for command-line disable
if test x"$ac_with_infiniband" = xyes; then

        AC_MSG_NOTICE([=== checking for INFINIBAND ===])

        dnl Look for Infiniband header files
        save_CPPFLAGS=$CPPFLAGS;
        save_LDFLAGS=$LDFLAGS;
        CPPFLAGS="$CPPFLAGS $INFINIBAND_CPPFLAGS"
        LDFLAGS="$LDFLAGS $INFINIBAND_LDFLAGS"


        if test x"$ac_infiniband_hdr_ok" = xno; then
                AC_CHECK_HEADER(infiniband/verbs.h,
                         [AC_DEFINE(HAVE_IBVERBS_H, 1,
                                [Define to 1 if you have <infiniband/verbs.h>.])
                         ac_infiniband_hdr_ok=yes;
                         INFINIBAND_CFLAGS="$INFINIBAND_CFLAGS $EXTRA_CFLAGS";
                         INFINIBAND_CPPFLAGS="$INFINIBAND_CPPFLAGS $EXTRA_CFLAGS"],
                         [ac_infiniband_hdr_ok=no])
        fi

        if test x"$ac_infiniband_hdr_ok" = xno; then
                CPPFLAGS=$save_CPPFLAGS
        fi

        dnl Look for -libverbs
        if test x"$ac_infiniband_lib_ok" = xno -a x$ac_infiniband_hdr_ok = xyes; then
            save_LIBS=$LIBS;
            LIBS=""
            AC_SEARCH_LIBS(ibv_alloc_pd,[ibverbs],
                    [ac_infiniband_lib_ok=yes],
                    [ac_infiniband_lib_ok=no],
                    [$save_LIBS $PTHREAD_LDFLAGS $PTHREAD_LIBS])
            if test -n $LIBS; then
                INFINIBAND_LIBS="$INFINIBAND_LIBS $LIBS";
            fi
            LIBS=$save_LIBS;
        fi

        if test x"$ac_infiniband_hdr_ok" = xno -o x$ac_infiniband_lib_ok = xno; then
            AM_CONDITIONAL(HAVE_INFINIBAND,false)
            INFINIBAND_CFLAGS=""
            INFINIBAND_CPPFLAGS=""
            INFINIBAND_LDFLAGS=""
            INFINIBAND_LIBS=""
            ac_with_infiniband=no
        else
            AM_CONDITIONAL(HAVE_INFINIBAND,true)
        fi

        CPPFLAGS=$save_CPPFLAGS;
        LDFLAGS=$save_LDFLAGS;

        AC_SUBST(INFINIBAND_CPPFLAGS)
        AC_SUBST(INFINIBAND_CFLAGS)
        AC_SUBST(INFINIBAND_LDFLAGS)
        AC_SUBST(INFINIBAND_LIBS)


        # Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
        if test x"$ac_infiniband_lib_ok" = xyes; then
                ifelse([$1],,[AC_DEFINE(HAVE_INFINIBAND,1,[Define if you have the Infiniband.])],[$1])
                :
        else
                $2
                :
        fi

fi

AC_LANG_RESTORE
])dnl AC_INFINIBAND
