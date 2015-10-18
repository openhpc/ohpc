dnl @synopsis AC_PORTALS([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl
dnl This macro tries to find out how to compile programs that
dnl use the Portals API.
dnl
dnl On success, it defines HAVE_PORTALS and sets PORTALS_LIBS
dnl to any libraries that are needed for linking
dnl Portals (e.g. -lp3utcp, -lp3lib,...).
dnl
dnl If you want to compile everything with Portals, you should set:
dnl
dnl     LIBS="$PTL_LIBS $LIBS"
dnl
dnl ACTION-IF-FOUND is a list of shell commands to run if a Portals
dnl library is found, and ACTION-IF-NOT-FOUND is a list of commands
dnl to run it if it is not found.  If ACTION-IF-FOUND is not specified,
dnl the default action will define HAVE_PORTALS.
dnl
dnl @version $Id: acx_mpi.m4 676 2006-05-16 20:44:08Z raoldfi $
dnl @author Ron A. Oldfield <raoldfi@sandia.gov>

AC_DEFUN([AC_PORTALS], [
AC_REQUIRE([AC_CANONICAL_HOST])
AC_REQUIRE([ACX_PTHREAD])
AC_LANG_SAVE
AC_LANG_C
ac_portals_hdr_ok=no
ac_portals_lib_ok=no
ac_with_portals=no

AM_CONDITIONAL(HAVE_PORTALS,true)
AM_CONDITIONAL(NEED_DARWIN_SINGLE_MODULE,false)

PORTALS_CFLAGS=""
PORTALS_CPPFLAGS=""
PORTALS_LDFLAGS=""
PORTALS_LIBS=""

dnl PtlInit
dnl PtlFini

AC_ARG_WITH(portals,
        [  --with-portals=DIR      Location of Portals (yes/no/path_to_portals)],
        [ ac_with_portals=$withval; ], [ac_with_portals=no])

if test x"$ac_with_portals" = xno; then

        AM_CONDITIONAL(HAVE_PORTALS,false)
        ac_with_portals=no;

elif test x"$withval" = xyes -o x"$withval" = x; then

        PORTALS_LDFLAGS="";
        PORTALS_CPPFLAGS="";
        ac_with_portals=yes;

else

        PORTALS_LDFLAGS="-L$withval/lib";
        PORTALS_CPPFLAGS="-I$withval/include";
        ac_with_portals=yes;

fi

if test x"$ac_with_portals" = xyes; then

        dnl Look for portals header files
        save_CPPFLAGS=$CPPFLAGS;
        CPPFLAGS="$CPPFLAGS $PORTALS_CPPFLAGS"
        LDFLAGS="$LDFLAGS $PORTALS_LDFLAGS"



        if test x"$ac_portals_hdr_ok" = xno; then
                AC_CHECK_HEADER(portals3.h,
                         [AC_DEFINE(HAVE_PORTALS3_H, 1,
                                [Define to 1 if you have <portals3.h>.])
                         ac_portals_hdr_ok=yes;
                         PORTALS_CFLAGS="$PORTALS_CFLAGS $EXTRA_CFLAGS";
                         PORTALS_CPPFLAGS="$PORTALS_CPPFLAGS $EXTRA_CFLAGS"],
                         [ac_portals_hdr_ok=no])
        fi

        if test x"$ac_portals_hdr_ok" = xno; then
                AC_CHECK_HEADER(portals/portals3.h,
                         [AC_DEFINE(HAVE_PORTALS_PORTALS3_H, 1,
                                [Define to 1 if you have <portals/portals3.h>.])
                          ac_portals_hdr_ok=yes;
                         PORTALS_CFLAGS="$PORTALS_CFLAGS $EXTRA_CFLAGS";
                         PORTALS_CPPFLAGS="$PORTALS_CPPFLAGS $EXTRA_CFLAGS"],
                         [ac_portals_hdr_ok=no])
        fi

        dnl Look for other portals files
        AC_CHECK_HEADERS([p3nal_utcp.h p3rt/p3rt.h], , ,
             [
                        [#if defined(HAVE_PORTALS3_H)
                         #include <portals3.h>
                         #elif defined(HAVE_PORTALS_PORTALS3_H)
                         #include <portals/portals3.h>
                         #endif]
             ])

        if test x"$ac_portals_hdr_ok" = xno; then
                CPPFLAGS=$save_CPPFLAGS
        fi


        dnl Look for -lportals
        if test x"$ac_portals_lib_ok" = xno -a x$ac_portals_hdr_ok = xyes; then
            save_LIBS=$LIBS;
            LIBS=""
            AC_SEARCH_LIBS(PtlInit,[portals],
                    [ac_portals_lib_ok=yes],
                    [ac_portals_lib_ok=no],
                    [$save_LIBS $PTHREAD_LDFLAGS $PTHREAD_LIBS])
            if test -n $LIBS; then
                PORTALS_LIBS=$LIBS;
            fi
            LIBS=$save_LIBS;
        fi

        dnl Check for Portals UTCP libraries (Schutt's implementation)
        dnl Look for PtlInit in -lp3api -lp3lib -lp3rt -lp3utcp
        if test x"$ac_portals_lib_ok" = xno -a x$ac_portals_hdr_ok = xyes; then
            save_LIBS=$LIBS;
            save_LDFLAGS=$LDFLAGS;
            save_CFLAGS=$CFLAGS;

            extra_LIBS="-lp3api -lp3lib -lp3rt -lp3utcp"


            LDFLAGS="$LDFLAGS $PORTALS_LDFLAGS"
            LIBS="$LIBS $extra_LIBS $PTHREAD_LIBS"
            CFLAGS="$CFLAGS $PTHREAD_CFLAGS"

            AC_MSG_CHECKING([if portals links with $LDFLAGS $LIBS])

            AC_LINK_IFELSE([AC_LANG_CALL([],[PtlInit])],
                        [PORTALS_LIBS=$extra_LIBS;
                        ac_portals_lib_ok=yes;
                        AC_MSG_RESULT(yes)],
                        [ac_portals_lib_ok=no;
                         AC_MSG_RESULT(no)])

            LIBS=$save_LIBS;
            LDFLAGS=$save_LDFLAGS;
            CDFLAGS=$save_CFLAGS;
        fi

        dnl See if PTHREADS is really necessary
        if test x"$ac_portals_lib_ok" = xyes; then

            ac_portals_flags="none ${PTHREAD_CFLAGS} ${PTHREAD_LIBS}"
            ac_portals_lib_ok=no

            for flag in $ac_portals_flags; do

                case $flag in
                    none)
                        AC_MSG_CHECKING([whether portals works without any additional flags])
                    ;;

                    -l*)
                        AC_MSG_CHECKING([whether portals works with library $flag])
                        EXTRA_LIBS="$flag"
                        ;;

                    -*)
                        AC_MSG_CHECKING([whether portals works with library -l$flag])
                        EXTRA_LIBS="$flag"
                        ;;
                esac

                save_LIBS="$LIBS"
                save_CFLAGS="$CFLAGS"
                save_CPPFLAGS="$CPPFLAGS"
                save_LDFLAGS="$LDFLAGS"

                LIBS="$LIBS $PORTALS_LIBS $EXTRA_LIBS"
                CFLAGS="$CFLAGS $PORTALS_CFLAGS $EXTRA_CFLAGS"
                CPPFLAGS="$CPPFLAGS $PORTALS_CPPFLAGS"
                LDFLAGS="$LDFLAGS $PORTALS_LDFLAGS"

                See if Portals will link with the provided flags
                AC_LINK_IFELSE([AC_LANG_CALL([],[PtlInit])],
                        [PORTALS_CFLAGS="$PORTALS_CFLAGS $EXTRA_CFLAGS";
                        PORTALS_CPPFLAGS="$PORTALS_CPPFLAGS $EXTRA_CPPFLAGS";
                        PORTALS_LIBS="$PORTALS_LIBS $EXTRA_LIBS";
                        ac_portals_lib_ok=yes],
                        [ac_portals_lib_ok=no])
                AC_MSG_RESULT($ac_portals_lib_ok)

                if test "x$ac_portals_lib_ok" = xyes; then
                        PORTALS_LIBS="$PORTALS_LIBS $EXTRA_LIBS";
                        PORTALS_CFLAGS="$PORTALS_CFLAGS $EXTRA_CFLAGS";
                        PORTALS_CPPFLAGS="$PORTALS_CPPFLAGS $EXTRA_CFLAGS";
                        if test `uname -s` = "Darwin"; then
                              AM_CONDITIONAL(NEED_DARWIN_SINGLE_MODULE,true)
                        fi
                fi


                dnl Restore environment variables
                LIBS="$save_LIBS"
                CFLAGS="$save_CFLAGS"
                CPPFLAGS="$save_CPPFLAGS"
                LDFLAGS="$save_LDFLAGS"

            done
        fi


        if test x"$ac_portals_hdr_ok" = xno -o x$ac_portals_lib_ok = xno; then
            AM_CONDITIONAL(HAVE_PORTALS,false)
            PORTALS_CFLAGS=""
            PORTALS_CPPFLAGS=""
            PORTALS_LDFLAGS=""
            PORTALS_LIBS=""
        else
            ac_portals_lib_ok=yes;
            AM_CONDITIONAL(HAVE_PORTALS,true)
        fi


        dnl Create definitions for the include files
        dnl AC_DEFINE_UNQUOTED([PORTALS_HEADER], ["$PORTALS_HEADER"],
        dnl 	["Path to portals3.h"])
        dnl AC_DEFINE_UNQUOTED([PORTALS_RT_HEADER], ["$PORTALS_RT_HEADER"],
        dnl 	["Path to Portals runtime definitions"])
        dnl AC_DEFINE_UNQUOTED([PORTALS_NAL_HEADER], ["$PORTALS_NAL_HEADER"],
        dnl 	["Path to portals NAL definitions"])

        dnl Check for portals types and functions defined in the 3.1 specification
        if test x"$ac_portals_lib_ok" = xyes; then

                save_CFLAGS=$CFLAGS
                save_CPPFLAGS=$CPPFLAGS
                save_LDFLAGS=$LDFLAGS
                save_LIBS=$LIBS

                CFLAGS="$CFLAGS $PORTALS_CFLAGS"
                CPPFLAGS="$CPPFLAGS $PORTALS_CPPFLAGS"
                LDFLAGS="$LDFLAGS $PORTALS_LDFLAGS"
                LIBS="$LIBS $PORTALS_LIBS"

                dnl See if we have PTL_NO_ACK_REQ or PTL_NOACK_REQ
                AC_MSG_CHECKING([whether portals uses PTL_NO_ACK_REQ])
                AC_LINK_IFELSE([AC_LANG_PROGRAM(
                        [#if defined(HAVE_PORTALS3_H)
                         #include <portals3.h>
                         #elif defined(HAVE_PORTALS_PORTALS3_H)
                         #include <portals/portals3.h>
                         #else
                         #error Cound not find include file
                         #endif
                 ],[int a = PTL_NO_ACK_REQ;])],
                 [# Success
                  AC_DEFINE(HAVE_PTL_NO_ACK_REQ,1,[Define if you have PTL_NO_ACK_REQ.])
                  AC_MSG_RESULT(yes)
                 ],
                 [
                  AC_MSG_RESULT(no)
                  AC_MSG_CHECKING([whether portals uses PTL_NOACK_REQ])
                  AC_LINK_IFELSE([AC_LANG_PROGRAM(
                              [#if defined(HAVE_PORTALS3_H)
                               #include <portals3.h>
                               #elif defined(HAVE_PORTALS_PORTALS3_H)
                               #include <portals/portals3.h>
                               #else
                               #error Cound not find include file
                               #endif
                       ],[int a = PTL_NOACK_REQ;])],
                       [# Success
                        AC_DEFINE(HAVE_PTL_NOACK_REQ,1,[Define if you have PTL_NOACK_REQ.])
                        AC_MSG_RESULT(yes)
                       ],
                       [
                        AC_MSG_RESULT(no)
                       ])
                 ])

                AC_CHECK_TYPES([ptl_time_t, ptl_eq_handler_t],
                        , ,
                        [#if defined(HAVE_PORTALS3_H)
                         #include <portals3.h>
                         #elif defined(HAVE_PORTALS_PORTALS3_H)
                         #include <portals/portals3.h>
                         #endif])

                AC_CHECK_FUNCS([PtlErrorStr \
                        PtlNIFailStr \
                        PtlEventKindStr \
                        PtlGetJid \
                        PtlACEntry])

                CFLAGS=$save_CFLAGS
                CPPFLAGS=$save_CPPFLAGS
                LDFLAGS=$save_LDFLAGS
                LIBS=$save_LIBS
        fi


        # This is a big hack.  This is fragile.  Try to determine if we are
        # using Cray Portals.  Cray Portals generates more data movement
        # events than Schutt (standard) Portals.  This requires a memory
        # descriptor threshold adjustment.  Cannot be determined at runtime.
        #
        # This test relies on a priori knowledge of specific types and
        # functions that are (un)defined.  If this list changes, this
        # test will quickly crumble.
        #
        if test x"$ac_cv_func_PtlErrorStr" = xno -a \
                x"$ac_cv_type_ptl_eq_handler_t" = xyes -a \
                x"$ac_cv_func_PtlNIFailStr" = xno -a \
                x"$ac_cv_func_PtlEventKindStr" = xno -a \
                x"$ac_cv_func_PtlGetJid" = xyes -a \
                x"$ac_cv_func_PtlACEntry" = xyes; then
            AC_DEFINE(HAVE_CRAY_PORTALS,1,[Define if you have Cray Portals.])
        fi


        AC_SUBST(PORTALS_CPPFLAGS)
        AC_SUBST(PORTALS_CFLAGS)
        AC_SUBST(PORTALS_LDFLAGS)
        AC_SUBST(PORTALS_LIBS)
        AC_SUBST(PORTALS_HEADER)
        AC_SUBST(PORTALS_NAL_HEADER)
        AC_SUBST(PORTALS_RT_HEADER)

        # Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
        if test x"$ac_portals_lib_ok" = xyes; then
                ifelse([$1],,[AC_DEFINE(HAVE_PORTALS,1,[Define if you have the Portals.])],[$1])
                :
        else
                $2
                :
        fi
fi
AC_LANG_RESTORE
])dnl AC_PORTALS
