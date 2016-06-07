dnl ######################################################################
dnl
dnl Finds CRAY_UGNI
dnl
dnl ######################################################################

AC_DEFUN([AC_CRAY_UGNI],
[

ac_cray_ugni_lib_ok=no

AM_CONDITIONAL(HAVE_CRAY_UGNI,true)

dnl Automatic checking for CRAY_UGNI is disabled now.
AC_ARG_WITH(cray-ugni,
            [  --with-cray-ugni=<location of CRAY UGNI installation>],
            [CRAY_UGNI_DIR=$withval], [with_cray_ugni=no])

AC_ARG_WITH(cray-ugni-incdir,
            [  --with-cray-ugni-incdir=<location of CRAY UGNI includes>],
            [CRAY_UGNI_INCDIR=$withval
             with_cray_ugni=detailed])

AC_ARG_WITH(cray-ugni-libdir,
             [  --with-cray-ugni-libdir=<location of CRAY UGNI library>],
             [CRAY_UGNI_LIBDIR=$withval
              with_cray_ugni=detailed])

AC_ARG_WITH(cray-ugni-libs,
             [  --with-cray-ugni-libs=<linker flags besides -L<cray-ugni-libdir>, e.g. -lugni],
             [CRAY_UGNI_LIBS=$withval
              with_cray_ugni=detailed])


dnl If --without-cray-ugni was given set HAVE_CRAY_UGNI to false and do nothing more
dnl If nothing was given, then too, do nothing 
if test "x$with_cray_ugni" == "xno"; then

   AM_CONDITIONAL(HAVE_CRAY_UGNI,false)

else

    AC_MSG_NOTICE([=== checking for CRAY UGNI ===])

    dnl If we know CRAY_UGNI_DIR, then we can know CRAY_UGNI_INCDIR.
    dnl We don't overwrite CRAY_UGNI_INCDIR.
    if test -z "${CRAY_UGNI_INCDIR}"; then
        if test -n "${CRAY_UGNI_DIR}"; then
            CRAY_UGNI_INCDIR="${CRAY_UGNI_DIR}/include";
        fi
    fi

    dnl If we know CRAY_UGNI_DIR, then we can know CRAY_UGNI_LIBDIR.
    dnl We don't overwrite CRAY_UGNI_LIBDIR.
    if test -z "${CRAY_UGNI_LIBDIR}"; then
        if test -n "${CRAY_UGNI_DIR}"; then
            if test -d "${CRAY_UGNI_DIR}/lib64"; then
                CRAY_UGNI_LIBDIR="${CRAY_UGNI_DIR}/lib64";
            else
                CRAY_UGNI_LIBDIR="${CRAY_UGNI_DIR}/lib";
            fi
        fi
    fi

    dnl Add "-I" to CRAY_UGNI_INCDIR.
    CRAY_UGNI_CPPFLAGS="-I${CRAY_UGNI_INCDIR}"

    dnl Add "-L" to CRAY_UGNI_LIBDIR.
    CRAY_UGNI_LDFLAGS="-L${CRAY_UGNI_LIBDIR}"

    dnl if hdf5 libs are not defined then guess and define it
    if test -z "${CRAY_UGNI_LIBS}"; then
        dnl default CRAY_UGNI lib is usually just -lugni
        CRAY_UGNI_LIBS="-lugni"
    fi

    save_CC="$CC"
    save_CPPFLAGS="$CPPFLAGS"
    save_LIBS="$LIBS"
    save_LDFLAGS="$LDFLAGS"
    LIBS="$LIBS $CRAY_UGNI_LIBS"
    LDFLAGS="$LDFLAGS $CRAY_UGNI_LDFLAGS"
    CPPFLAGS="$CPPFLAGS $CRAY_UGNI_CPPFLAGS"
    CC="$MPICC"

    if test -z "${HAVE_CRAY_UGNI_TRUE}"; then
        dnl  AC_CHECK_HEADERS(ugni.h,
        dnl      ,
        dnl      [if test "x$with_cray_ugni" != xcheck; then
        dnl          AC_MSG_FAILURE( [--with-cray-ugni was given, but test for ugni.h failed])
        dnl       fi
        dnl       AM_CONDITIONAL(HAVE_CRAY_UGNI,false)])
        AC_MSG_CHECKING([for Cray's ugni.h])
        if test -f ${CRAY_UGNI_INCDIR}/gni_pub.h; then
            AC_MSG_RESULT(yes)
        else
            AM_CONDITIONAL(HAVE_CRAY_UGNI,false)
            AC_MSG_RESULT(no)
            AC_MSG_FAILURE( [--with-cray-ugni was given, but test for ${CRAY_UGNI_INCDIR}/gni_pub.h failed])
        fi
    fi

    if test -z "${HAVE_CRAY_UGNI_TRUE}"; then
        AC_MSG_CHECKING([if ugni code can be compiled])
        AC_TRY_COMPILE(
            [#include <stdlib.h>
             #include <stdint.h>
             #include "gni_pub.h"
            ],
            [uint32_t inst_id;
             uint8_t ptag;
             uint32_t cookie;
             uint32_t  modes;
             gni_cdm_handle_t * cdm_hndl;
             gni_return_t status;
             status = GNI_CdmCreate(inst_id, ptag, cookie, modes, cdm_hndl);
            ],
            [AC_MSG_RESULT(yes)
             ac_cray_ugni_lib_ok=yes],
            [AC_MSG_RESULT(no)
             if test "x$with_cray_ugni" != xcheck; then
                AC_MSG_FAILURE( [--with-cray-ugni was given, but compile test failed])
             fi
             AM_CONDITIONAL(HAVE_CRAY_UGNI,false)
            ])

        AC_SUBST(CRAY_UGNI_LIBS)
        AC_SUBST(CRAY_UGNI_LDFLAGS)
        AC_SUBST(CRAY_UGNI_CPPFLAGS)
    fi

    LIBS="$save_LIBS"
    LDFLAGS="$save_LDFLAGS"
    CPPFLAGS="$save_CPPFLAGS"
    CC="$save_CC"

    # Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
    if test -z "${HAVE_CRAY_UGNI_TRUE}"; then
            ifelse([$1],,[AC_DEFINE(HAVE_CRAY_UGNI,1,[Define if you have CRAY_UGNI.])],[$1])
            :
    else
            $2
            :
    fi

fi

])
