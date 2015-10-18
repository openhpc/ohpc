dnl ######################################################################
dnl
dnl Finds CRAY_PMI
dnl
dnl ######################################################################

AC_DEFUN([AC_CRAY_PMI],
[

ac_cray_pmi_lib_ok=no

AM_CONDITIONAL(HAVE_CRAY_PMI,true)

dnl Automatic checking for CRAY_PMI is disabled now.
AC_ARG_WITH(cray-pmi,
            [  --with-cray-pmi=<location of CRAY_PMI installation>],
            [CRAY_PMI_DIR=$withval], [with_cray_pmi=no])

AC_ARG_WITH(cray-pmi-incdir,
            [  --with-cray-pmi-incdir=<location of CRAY_PMI includes>],
            [CRAY_PMI_INCDIR=$withval
             with_cray_pmi=detailed])

AC_ARG_WITH(cray-pmi-libdir,
             [  --with-cray-pmi-libdir=<location of CRAY_PMI library>],
             [CRAY_PMI_LIBDIR=$withval
              with_cray_pmi=detailed])

AC_ARG_WITH(cray-pmi-libs,
             [  --with-cray-pmi-libs=<linker flags besides -L<cray-pmi-libdir>, e.g. -lpmi],
             [CRAY_PMI_LIBS=$withval
              with_cray_pmi=detailed])


dnl If --without-cray-pmi was given set HAVE_CRAY_PMI to false and do nothing more
dnl If nothing was given, then too, do nothing 
if test "x$with_cray_pmi" == "xno"; then

   AM_CONDITIONAL(HAVE_CRAY_PMI,false)

else

    AC_MSG_NOTICE([=== checking for CRAY PMI ===])

    dnl If we know CRAY_PMI_DIR, then we can know CRAY_PMI_INCDIR.
    dnl We don't overwrite CRAY_PMI_INCDIR.
    if test -z "${CRAY_PMI_INCDIR}"; then
        if test -n "${CRAY_PMI_DIR}"; then
            CRAY_PMI_INCDIR="${CRAY_PMI_DIR}/include";
        fi
    fi

    dnl If we know CRAY_PMI_DIR, then we can know CRAY_PMI_LIBDIR.
    dnl We don't overwrite CRAY_PMI_LIBDIR.
    if test -z "${CRAY_PMI_LIBDIR}"; then
        if test -n "${CRAY_PMI_DIR}"; then
            if test -d "${CRAY_PMI_DIR}/lib64"; then
                CRAY_PMI_LIBDIR="${CRAY_PMI_DIR}/lib64";
            else
                CRAY_PMI_LIBDIR="${CRAY_PMI_DIR}/lib";
            fi
        fi
    fi

    dnl Add "-I" to CRAY_PMI_INCDIR.
    CRAY_PMI_CPPFLAGS="-I${CRAY_PMI_INCDIR}"

    dnl Add "-L" to CRAY_PMI_LIBDIR.
    CRAY_PMI_LDFLAGS="-L${CRAY_PMI_LIBDIR}"

    dnl if hdf5 libs are not defined then guess and define it
    if test -z "${CRAY_PMI_LIBS}"; then
        dnl default CRAY_PMI lib is usually just -lpmi
        CRAY_PMI_LIBS="-lpmi"
    fi

    save_CC="$CC"
    save_CPPFLAGS="$CPPFLAGS"
    save_LIBS="$LIBS"
    save_LDFLAGS="$LDFLAGS"
    LIBS="$LIBS $CRAY_PMI_LIBS"
    LDFLAGS="$LDFLAGS $CRAY_PMI_LDFLAGS"
    CPPFLAGS="$CPPFLAGS $CRAY_PMI_CPPFLAGS"
    CC="$MPICC"

    if test -z "${HAVE_CRAY_PMI_TRUE}"; then
        dnl  AC_CHECK_HEADERS(pmi.h,
        dnl      ,
        dnl      [if test "x$with_cray_pmi" != xcheck; then
        dnl          AC_MSG_FAILURE( [--with-cray-pmi was given, but test for pmi.h failed])
        dnl       fi
        dnl       AM_CONDITIONAL(HAVE_CRAY_PMI,false)])
        AC_MSG_CHECKING([for Cray's pmi.h])
        if test -f ${CRAY_PMI_INCDIR}/pmi.h; then
            AC_MSG_RESULT(yes)
        else
            AM_CONDITIONAL(HAVE_CRAY_PMI,false)
            AC_MSG_RESULT(no)
            AC_MSG_FAILURE( [--with-cray-pmi was given, but test for ${CRAY_PMI_INCDIR}/pmi.h failed])
        fi
    fi

    if test -z "${HAVE_CRAY_PMI_TRUE}"; then
        AC_MSG_CHECKING([if pmi code can be compiled])
        AC_TRY_COMPILE([#include "pmi.h"],
            [int size;
             int rank;
             PMI_Get_size(&size);
             PMI_Get_rank(&rank);
            ],
            [AC_MSG_RESULT(yes)
             ac_cray_pmi_lib_ok=yes],
            [AC_MSG_RESULT(no)
             if test "x$with_cray_pmi" != xcheck; then
                AC_MSG_FAILURE( [--with-cray-pmi was given, but compile test failed])
             fi
             AM_CONDITIONAL(HAVE_CRAY_PMI,false)
            ])

        AC_SUBST(CRAY_PMI_LIBS)
        AC_SUBST(CRAY_PMI_LDFLAGS)
        AC_SUBST(CRAY_PMI_CPPFLAGS)
    fi

    LIBS="$save_LIBS"
    LDFLAGS="$save_LDFLAGS"
    CPPFLAGS="$save_CPPFLAGS"
    CC="$save_CC"

    # Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
    if test -z "${HAVE_CRAY_PMI_TRUE}"; then
            ifelse([$1],,[AC_DEFINE(HAVE_CRAY_PMI,1,[Define if you have CRAY_PMI.])],[$1])
            :
    else
            $2
            :
    fi

fi

])
