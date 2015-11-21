dnl
dnl
dnl AC_PROG_MXML
dnl
dnl Test for Mini-XML
dnl and set $MXML to the correct value.
dnl
dnl
dnl @synopsis AC_MXML
dnl
dnl This macro test if mini-XML is installed. If mini-XML
dnl is installed, it set $MXML to the right value
dnl
dnl @version 2.0
dnl @author Jay Lofstead lofstead@cc.gatech.edu
dnl @author Norbert Podhorszki pnorbert@ornl.gov
dnl
AC_DEFUN([AC_MXML],[

AM_CONDITIONAL(HAVE_MXML,true)
ac_with_mxml=no

dnl By default assume mxml is installed in system location
AC_ARG_WITH(mxml,
        [  --with-mxml=DIR      Location of Mini-XML library],
        [:])

dnl If --without-mxml was given give an error
if test "x$with_mxml" == "xno"; then

    AM_CONDITIONAL(HAVE_MXML,false)

elif test "x$with_mxml" == "xyes" -o "x$with_mxml" == "x"; then

    dnl If nothing was given, then look in the system libs
    if test -n "$MXML_LIB"; then
         MXML_LDFLAGS="$MXML_LIB"
    else
       dnl If not in the environment, then look in the system libs
         MXML_LDFLAGS=""
    fi

    if test -n "$MXML_INC"; then
        MXML_CPPFLAGS="$MXML_INC"
    else
        MXML_CPPFLAGS=""
    fi
    ac_with_mxml=yes

else

    dnl Otherwise, set up the flags
    MXML_DIR=$with_mxml
    MXML_CPPFLAGS="-I${MXML_DIR}/include"
    if test -d "${MXML_DIR}/lib64"; then
        MXML_LDFLAGS="-L${MXML_DIR}/lib64";
    else
        MXML_LDFLAGS="-L${MXML_DIR}/lib";
    fi
    ac_with_mxml=yes

fi


if test "x$ac_with_mxml" == "xyes"; then

    AC_ARG_WITH(mxml-libs,
            [  --with-mxml-libs=<linker flags for Mini-XML library>],
            [MXML_LIBS=$withval])

    save_CPPFLAGS="$CPPFLAGS"
    save_LIBS="$LIBS"
    save_LDFLAGS="$LDFLAGS"

    CPPFLAGS="$CPPFLAGS $MXML_CPPFLAGS"
    LDFLAGS="$LDFLAGS $MXML_LDFLAGS"

    if test -z "$MXML_LIBS"; then
        MXML_LIBS="-lmxml"
    fi
    LIBS="$LIBS ${MXML_LIBS}"


    AC_CHECK_HEADERS(mxml.h,
        ,
        [AM_CONDITIONAL(HAVE_MXML,false)])

    if test -z "${HAVE_MXML_TRUE}"; then
        dnl Check for the Mini-XML library and headers
        AC_MSG_CHECKING([if mxml code can be linked])
        AC_TRY_LINK([#include "mxml.h"],
        [mxml_node_t * n; 
         char *buffer;
         char *value;
         n = mxmlLoadString (0, buffer, MXML_TEXT_CALLBACK);
         mxmlWalkNext (n, n, MXML_DESCEND);
         value = mxmlElementGetAttr (n, "value");
         mxmlRelease (n);
        ],
        [MXML_LIBS="-lmxml"
         AC_MSG_RESULT(yes)
        ],
        [AM_CONDITIONAL(HAVE_MXML,false)
         AC_MSG_RESULT(no)
        ])

        dnl If Linking above failed, one reason might be that mxml uses pthreads and
        dnl the compiler does not use it by default. Try getting phtreads
        if test -z "${HAVE_MXML_FALSE}"; then
            dnl Check for the Mini-XML library and headers
            AC_REQUIRE([ACX_PTHREAD])
            LDFLAGS="$LDFLAGS $PTHREAD_LDFLAGS"
            LIBS="$LIBS $PTHREAD_LIBS"
            AC_MSG_CHECKING([if mxml code can be linked using pthreads])
            AC_TRY_LINK([#include "mxml.h"],
            [mxml_node_t * n; 
             char *buffer;
             char *value;
             n = mxmlLoadString (0, buffer, MXML_TEXT_CALLBACK);
             mxmlWalkNext (n, n, MXML_DESCEND);
             value = mxmlElementGetAttr (n, "value");
             mxmlRelease (n);
            ],
            [MXML_LDFLAGS="$MXML_LDFLAGS $PTHREAD_LDFLAGS"
             MXML_LIBS="-lmxml $PTHREAD_LIBS"
             AM_CONDITIONAL(HAVE_MXML,true)
             AC_MSG_RESULT(yes)
            ],
            [AM_CONDITIONAL(HAVE_MXML,false)
             AC_MSG_RESULT(no)
            ])
        fi
    fi


    LIBS="$save_LIBS"
    LDFLAGS="$save_LDFLAGS"
    CPPFLAGS="$save_CPPFLAGS"

    AC_SUBST(MXML_LIBS)
    AC_SUBST(MXML_LDFLAGS)
    AC_SUBST(MXML_CPPFLAGS)

    dnl Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
    if test -z "${HAVE_MXML_TRUE}"; then
        ifelse([$1],,[AC_DEFINE(HAVE_MXML,1,[Define if you have the MXML.])],[$1])
        :
    else
        $2
        :
    fi

fi
])dnl AC_MXML
