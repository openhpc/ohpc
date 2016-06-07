
AC_DEFUN([AC_BGQ], [


AC_MSG_NOTICE([=== checking for BGQ ===])

temptest=enable

AC_ARG_WITH(bgq, 
	[  --with-bgq 	Whether to enable BGQ method or not], 
	[ ], [with_bgq=no])

if test "x$with_bgq" = "xno"; then
        echo "no bgq"
	AM_CONDITIONAL(HAVE_BGQ, false)
        AC_DEFINE(HAVE_BGQ,0,[Define if you want to enable BGQ method])
elif test x"$with_bgq" != xyes -o x"$with_bgq" != xcheck; then
dnl        AC_CHECK_HEADERS(/bgsys/drivers/ppcfloor/spi/include/kernel/location.h /bgsys/drivers/ppcfloor/spi/include/kernel/process.h /bgsys/drivers/ppcfloor/firmware/include/personality.h,
dnl                        ,
dnl                        [AM_CONDITIONAL(HAVE_BGQ,false)])
	AM_CONDITIONAL(HAVE_BGQ, true)
        AC_DEFINE(HAVE_BGQ,1,[Define if you want to enable BGQ method])
fi
]) dnl AC_BGQ
