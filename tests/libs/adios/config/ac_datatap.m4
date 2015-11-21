
AC_DEFUN([AC_DATATAP], [

AC_REQUIRE([AC_INFINIBAND])
AC_REQUIRE([AC_PORTALS])

dnl give an option to the user to enable datatap (with either ib or portals)
dnl by default datatap will be disabled

DT_SRCDIR=""
DT_CPPFLAGS=""
DT_LDFLAGS=""
DT_LIBS=""

datatap=disable

temptest=enable

datatap_dir=""


AC_ARG_WITH(datatap, 
	[  --with-datatap=DIR 	Location of DataTap], 
	[ ac_with_datatap=$withval], [with_datatap=no])

if test "x$with_datatap" = "xno"; then
	AC_DEFINE(NO_DATATAP, 1, [Datatap is disabled])
	datatap=disable
	temptest=disable

elif test x"$with_datatap" = xyes -o x"$with_datatap" = xcheck; then
		
	AC_DEFINE(NO_DATATAP, 0, [Datatap is disabled])

	if test x"$ac_with_infiniband" = xyes; then
		CERCS_REQUIRE_PACKAGE(ibpbio, thin_ib.h, libibclient.la)
		CERCS_REQUIRE_PACKAGE(ffs, ffs.h, libffs.la)
		CERCS_REQUIRE_PACKAGE(gen_thread, gen_thread.h, libgen_thread.la)
		if test -n "$cercs_cv_ibpbio_link_dir";then
			DT_LDFLAGS="$DT_LDFLAGS -L$cercs_cv_ibpbio_link_dir"
			DT_LIBS="$DT_LIBS -libclient"
			datatap=ibverbs
		else
			temptest=disable
		fi
		if test -n "$cercs_cv_ffs_link_dir";then
                   	DT_LDFLAGS="$DT_LDFLAGS -L$cercs_cv_ffs_link_dir"
			DT_LIBS="$DT_LIBS -lgen_thread"
			datatap=ibverbs
		else
			temptest=disable
		fi
		if test -n "$cercs_cv_gen_thread_link_dir";then
			DT_LDFLAGS="$DT_LDFLAGS -L$cercs_cv_gen_thread_link_dir"
			DT_LIBS="$DT_LIBS -lgen_thread"
			datatap=ibverbs
		else
			temptest=disable
		fi		
				
        elif test x"$ac_with_portals" = xyes; then
		CERCS_REQUIRE_PACKAGE(ptlpbio, thin_portal.h, libptlclient.a)

		if test -n "$cercs_cv_ptlpbio_link_dir";then
			DT_LDFLAGS="$DT_LDFLAGS -L$cercs_cv_ptlpbio_link_dir"
			DT_LIBS="$DT_LIBS -lptlclient -lptlserver -lbench -ldl"
			datatap=portals
		else
			temptest=disable
		fi

	else
		echo "Neither portals nor infiniband found. Disabling datatap"
		AC_DEFINE(NO_DATATAP, 1, [Datatap is disabled])
		datatap=disable
		temptest=disable
	fi
else
dnl directory given .. add it to search path with CERCS_REQUIRE_PACKAGE
	AC_MSG_NOTICE([Datatap with custom library path: $withval])
	
	datatap_dir=$withval

	AC_DEFINE(NO_DATATAP, 0, [Datatap is disabled])

	if test x"$ac_with_infiniband" = xyes; then
		CERCS_REQUIRE_PACKAGE(ibpbio, thin_ib.h, libibclient.la)
		CERCS_REQUIRE_PACKAGE(ffs, ffs.h, libffs.la)
		CERCS_REQUIRE_PACKAGE(gen_thread, gen_thread.h, libgen_thread.la)
		if test -n "$cercs_cv_ibpbio_link_dir";then
			DT_LDFLAGS="$DT_LDFLAGS -L$cercs_cv_ibpbio_link_dir"
			DT_LIBS="$DT_LIBS -libclient"
			datatap=ibverbs
		else
			temptest=disable
		fi
		if test -n "$cercs_cv_ffs_link_dir";then
			DT_LDFLAGS="$DT_LDFLAGS -L$cercs_cv_ffs_link_dir"
			DT_LIBS="$DT_LIBS -lgen_thread"
			datatap=ibverbs
		else
			temptest=disable
		fi
		if test -n "$cercs_cv_gen_thread_link_dir";then
			DT_LDFLAGS="$DT_LDFLAGS -L$cercs_cv_gen_thread_link_dir"
			DT_LIBS="$DT_LIBS -lgen_thread"
			datatap=ibverbs
		else
			temptest=disable
		fi		
				
        elif test x"$ac_with_portals" = xyes; then
		CERCS_REQUIRE_PACKAGE(ptlpbio, thin_portal.h, libptlclient.a)

		if test -n "$cercs_cv_ptlpbio_link_dir";then
			DT_LDFLAGS="$DT_LDFLAGS -L$cercs_cv_ptlpbio_link_dir"
			DT_LIBS="$DT_LIBS -lptlclient -lptlserver -lbench -ldl"
			datatap=portals
		else
			temptest=disable
		fi

	else
		echo "Neither portals nor infiniband found. Disabling datatap"
		AC_DEFINE(NO_DATATAP, 1, [Datatap is disabled])
		datatap=disable
		temptest=disable
	fi	
fi

if test x"$temptest" = xdisable; then
	datatap=disable
	echo "Datatap dependency check failed"
	AC_DEFINE(NO_DATATAP, 1, [Datatap is disabled])
fi

AC_SUBST(DT_LIBS)
AC_SUBST(DT_CPPFLAGS)
AC_SUBST(DT_LDFLAGS)


]) dnl AC_DATATAP
