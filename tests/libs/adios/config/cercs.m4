dnl  Mon Jan 28 13:01:16 EST 2008
dnl
dnl cercs_require_package(package, include_file, library_file)
dnl   either include file or library_file may be left off if not needed
dnl   this macro searches for the files using find_cercs_file().  If
dnl   found, it adds directory in which the found file resides to either 
dnl   CPPFLAGS (with a -I prefix)  or LDFLAGS (with a -L prefix)
dnl
AC_DEFUN([CERCS_REQUIRE_PACKAGE],
[
AC_REQUIRE([CERCS_SET_ARCHIVE])
AC_REQUIRE([CERCS_HAS_CYGPATH])
if test -z "$with_local_specified"; then
CERCS_REQUIRE_OTHER_PACKAGE($1,$2,$3)
else
CERCS_REQUIRE_OTHER_PACKAGE($1,$2,$3,1)
fi
])
dnl
dnl
AC_DEFUN([CERCS_REQUIRE_OTHER_PACKAGE],
[
AC_REQUIRE([CERCS_SET_ARCHIVE])
AC_REQUIRE([CERCS_HAS_CYGPATH])
AC_ARG_WITH(translit($1, `/',`_'), translit([  --with-$1=DIR	Where to find $1 package], `/',`_'))
define([with_translit], translit(with_$1, `/',`_'))
if test -n "$with_translit"; then
dnl
dnl if they did a with, kill the cache variables
dnl
translit(unset cercs_cv_$1_include_arg,  `/',`_')
translit(unset cercs_cv_$1_link_arg,  `/',`_')
if test `echo $with_translit | sed 's/\(.\).*/\1/g'` != "/"; then
with_translit=`pwd`/$with_translit
fi
fi
ifelse([$2], , ,
dnl
dnl  if arg2 (include_file) is specified
dnl
AC_MSG_CHECKING(needed include args for $1 package)
AC_CACHE_VAL(translit(cercs_cv_$1_include_arg, `/',`_'), 
[
ifelse([$4],1,[
search_list="$PWD/../$1 $PWD/../../$1 $PWD/../../../$1"
CERCS_SEARCH($search_list)
if test -n "$tmp_search_results"; then
cercs_tmp=$tmp_search_results
fi
],
CERCS_FIND_FILE($1, $2, cercs_tmp, $with_translit, include))
if test -n "$cercs_tmp" -a "$cercs_tmp" != "/usr/include/$2"; then
translit(cercs_cv_$1_include_arg, `/',`_')=-I`$PATHPROG $cercs_tmp | sed 's#\\\\#/#g' | sed "s#.$2##g"`
fi
])
AC_MSG_RESULT(translit($cercs_cv_$1_include_arg, `/',`_'))
dnl
dnl  add the result to CPPFLAGS if it is absent
dnl
translit(if test -n "$cercs_cv_$1_include_arg"; then, `/',`_')
translit(arg="$cercs_cv_$1_include_arg", `/',`_')
no_dash_arg=`echo $arg | sed 's/^-//g'`
[if test `echo $DT_CPPFLAGS | grep -c "$no_dash_arg"` -eq 0; then
if test `echo $arg | grep -c "$1"` -eq 0; then
DT_CPPFLAGS="$DT_CPPFLAGS $arg";
else
DT_CPPFLAGS="$arg $DT_CPPFLAGS"
fi
fi]
fi
)
ifelse([$3], , ,
dnl
dnl  if arg3 (library_file) is specified
dnl
AC_MSG_CHECKING(needed link args for $1 package)
AC_CACHE_VAL(translit(cercs_cv_$1_link_dir,  `/',`_'), 
[
ifelse([$4],1,[
search_list="$PWD/../$1 $PWD/../../$1 $PWD/../../../$1"
CERCS_SEARCH($search_list)
if test -n "$tmp_search_results"; then
cercs_tmp=$tmp_search_results
fi
],
CERCS_FIND_FILE($1, $3, cercs_tmp, $with_translit, lib))
if test -n "$cercs_tmp" -a "$cercs_tmp" != "$3"; then
translit(cercs_cv_$1_link_dir, `/',`_')=`$PATHPROG $cercs_tmp | sed 's#\\\\#/#g' | sed "s/.$3//g"`
else
translit(cercs_cv_$1_link_dir="",  `/',`_')
fi
])
AC_MSG_RESULT(translit($cercs_cv_$1_link_dir, `/',`_'))
ld_arg="-L"
new_flags=$LDFLAGS
dnl
dnl  add the result to LDFLAGS if it is absent
dnl
translit(cercs_cv_$1_link_arg=`echo $cercs_cv_$1_link_dir, `/',`_') | sed "/./ s/^/$ld_arg/1"`
translit(if test -n "$cercs_cv_$1_link_arg"; then, `/',`_')
translit(arg=$cercs_cv_$1_link_arg, `/',`_')
no_dash_arg=`echo $arg | sed 's/^-//g'`
[if test `echo $new_flags | grep -c "$no_dash_arg"` -eq 0; then
root=`echo $3 | sed 's/\..*//'`
translit(if test ! -r $cercs_cv_$1_link_dir, `/`, `_`)/$root.la; then
if eval "test \"\${LIBTOOL+set}\" = set"; then
translit(arg="$cercs_cv_$1_link_arg "`echo $cercs_cv_$1_link_dir, `/',`_') | sed "/./ s/^/-R/1"`
fi
fi
if test `echo $arg | grep -c "$1"` -eq 0; then
dnl if arg does not includes a project spec add it at the end
new_flags="$new_flags $arg"
else
new_flags="$arg $new_flags"
fi
fi]
DT_LDFLAGS=$new_flags
fi
)
])dnl
dnl
dnl cercs_find_file(package, file_to_find, variable_to_set, suggestion, dir_name)
dnl    search a set of standard directories to find file_to_find.  When found,
dnl    set $variable_to_set to the path of the file.  Use package and
dnl    suggestions to help search.  Mostly this searches ~chaos (if it exists)
dnl    and the usual suspects like:
dnl    /usr/{include,lib} /usr/local/{include,lib} /opt/<package>/{include,lib}
dnl    /opt/misc/{include,lib}.
dnl
AC_DEFUN([CERCS_FIND_FILE],
[
AC_REQUIRE([CERCS_HAS_CSH])
AC_REQUIRE([CERCS_SET_ARCHIVE])
$3=""
search_list="./$2"
CHAOS_HOMEDIR=""
if test -n "$CSH"; then
CHAOS_HOMEDIR=`echo "echo ~chaos" | csh -sf  2>/dev/null | sed 's%/$%%'` || CHAOS_HOMEDIR=""
fi
if test -n "$4"; then
if test `echo $4 | cut -c1` = "~"; then
EXPANDED=`echo "echo $4" | csh -sf 2>/dev/null` || EXPANDED=""
else
EXPANDED=$4
fi

echo "Datatap = $datatap_dir"

search_list="$datatap_dir/$2 $datatap_dir/lib/$2 $datatap_dir/include/$2 $search_list $EXPANDED/$2 $EXPANDED/$5/$2 $EXPANDED/share/$2 $EXPANDED/$cercs_cv_archive/$2 $EXPANDED/$cercs_cv_archive/$5/$2 $EXPANDED/$1/$2 $EXPANDED/$1/$cercs_cv_archive/$2 $EXPANDED/$1/$5/$2 $EXPANDED/$1/$5/$cercs_cv_archive/$2"
fi
if test -z "$with_installed_specified"; then
search_list="$search_list `pwd`/../$1/$2 `pwd`/../$5/$2 `pwd`/../share/$2"
if test "$CHAOS_HOMEDIR" != "$HOME"; then
search_list="$search_list $HOME/$1/$2 $HOME/$cercs_cv_archive/$5/$2 $HOME/$5/$2"
fi
fi
if test -n "$CHAOS_HOMEDIR" -a -n "$cercs_cv_archive"; then
search_list="$search_list $CHAOS_HOMEDIR/$cercs_cv_archive/$1/$5/$2 $CHAOS_HOMEDIR/$cercs_cv_archive/$5/$2 $CHAOS_HOMEDIR/$1/$cercs_cv_archive/$5/$2 $CHAOS_HOMEDIR/$1/$5/$2 $CHAOS_HOMEDIR/$5/$2"
fi
if test "$libdir" != '${exec_prefix}/lib'; then
tmpdir=`echo ${libdir} |  sed 's%/$%%'` 
search_list="$tmpdir/$2 $search_list"
fi
if test "$exec_prefix" != "NONE"; then
tmpdir=`echo ${exec_prefix} |  sed 's%/$%%'` 
search_list="$tmpdir/lib/$2 $search_list"
fi
if test "$includedir" != '${prefix}/include'; then
tmpdir=`echo ${includedir} |  sed 's%/$%%'` 
search_list="$tmpdir/$2 $search_list"
fi
if test "$prefix" != "NONE"; then
tmpdir=`echo ${prefix} |  sed 's%/$%%'` 
search_list="$tmpdir/$5/$2 $search_list"
fi
if test "$5" == "lib"; then
  for tmp_lib_value in $sys_lib_search_path_spec; do
     search_list="$search_list $tmp_lib_value/$2"
  done
fi

search_list="$datatap_dir/$2 $datatap_dir/lib/$2 $datatap_dir/include/$2 $search_list /usr/$5/$2 /usr/local/$5/$2 /opt/$1/$5/$2 /opt/misc/$5/$2 /opt/misc/$5/$cercs_cv_archive/$2"
CERCS_SEARCH($search_list)
if test -n "$tmp_search_results"; then
$3=$tmp_search_results
fi
])dnl
AC_DEFUN([CERCS_SET_INSTALLED],[AC_ARG_WITH(installed, [  --with-installed        Don't use local copies of CERCS packages],with_installed_specified=1)])
AC_DEFUN([CERCS_SET_LOCAL],[AC_ARG_WITH(local, [  --with-local            Use only local copies of CERCS packages],with_local_specified=1)])
dnl
dnl CERCS_SET_ARCHIVE()
dnl   set the $cercs_cv_machine_target variable to a standard archive name
dnl
AC_DEFUN([CERCS_SET_ARCHIVE],[
AC_REQUIRE([CERCS_SET_INSTALLED])
AC_REQUIRE([CERCS_SET_LOCAL])
AC_REQUIRE([CERCS_HAS_CSH])
CHAOS_HOMEDIR=""
if test -n "$CSH"; then
CHAOS_HOMEDIR=`echo "echo ~chaos" | csh -sf  2>/dev/null | sed 's%/$%%'` || CHAOS_HOMEDIR=""
fi
if test "$cross_compiling" = yes ; then
  cpu=$host_cpu
  vendor=$host_vendot
  os=$host_os
else
  cpu=
  vendor=
  os=
fi
if test -x $CHAOS_HOMEDIR/bin/cercs_arch; then
cercs_cv_archive=`$CHAOS_HOMEDIR/bin/cercs_arch "$cpu" "$vendor" "$os"`
else
cercs_cv_archive=${cercs_cv_archive-`cercs_arch 2>/dev/null`} || cercs_cv_archive=""
fi
])dnl
dnl
dnl  CERCS_SEARCH(variable to define, options to try)
define(CERCS_SEARCH,
[tmp_search_results=""
echo "configure:__oline__: searching for $1 " >&5
for tmp_search_value in $1; do 
   if test -r $tmp_search_value; then 
	tmp_search_results=$tmp_search_value
	echo "configure:__oline__: first found $tmp_search_results " >&5
	break
   fi 
done
])dnl
dnl
dnl
dnl CERCS_LIB_PREFIX
dnl  this macro tries to set a reasonable default for the prefix value
dnl  call with two arguments, project name and library name
dnl
AC_DEFUN([CERCS_LIB_PREFIX],
[if test "x$prefix" = xNONE; then
AC_REQUIRE([CERCS_SET_ARCHIVE])
search_list=""
CHAOS_HOMEDIR=""
if test -n "$CSH"; then
CHAOS_HOMEDIR=`echo "echo ~chaos" | csh -sf  2>/dev/null | sed 's%/$%%'` || CHAOS_HOMEDIR=""
fi
if test -n "$CHAOS_HOMEDIR"; then
search_list="$search_list $CHAOS_HOMEDIR/$cercs_cv_archive/lib/$2 $CHAOS_HOMEDIR/lib/$2"
fi
search_list="$search_list /usr/lib/$2 /usr/local/lib/$2 /opt/$1/lib/$2 /opt/misc/lib/$2"
CERCS_SEARCH($search_list)
if test -n "$tmp_search_results"; then
    prefix=`echo $tmp_search_results|sed "s%$cercs_cv_archive/lib/$2%%g;s%lib/$2%%g;s%/[^/][^/]*//*[^/][^/]*$%%"`
    exec_prefix=`echo $tmp_search_results|sed 's%lib/$2%%g;s%/[^/][^/]*//*[^/][^/]*$%%'`
fi
fi
])dnl
AC_DEFUN([CERCS_HAS_CSH], [AC_PATH_PROG(CSH,csh)])dnl
AC_DEFUN([CERCS_HAS_CYGPATH], [AC_CHECK_PROG(PATHPROG,cygpath,[cygpath -w],[echo])])dnl
