AC_PREREQ([2.63])
AC_INIT([hpcg], [3.0], [https://github.com/openhpc/ohpc])
AM_INIT_AUTOMAKE([foreign subdir-objects])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])
AC_SUBST([LOG_DRIVER],['$(SHELL) $(top_srcdir)/../../test-driver-ohpc'])

# build with the loaded MPI toolchain

CXX=mpicxx

AC_PROG_CXX

# HPCG's own optimization flags, as the upstream setup/Make.* files set them
# for each compiler. HPCG is built with MPI and OpenMP, so no HPCG_NO_MPI or
# HPCG_NO_OPENMP here.

AS_CASE([$LMOD_FAMILY_COMPILER],
	[intel], [HPCG_CXXFLAGS="-O3 -qopenmp"],
	[acfl],  [HPCG_CXXFLAGS="-O3 -fsimdmath -fopenmp"],
	         [HPCG_CXXFLAGS="-O3 -ffast-math -ftree-vectorize -fopenmp"])
AC_SUBST([HPCG_CXXFLAGS])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

echo
echo '-------------------------------------------------- SUMMARY --------------------------------------------------'
echo
echo Package version............... : $PACKAGE-$VERSION
echo OHPC compiler toolchain....... : $LMOD_FAMILY_COMPILER
echo OHPC MPI toolchain............ : $LMOD_FAMILY_MPI
echo
echo C++ compiler.................. : `which ${CXX%% *}`
echo C++ compiler flags............ : $HPCG_CXXFLAGS
echo
echo '-------------------------------------------------------------------------------------------------------------'
