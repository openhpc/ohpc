AUTOMAKE_OPTIONS = foreign subdir-objects
AM_MAKEFLAGS     = --no-print-directory

# -DMPICH_IGNORE_CXX_SEEK as the upstream HPCG setup files set it.
AM_CPPFLAGS      = -I$(srcdir)/src -DMPICH_IGNORE_CXX_SEEK
AM_CXXFLAGS      = @HPCG_CXXFLAGS@

# make check builds the benchmark before it runs the tests below, so the
# tests never have to build it themselves.
check_PROGRAMS   = xhpcg
xhpcg_SOURCES    = \
	src/CG.cpp \
	src/CG_ref.cpp \
	src/CheckAspectRatio.cpp \
	src/CheckProblem.cpp \
	src/ComputeDotProduct.cpp \
	src/ComputeDotProduct_ref.cpp \
	src/ComputeMG.cpp \
	src/ComputeMG_ref.cpp \
	src/ComputeOptimalShapeXYZ.cpp \
	src/ComputeProlongation_ref.cpp \
	src/ComputeResidual.cpp \
	src/ComputeRestriction_ref.cpp \
	src/ComputeSPMV.cpp \
	src/ComputeSPMV_ref.cpp \
	src/ComputeSYMGS.cpp \
	src/ComputeSYMGS_ref.cpp \
	src/ComputeWAXPBY.cpp \
	src/ComputeWAXPBY_ref.cpp \
	src/ExchangeHalo.cpp \
	src/finalize.cpp \
	src/GenerateCoarseProblem.cpp \
	src/GenerateGeometry.cpp \
	src/GenerateProblem.cpp \
	src/GenerateProblem_ref.cpp \
	src/init.cpp \
	src/main.cpp \
	src/MixedBaseCounter.cpp \
	src/mytimer.cpp \
	src/OptimizeProblem.cpp \
	src/ReadHpcgDat.cpp \
	src/ReportResults.cpp \
	src/SetupHalo.cpp \
	src/SetupHalo_ref.cpp \
	src/TestCG.cpp \
	src/TestNorms.cpp \
	src/TestSymmetry.cpp \
	src/WriteProblem.cpp \
	src/YAML_Doc.cpp \
	src/YAML_Element.cpp


TESTS            = rm_execution_single_host
TESTS           += rm_execution_multi_host
