## This file should be placed in the root directory of your project.
## Then modify the CMakeLists.txt file in the root directory of your
## project to incorporate the testing dashboard.
## # The following are required to uses Dart and the Cdash dashboard
##   ENABLE_TESTING()
##   INCLUDE(CTest)
set (CTEST_NIGHTLY_START_TIME "18:00:00 CST")
set (CTEST_PROJECT_NAME "HDF5Examples")

set (CTEST_DROP_METHOD "http")
if (CDASH_LOCAL)
  set (CTEST_DROP_SITE "72.36.68.252")
  set (CTEST_DROP_LOCATION "/submit.php?project=HDF5Examples")
else (CDASH_LOCAL)
  set (CTEST_DROP_SITE "cdash.hdfgroup.uiuc.edu")
  set (CTEST_DROP_LOCATION "/submit.php?project=HDF5Examples")
endif (CDASH_LOCAL)
set (CTEST_DROP_SITE_CDASH TRUE)

set (UPDATE_TYPE svn)
set (VALGRIND_COMMAND "/usr/bin/valgrind")
set (VALGRIND_COMMAND_OPTIONS "-v --tool=memcheck --leak-check=full --track-fds=yes --num-callers=50 --show-reachable=yes --track-origins=yes --malloc-fill=0xff --free-fill=0xfe")
set (CTEST_MEMORYCHECK_COMMAND "/usr/bin/valgrind")
set (CTEST_MEMORYCHECK_COMMAND_OPTIONS "-v --tool=memcheck --leak-check=full --track-fds=yes --num-callers=50 --show-reachable=yes --track-origins=yes --malloc-fill=0xff --free-fill=0xfe")

set (CTEST_TESTING_TIMEOUT 1200) 
set (DART_TESTING_TIMEOUT 1200) 
