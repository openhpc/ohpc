TESTS                            = rm_execution

check_PROGRAMS                   = lesson_kokkos_memspace
lesson_kokkos_memspace_SOURCES   = lesson_kokkos_memspace.cpp
lesson_kokkos_memspace_LDADD     = -lkokkoscore

check_PROGRAMS                  += lesson_tpetra_init
lesson_tpetra_init_SOURCES       = lesson_tpetra_init.cpp
lesson_tpetra_init_LDADD         = -ltpetra -lteuchoscomm -lteuchoscore

check_PROGRAMS                  += lesson_tpetra_dataredist
lesson_tpetra_dataredist_SOURCES = lesson_tpetra_dataredist.cpp
lesson_tpetra_dataredist_LDADD   = -lkokkoscore -ltpetra -lteuchoscomm -lteuchoscore

check_PROGRAMS                  += lesson_teuchos_pl
lesson_teuchos_pl_SOURCES        = lesson_teuchos_pl.cpp
lesson_teuchos_pl_LDADD          = -lteuchosparameterlist -lteuchoscore

check_PROGRAMS                  += lesson_teuchos_sdm
lesson_teuchos_sdm_SOURCES       = lesson_teuchos_sdm.cpp
lesson_teuchos_sdm_LDADD         = -lteuchosnumerics

check_PROGRAMS                  += lesson_teuchos_blas
lesson_teuchos_blas_SOURCES      = lesson_teuchos_blas.cpp
lesson_teuchos_blas_LDADD        = -lteuchosparameterlist -lteuchoscore

check_PROGRAMS                  += lesson_teuchos_lapack
lesson_teuchos_lapack_SOURCES    = lesson_teuchos_lapack.cpp
lesson_teuchos_lapack_LDADD      = -lteuchosnumerics
