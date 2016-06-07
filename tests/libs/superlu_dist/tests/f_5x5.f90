
! -- Distributed SuperLU routine (version 2.0) --
! Lawrence Berkeley National Lab, Univ. of California Berkeley.
! July 20, 2004
!
!
      program f_5x5
! 
! Purpose
! =======
!
! This example illustrates how to use F_PDGSSVX with the full
! (default) options to solve a linear system.
! The input matrix is a small 5x5 example appeared in SuperLU Users' Guide,,
! Section 2.2:
!
!   [ s     u  u    ]     [ 19      21  21    ]
!   [ l  u          ]     [ 12  21            ]
!   [    l  p       ]  =  [     12  16        ]
!   [          e  u ]     [             5  21 ]
!   [ l  l        r ]     [ 12  12         18 ]
!
! It is set up to use 2 processors:
!    processor 1 contains the first 2 rows
!    processor 2 contains the last 3 rows
!
! Seven basic steps are required:
!   1. Create C structures used in SuperLU_DIST
!   2. Initialize the MPI environment and the SuperLU process grid
!   3. Set up the input matrix and the right-hand side
!   4. Set the options argument
!   5. Call f_pdgssvx
!   6. Release the process grid and terminate the MPI environment
!   7. Release all structures
!
      use superlu_mod
!      implicit none
      include 'mpif.h'
      integer maxn, maxnz, maxnrhs
      parameter ( maxn = 10, maxnz = 100, maxnrhs = 10 )
      integer colind(maxnz), rowptr(maxn+1)
      real*8  nzval(maxnz), b(maxn), berr(maxnrhs)
      integer n, m, nnz, nrhs, ldb, nprow, npcol, init
      integer*4 iam, info, i, ierr, ldb4
      integer nnz_loc, m_loc, fst_row
      real*8  s, u, p, e, r, l

      integer(superlu_ptr) :: grid
      integer(superlu_ptr) :: options
      integer(superlu_ptr) :: ScalePermstruct
      integer(superlu_ptr) :: LUstruct
      integer(superlu_ptr) :: SOLVEstruct
      integer(superlu_ptr) :: A
      integer(superlu_ptr) :: stat

! Initialize MPI environment 
      call mpi_init(ierr)

! Check malloc
!      call f_check_malloc(iam)

! Create Fortran handles for the C structures used in SuperLU_DIST
      call f_create_gridinfo_handle(grid)
      call f_create_options_handle(options)
      call f_create_ScalePerm_handle(ScalePermstruct)
      call f_create_LUstruct_handle(LUstruct)
      call f_create_SOLVEstruct_handle(SOLVEstruct)
      call f_create_SuperMatrix_handle(A)
      call f_create_SuperLUStat_handle(stat)

! Initialize the SuperLU_DIST process grid
      nprow = 1
      npcol = 2
      call f_superlu_gridinit(MPI_COMM_WORLD, nprow, npcol, grid)

! Bail out if I do not belong in the grid. 
      call get_GridInfo(grid, iam=iam)
      if ( iam >= nprow * npcol ) then 
         go to 100
      endif
      if ( iam == 0 ) then 
         write(*,*) ' Process grid ', nprow, ' X ', npcol
         write(*,*) ' default integer size ', kind(0) 
      endif
!
!*************************************************************************
! Set up the input matrix A
!*************************************************************************
! The input matrix is a small 5x5 example appeared in SuperLU Users' Guide:
!
!   [ s     u  u    ]     [ 19      21  21    ]
!   [ l  u          ]     [ 12  21            ]
!   [    l  p       ]  =  [     12  16        ]
!   [          e  u ]     [             5  21 ]
!   [ l  l        r ]     [ 12  12         18 ]
!
! It is set up to use 2 processors:
!    processor 1 contains the first 2 rows
!    processor 2 contains the last 3 rows
!
      m = 5
      n = 5
      nnz = 12
      s = 19.0
      u = 21.0
      p = 16.0
      e = 5.0
      r = 18.0
      l = 12.0
!      
      if ( iam == 0 ) then
! Processor 0 owns the first 2 rows of the matrix
! NOTE: 0-based indexing must be used for the C routines.
         nnz_loc   = 5
         m_loc     = 2
         fst_row   = 0         ! 0-based indexing
         nzval(1)  = s
         colind(1) = 0         ! 0-based indexing
         nzval(2)  = u
         colind(2) = 2
         nzval(3)  = u
         colind(3) = 3
         nzval(4)  = l
         colind(4) = 0
         nzval(5)  = u
         colind(5) = 1
         rowptr(1) = 0         ! 0-based indexing
         rowptr(2) = 3
         rowptr(3) = 5
      else
! Processor 1 owns the last 3 rows of the matrix
         nnz_loc   = 7
         m_loc     = 3
         fst_row   = 2         ! 0-based indexing
         nzval(1)  = l
         colind(1) = 1
         nzval(2)  = p
         colind(2) = 2
         nzval(3)  = e
         colind(3) = 3
         nzval(4)  = u
         colind(4) = 4
         nzval(5)  = l
         colind(5) = 0
         nzval(6)  = l
         colind(6) = 1
         nzval(7)  = r
         colind(7) = 4
         rowptr(1) = 0         ! 0-based indexing
         rowptr(2) = 2
         rowptr(3) = 4
         rowptr(4) = 7
      endif

      if ( iam == 0 ) then 
         write(*,*) ' Matrix A was set up'
      endif

! Create the distributed compressed row matrix pointed to by the F90 handle A
      call f_dCreate_CompRowLoc_Mat_dist(A, m, n, nnz_loc, m_loc, fst_row, &
           nzval, colind, rowptr, SLU_NR_loc, SLU_D, SLU_GE)

! Setup the right hand side
      call get_CompRowLoc_Matrix(A, nrow_loc=ldb)
      do i = 1, ldb
         b(i) = 1.0
      enddo
      nrhs = 1
      ldb4 = ldb

! Set the default input options
      call f_set_default_options(options)

! Modify one or more options
      call set_superlu_options(options,ColPerm=NATURAL)
      call set_superlu_options(options,RowPerm=NOROWPERM)

! Initialize ScalePermstruct and LUstruct
      call get_SuperMatrix(A,nrow=m,ncol=n)
      call f_ScalePermstructInit(m, n, ScalePermstruct)
      call f_LUstructInit(m, n, LUstruct)

! Initialize the statistics variables
      call f_PStatInit(stat)

! Call the linear equation solver
      call f_pdgssvx(options, A, ScalePermstruct, b, ldb4, nrhs, &
                     grid, LUstruct, SOLVEstruct, berr, stat, info)

      if (info == 0 .and. iam == 1) then
         write (*,*) 'Backward error: ', (berr(i), i = 1, nrhs)
      else
         write(*,*) 'INFO from f_pdgssvx = ', info
      endif

! Deallocate the storage allocated by SuperLU_DIST
      call f_PStatFree(stat)
      call f_Destroy_SuperMat_Store_dist(A)
      call f_ScalePermstructFree(ScalePermstruct)
      call f_Destroy_LU(n, grid, LUstruct)
      call f_LUstructFree(LUstruct)
      call get_superlu_options(options, SolveInitialized=init)
      if (init == YES) then
         call f_dSolveFinalize(options, SOLVEstruct)
      endif

! Release the SuperLU process grid
100   call f_superlu_gridexit(grid)

! Deallocate the C structures pointed to by the Fortran handles
      call f_destroy_gridinfo_handle(grid)
      call f_destroy_options_handle(options)
      call f_destroy_ScalePerm_handle(ScalePermstruct)
      call f_destroy_LUstruct_handle(LUstruct)
      call f_destroy_SOLVEstruct_handle(SOLVEstruct)
      call f_destroy_SuperMatrix_handle(A)
      call f_destroy_SuperLUStat_handle(stat)

! Check malloc
!      call f_check_malloc(iam)

! Terminate the MPI execution environment
      call mpi_finalize(ierr)

      stop
      end
