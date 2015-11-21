!
! Copyright (C) 2002-2009 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
MODULE mp_global
  !----------------------------------------------------------------------------
  !
  USE mp, ONLY : mp_comm_free, mp_size, mp_rank, mp_sum, mp_barrier, &
       mp_bcast, mp_start, mp_end
  USE io_global, ONLY : stdout, io_global_start, meta_ionode_id, meta_ionode
  USE parallel_include
  !
  IMPLICIT NONE 
  SAVE
  !
  ! ... World group (all processors)
  !
  INTEGER :: mpime = 0  ! processor index (starts from 0 to nproc-1)
  INTEGER :: root  = 0  ! index of the root processor
  INTEGER :: nproc = 1  ! number of processors
  INTEGER :: world_comm = 0  ! communicator
  !
  ! ... Image groups (processors within an image)
  !
  INTEGER :: nimage    = 1 ! number of images
  INTEGER :: me_image  = 0 ! index of the processor within an image
  INTEGER :: root_image= 0 ! index of the root processor within an image
  INTEGER :: my_image_id=0 ! index of my image
  INTEGER :: nproc_image=1 ! number of processors within an image
  INTEGER :: inter_image_comm = 0  ! inter image communicator
  INTEGER :: intra_image_comm = 0  ! intra image communicator  
  !
  ! ... Pool groups (processors within a pool of k-points)
  !
  INTEGER :: npool       = 1  ! number of "k-points"-pools
  INTEGER :: me_pool     = 0  ! index of the processor within a pool 
  INTEGER :: root_pool   = 0  ! index of the root processor within a pool
  INTEGER :: my_pool_id  = 0  ! index of my pool
  INTEGER :: nproc_pool  = 1  ! number of processors within a pool
  INTEGER :: inter_pool_comm  = 0  ! inter pool communicator
  INTEGER :: intra_pool_comm  = 0  ! intra pool communicator
  !
  ! ... Band groups (processors within a pool of bands)
  !
  INTEGER :: nbgrp       = 1  ! number of band groups
  INTEGER :: me_bgrp     = 0  ! index of the processor within a band group
  INTEGER :: root_bgrp   = 0  ! index of the root processor within a band group
  INTEGER :: my_bgrp_id  = 0  ! index of my band group
  INTEGER :: nproc_bgrp  = 1  ! number of processor within a band group
  INTEGER :: inter_bgrp_comm  = 0  ! inter band group communicator
  INTEGER :: intra_bgrp_comm  = 0  ! intra band group communicator  
  INTEGER :: ibnd_start = 0 !starting band index
  INTEGER :: ibnd_end = 0   !ending band index
  !
  ! ... ortho (or linear-algebra) groups
  !
  INTEGER :: np_ortho(2) = 1  ! size of the processor grid used in ortho
  INTEGER :: me_ortho(2) = 0  ! coordinates of the processors
  INTEGER :: me_ortho1   = 0  ! task id for the ortho group
  INTEGER :: nproc_ortho = 1  ! size of the ortho group:
  INTEGER :: leg_ortho   = 1  ! the distance in the father communicator
                              ! of two neighbour processors in ortho_comm
  INTEGER :: ortho_comm  = 0  ! communicator for the ortho group
  INTEGER :: ortho_comm_id= 0 ! id of the ortho_comm
  !
!!$ #if defined __SCALAPACK
  INTEGER :: me_blacs   =  0  ! BLACS processor index starting from 0
  INTEGER :: np_blacs   =  1  ! BLACS number of processor
  INTEGER :: world_cntx = -1  ! BLACS context of all processor 
  INTEGER :: ortho_cntx = -1  ! BLACS context for ortho_comm
!!$ #endif
  !
  ! ... "task" groups (for band parallelization of FFT)
  !
  INTEGER :: ntask_groups = 1  ! number of proc. in an orbital "task group" 
  !
  ! ... Misc parallelization info
  ! 
  INTEGER :: kunit = 1  ! granularity of k-point distribution
  ! ... number of processors written in the data file for checkin purposes:
  INTEGER :: nproc_file = 1        ! world group
  INTEGER :: nproc_image_file = 1  ! in an image
  INTEGER :: nproc_pool_file  = 1  ! in a pool
  !
  PRIVATE :: init_pools, init_bands, init_ortho
  PRIVATE :: ntask_groups
  !
CONTAINS
  !
  !-----------------------------------------------------------------------
  SUBROUTINE mp_startup ( )
    !-----------------------------------------------------------------------
    ! ... This subroutine initializes MPI
    ! ... Processes are organized in NIMAGE images each dealing with a subset of
    ! ... images used to discretize the "path" (only in "path" optimizations)
    ! ... Within each image processes are organized in NPOOL pools each dealing
    ! ... with a subset of kpoints.
    ! ... Within each pool R & G space distribution is performed.
    ! ... NPROC is read from command line or can be set with the appropriate
    ! ... environment variable ( for example use 'setenv MP_PROCS 8' on IBM SP
    ! ... machine to run on NPROC=8 processors ); NIMAGE and NPOOL are read from
    ! ... command line.
    ! ... NPOOL must be a whole divisor of NPROC
    !
    IMPLICIT NONE
    INTEGER :: world, nproc_ortho_in = 0
    INTEGER :: root = 0
    !
    !
    ! ... get the basic parameters from communications sub-system
    ! ... to handle processors
    ! ... mpime = processor number, starting from 0
    ! ... nproc = number of processors
    ! ... world = group index of all processors
    !
    CALL mp_start( nproc, mpime, world )
    !
    !
    ! ... now initialize processors and groups variables
    ! ... set global coordinate for this processor
    ! ... root  = index of the root processor
    !
    CALL mp_global_start( root, mpime, world, nproc )
    !
    ! ... initialize input/output, set (and get) the I/O nodes
    !
    CALL io_global_start( mpime, root )
    !
    meta_ionode = ( mpime == root )
    meta_ionode_id = root
    !
    IF ( meta_ionode ) THEN
       !
       ! ... How many parallel images ?
       !
       CALL get_arg_nimage( nimage )
       !
       nimage = MAX( nimage, 1 )
       nimage = MIN( nimage, nproc )
       !
       ! ... How many band groups?
       !
       CALL get_arg_nbgrp( nbgrp )
       !
       nbgrp = MAX( nbgrp, 1 )
       nbgrp = MIN( nbgrp, nproc )
       !
       ! ... How many k-point pools ?
       !
       CALL get_arg_npool( npool )
       !
       npool = MAX( npool, 1 )
       npool = MIN( npool, nproc )
       !
       ! ... How many task groups ?
       !
       CALL get_arg_ntg( ntask_groups )
       !
       ! ... How many processors involved in diagonalization of Hamiltonian ?
       !
       CALL get_arg_northo( nproc_ortho_in )
       !
       ! ... nproc_ortho_in = 0 if no command-line option -ndiag or -northo
       ! ... nproc_ortho_in = N if -ndiag N or -northo N comm-line opt present
       !
    END IF
    !
    CALL mp_barrier()
    !
    ! ... broadcast input parallelization options to all processors
    !
    CALL mp_bcast( npool,  meta_ionode_id )
    CALL mp_bcast( nimage, meta_ionode_id )
    CALL mp_bcast( nbgrp, meta_ionode_id )
    CALL mp_bcast( ntask_groups, meta_ionode_id )
    CALL mp_bcast( nproc_ortho_in, meta_ionode_id )
    !
    ! ... initialize images, band, k-point, ortho groups in sequence
    !
    CALL init_images( world_comm )
    !
    CALL init_pools( intra_image_comm )
    !
    CALL init_bands( intra_pool_comm )
    !
    CALL init_ortho( nproc_ortho_in, intra_bgrp_comm )
    !
    !
    RETURN
    !
  END SUBROUTINE mp_startup
  !
  !-----------------------------------------------------------------------
  SUBROUTINE mp_global_start( root_i, mpime_i, group_i, nproc_i )
    !-----------------------------------------------------------------------
    !
    IMPLICIT NONE
    !
    INTEGER, INTENT(IN) :: root_i, mpime_i, group_i, nproc_i
    !
    root             = root_i
    mpime            = mpime_i
    world_comm       = group_i
    nproc            = nproc_i
    nproc_pool       = nproc_i
    nproc_image      = nproc_i
    nproc_bgrp       = nproc_i
    my_pool_id       = 0
    my_image_id      = 0
    my_bgrp_id       = 0
    me_pool          = mpime
    me_image         = mpime
    me_bgrp          = mpime
    root_pool        = root
    root_image       = root
    root_bgrp        = root
    inter_pool_comm  = group_i
    intra_pool_comm  = group_i
    inter_image_comm = group_i
    intra_image_comm = group_i
    inter_bgrp_comm  = group_i
    intra_bgrp_comm  = group_i
    ortho_comm       = group_i
    !
    RETURN
    !
  END SUBROUTINE mp_global_start
  !
  !-----------------------------------------------------------------------
  SUBROUTINE mp_startup_new (root, world ) 
    !-----------------------------------------------------------------------
    ! ... This subroutine initializes MPI
    ! ... Processes are organized in NIMAGE images each dealing with a subset of
    ! ... images used to discretize the "path" (only in "path" optimizations)
    ! ... Within each image processes are organized in NPOOL pools each dealing
    ! ... with a subset of kpoints.
    ! ... Within each pool R & G space distribution is performed.
    ! ... NPROC is read from command line or can be set with the appropriate
    ! ... environment variable ( for example use 'setenv MP_PROCS 8' on IBM SP
    ! ... machine to run on NPROC=8 processors ); NIMAGE and NPOOL are read from
    ! ... command line.
    ! ... NPOOL must be a whole divisor of NPROC
    !
    IMPLICIT NONE
    !
    INTEGER, INTENT(IN) :: world, root
    INTEGER :: nproc_ortho_in = 0 
    !
    INTEGER :: myrank, npe
    !
    ! ... now initialize processors and groups variables
    ! ... set global coordinate for this processor
    ! ... root  = index of the root processor
    !
    myrank = mp_rank(world)
    npe = mp_size(world)
    !
    CALL mp_global_start_new( root, myrank, world, npe )
    !
    ! ... initialize input/output, set (and get) the I/O nodes
    !
    CALL io_global_start( myrank, root )
    !
    IF ( myrank == root ) THEN
       !
       ! ... How many band groups?
       !
       CALL get_arg_nbgrp( nbgrp )
       !
       nbgrp = MAX( nbgrp, 1 )
       nbgrp = MIN( nbgrp, nproc )
       !
       ! ... How many k-point pools ?
       !
       CALL get_arg_npool( npool )
       !
       npool = MAX( npool, 1 )
       npool = MIN( npool, nproc )
       !
       ! ... How many task groups ?
       !
       CALL get_arg_ntg( ntask_groups )
       !
       ! ... How many processors involved in diagonalization of Hamiltonian ?
       !
       CALL get_arg_northo( nproc_ortho_in )
       !
       ! ... nproc_ortho_in = 0 if no command-line option -ndiag or -northo
       ! ... nproc_ortho_in = N if -ndiag N or -northo N comm-line opt present
       ! 
    END IF
    !
    CALL mp_barrier(world)
    !
    ! ... broadcast input parallelization options to all processors
    !
    CALL mp_bcast( npool,  root, world )
    CALL mp_bcast( nbgrp, root, world )
    CALL mp_bcast( ntask_groups, root, world )
    CALL mp_bcast( nproc_ortho_in, root, world )
    !
    ! ... initialize images, band, k-point, ortho groups in sequence
    !
    !
    CALL init_pools( intra_image_comm )
    !
    CALL init_bands( intra_pool_comm )
    !
    CALL init_ortho( nproc_ortho_in, intra_bgrp_comm )
    !
    !
    RETURN
    !
  END SUBROUTINE mp_startup_new
  !
  !-----------------------------------------------------------------------
  SUBROUTINE mp_global_start_new( root_i, mpime_i, group_i, nproc_i )
    !-----------------------------------------------------------------------
    !
    IMPLICIT NONE
    !
    INTEGER, INTENT(IN) :: root_i, mpime_i, group_i, nproc_i
    !
    root             = root_i
    mpime            = mpime_i
    world_comm       = group_i
    nproc            = nproc_i
    nproc_pool       = nproc_i
    nproc_bgrp       = nproc_i
    my_pool_id       = 0
    my_bgrp_id       = 0
    me_pool          = mpime
    me_bgrp          = mpime
    root_pool        = root
    root_bgrp        = root
    inter_pool_comm  = group_i
    intra_pool_comm  = group_i
    inter_bgrp_comm  = group_i
    intra_bgrp_comm  = group_i
    ortho_comm       = group_i
    !
    RETURN
    !
  END SUBROUTINE mp_global_start_new
  !
  !-----------------------------------------------------------------------
  SUBROUTINE mp_global_end ( )
    !-----------------------------------------------------------------------
    !
    CALL mp_barrier()
    CALL mp_end ()
    !
  END SUBROUTINE mp_global_end
  !
  !-----------------------------------------------------------------------     
  SUBROUTINE mp_global_group_start( mep, myp, nprocp, num_of_pools )
    !-----------------------------------------------------------------------
    !
    IMPLICIT NONE
    !     
    INTEGER, INTENT(IN) :: mep, myp, nprocp, num_of_pools
    !
    me_pool    = mep
    my_pool_id = myp
    nproc_pool = nprocp
    npool      = num_of_pools
    !
    RETURN
    !
  END SUBROUTINE mp_global_group_start
  !
  !----------------------------------------------------------------------------
  SUBROUTINE init_images ( parent_comm )
    !---------------------------------------------------------------------------
    !
    ! ... This routine divides all MPI processors into images
    !
    IMPLICIT NONE
    !
    INTEGER, INTENT(IN) :: parent_comm
    !
    INTEGER :: ierr = 0
    INTEGER :: parent_nproc = 1
    INTEGER :: parent_mype  = 0
    !
#if defined (__MPI)
    !
    parent_nproc = mp_size( parent_comm )
    parent_mype  = mp_rank( parent_comm )
    !
    IF ( nimage < 1 .OR. nimage > parent_nproc ) &
       CALL errore( 'init_images', 'invalid number of images, out of range', 1 )
    IF ( MOD( nproc, nimage ) /= 0 ) &
       CALL errore( 'init_images', 'n. of images must be divisor of parent_nproc', 1 )
    !
    ! ... set number of cpus per image
    !
    nproc_image = parent_nproc / nimage
    !
    ! ... set index of image for this processor   ( 0 : nimage - 1 )
    !
    my_image_id = parent_mype / nproc_image
    !
    ! ... set index of processor within the image ( 0 : nproc_image - 1 )
    !
    me_image    = MOD( parent_mype, nproc_image )
    !
    CALL mp_barrier( parent_comm )
    !
    ! ... the intra_image_comm communicator is created
    !
    CALL MPI_COMM_SPLIT( parent_comm, my_image_id, parent_mype, intra_image_comm, ierr )
    IF ( ierr /= 0 ) CALL errore &
       ( 'init_images', 'intra image communicator initialization', ABS(ierr) )
    !
    CALL mp_barrier( parent_comm )
    !
    ! ... the inter_image_comm communicator is created
    !
    CALL MPI_COMM_SPLIT( parent_comm, me_image, parent_mype, inter_image_comm, ierr )
    IF ( ierr /= 0 ) CALL errore &
       ( 'init_images', 'inter image communicator initialization', ABS(ierr) )
#endif
    RETURN
    !
  END SUBROUTINE init_images

  !----------------------------------------------------------------------------
  SUBROUTINE init_bands( parent_comm )
    !---------------------------------------------------------------------------
    !
    ! ... This routine divides parent_comm into band pools
    !
    IMPLICIT NONE
    !
    INTEGER, INTENT(IN) :: parent_comm
    !
    INTEGER :: ierr = 0
    INTEGER :: parent_nproc = 1
    INTEGER :: parent_mype  = 0
    !
#if defined (__MPI)
    !
    parent_nproc = mp_size( parent_comm )
    parent_mype  = mp_rank( parent_comm )
    !
    IF ( nbgrp < 1 .OR. nbgrp > parent_nproc ) &
       CALL errore( 'init_bands', 'invalid number of band groups, out of range', 1 )
    IF ( MOD( parent_nproc, nbgrp ) /= 0 ) &
       CALL errore( 'init_bands', 'n. of band groups  must be divisor of parent_nproc', 1 )
    ! 
    ! ... Set number of processors per band group
    !
    nproc_bgrp = parent_nproc / nbgrp
    !
    ! ... set index of band group for this processor   ( 0 : nbgrp - 1 )
    !
    my_bgrp_id = parent_mype / nproc_bgrp
    !
    ! ... set index of processor within the image ( 0 : nproc_image - 1 )
    !
    me_bgrp    = MOD( parent_mype, nproc_bgrp )
    !
    CALL mp_barrier( parent_comm )
    !
    ! ... the intra_bgrp_comm communicator is created
    !
    CALL MPI_COMM_SPLIT( parent_comm, my_bgrp_id, parent_mype, intra_bgrp_comm, ierr )
    !
    IF ( ierr /= 0 ) &
       CALL errore( 'init_bands', 'intra band group communicator initialization', ABS(ierr) )
    !
    CALL mp_barrier( parent_comm )
    !
    ! ... the inter_bgrp_comm communicator is created                     
    !     
    CALL MPI_COMM_SPLIT( parent_comm, me_bgrp, parent_mype, inter_bgrp_comm, ierr )  
    !
    IF ( ierr /= 0 ) &
       CALL errore( 'init_bands', 'inter band group communicator initialization', ABS(ierr) )
    !
#endif
    RETURN
    !
  END SUBROUTINE init_bands
  !
  !----------------------------------------------------------------------------
  SUBROUTINE init_pools( parent_comm )
    !---------------------------------------------------------------------------
    !
    ! ... This routine divides band groups into k-point pools
    !
    IMPLICIT NONE
    !
    INTEGER, INTENT(IN) :: parent_comm
    !
    INTEGER :: ierr = 0
    INTEGER :: parent_nproc = 1
    INTEGER :: parent_mype  = 0
    !
#if defined (__MPI)
    !
    parent_nproc = mp_size( parent_comm )
    parent_mype  = mp_rank( parent_comm )
    !
    ! ... number of cpus per pool of k-points (they are created inside each image)
    !
    nproc_pool = parent_nproc / npool
    !
    IF ( MOD( parent_nproc, npool ) /= 0 ) &
         CALL errore( 'init_pools', 'invalid number of pools, parent_nproc /= nproc_pool * npool', 1 )  
    !
    ! ... my_pool_id  =  pool index for this processor    ( 0 : npool - 1 )
    ! ... me_pool     =  processor index within the pool  ( 0 : nproc_pool - 1 )
    !
    my_pool_id = parent_mype / nproc_pool    
    me_pool    = MOD( parent_mype, nproc_pool )
    !
    CALL mp_barrier( parent_comm )
    !
    ! ... the intra_pool_comm communicator is created
    !
    CALL MPI_COMM_SPLIT( parent_comm, my_pool_id, parent_mype, intra_pool_comm, ierr )
    !
    IF ( ierr /= 0 ) &
       CALL errore( 'init_pools', 'intra pool communicator initialization', ABS(ierr) )
    !
    CALL mp_barrier( parent_comm )
    !
    ! ... the inter_pool_comm communicator is created
    !
    CALL MPI_COMM_SPLIT( parent_comm, me_pool, parent_mype, inter_pool_comm, ierr )
    !
    IF ( ierr /= 0 ) &
       CALL errore( 'init_pools', 'inter pool communicator initialization', ABS(ierr) )
    !
#endif
    !
    RETURN
  END SUBROUTINE init_pools
  !
  !----------------------------------------------------------------------------
  SUBROUTINE init_ortho( nproc_ortho_in, parent_comm )
    !---------------------------------------------------------------------------
    !
    ! ... Ortho group initialization
    !
    IMPLICIT NONE
    !
    INTEGER, INTENT(IN) :: nproc_ortho_in! read from command-line, 0 if unset
    INTEGER, INTENT(IN) :: parent_comm   ! communicator of the parent group
    !
    INTEGER :: nproc_ortho_try
    INTEGER :: parent_nproc  ! nproc of the parent group
    INTEGER :: ierr = 0
    !
    parent_nproc = mp_size( parent_comm )
    !
!!$ #if defined __SCALAPACK

    ! define a 1D grid containing all MPI task of MPI_COMM_WORLD communicator
    !
    CALL BLACS_PINFO( me_blacs, np_blacs )
    CALL BLACS_GET( -1, 0, world_cntx )
    CALL BLACS_GRIDINIT( world_cntx, 'Row', 1, np_blacs )
    !
!!$ #endif
    !
    IF( nproc_ortho_in > 0 ) THEN
       ! command-line argument -ndiag N or -nproc N set to N
       ! use the command line value ensuring that it falls in the proper range
       nproc_ortho_try = MIN( nproc_ortho_in , parent_nproc )
    ELSE 
       ! no command-line argument -ndiag N or -nproc N is present
       ! insert here custom architecture specific default definitions
!!$ #if defined __SCALAPACK
       nproc_ortho_try = MAX( parent_nproc/2, 1 )
!!$#else
!!$       nproc_ortho_try = 1
!!$#endif
    END IF
    !
    ! the ortho group for parallel linear algebra is a sub-group of the pool,
    ! then there are as many ortho groups as pools.
    !
    CALL init_ortho_group( nproc_ortho_try, parent_comm )
    !  
    RETURN
    !
  END SUBROUTINE init_ortho
  !
  !
  SUBROUTINE init_ortho_group( nproc_try_in, comm_all )
    !
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: nproc_try_in, comm_all

    LOGICAL, SAVE :: first = .true.
    INTEGER :: ierr, color, key, me_all, nproc_all, nproc_try

!!$ #if defined __SCALAPACK
    INTEGER, ALLOCATABLE :: blacsmap(:,:)
    INTEGER, ALLOCATABLE :: ortho_cntx_pe(:,:,:)
    INTEGER :: nprow, npcol, myrow, mycol, i, j, k
    INTEGER, EXTERNAL :: BLACS_PNUM
    !
    INTEGER :: nparent=1
    INTEGER :: total_nproc=1
    INTEGER :: total_mype=0
    INTEGER :: nproc_parent=1
    INTEGER :: my_parent_id=0
!!$ #endif


#if defined __MPI

    me_all    = mp_rank( comm_all )
    !
    nproc_all = mp_size( comm_all )
    !
    nproc_try = MIN( nproc_try_in, nproc_all )
    nproc_try = MAX( nproc_try, 1 )

    IF( .NOT. first ) THEN
       !  
       !  free resources associated to the communicator
       !
       CALL mp_comm_free( ortho_comm )
       !
!!$ #if defined __SCALAPACK
       IF(  ortho_comm_id > 0  ) THEN
          CALL BLACS_GRIDEXIT( ortho_cntx )
       ENDIF
       ortho_cntx = -1
!!$ #endif
       !
    END IF

    !  find the square closer (but lower) to nproc_try
    !
    CALL grid2d_dims( 'S', nproc_try, np_ortho(1), np_ortho(2) )
    !
    !  now, and only now, it is possible to define the number of tasks
    !  in the ortho group for parallel linear algebra
    !
    nproc_ortho = np_ortho(1) * np_ortho(2)
    !
    IF( nproc_all >= 4*nproc_ortho ) THEN
       !
       !  here we choose a processor every 4, in order not to stress memory BW
       !  on multi core procs, for which further performance enhancements are
       !  possible using OpenMP BLAS inside regter/cegter/rdiaghg/cdiaghg
       !  (to be implemented)
       !
       color = 0
       IF( me_all < 4*nproc_ortho .AND. MOD( me_all, 4 ) == 0 ) color = 1
       !
       leg_ortho = 4
       !
    ELSE IF( nproc_all >= 2*nproc_ortho ) THEN
       !
       !  here we choose a processor every 2, in order not to stress memory BW
       !
       color = 0
       IF( me_all < 2*nproc_ortho .AND. MOD( me_all, 2 ) == 0 ) color = 1
       !
       leg_ortho = 2
       !
    ELSE
       !
       !  here we choose the first processors
       !
       color = 0
       IF( me_all < nproc_ortho ) color = 1
       !
       leg_ortho = 1
       !
    END IF
    !
    key   = me_all
    !
    !  initialize the communicator for the new group by splitting the input communicator
    !
    CALL MPI_COMM_SPLIT( comm_all, color, key, ortho_comm, ierr )
    IF( ierr /= 0 ) &
         CALL errore( " init_ortho_group ", " initializing ortho group communicator ", ierr )
    !
    !  Computes coordinates of the processors, in row maior order
    !
    me_ortho1   = mp_rank( ortho_comm )
    !
    IF( me_all == 0 .AND. me_ortho1 /= 0 ) &
         CALL errore( " init_ortho_group ", " wrong root task in ortho group ", ierr )
    !
    if( color == 1 ) then
       ortho_comm_id = 1
       CALL GRID2D_COORDS( 'R', me_ortho1, np_ortho(1), np_ortho(2), me_ortho(1), me_ortho(2) )
       CALL GRID2D_RANK( 'R', np_ortho(1), np_ortho(2), me_ortho(1), me_ortho(2), ierr )
       IF( ierr /= me_ortho1 ) &
            CALL errore( " init_ortho_group ", " wrong task coordinates in ortho group ", ierr )
       IF( me_ortho1*leg_ortho /= me_all ) &
            CALL errore( " init_ortho_group ", " wrong rank assignment in ortho group ", ierr )
    else
       ortho_comm_id = 0
       me_ortho(1) = me_ortho1
       me_ortho(2) = me_ortho1
    endif
!!$ #if defined __SCALAPACK
    !
    !  This part is used to eliminate the image dependency from ortho groups
    !  SCALAPACK is now independent of whatever level of parallelization
    !  is present on top of pool parallelization
    !
    total_nproc = mp_size(mpi_comm_world)
    total_mype = mp_rank(mpi_comm_world)
    nparent = total_nproc/npool/nproc_pool
    nproc_parent = total_nproc/nparent
    my_parent_id = total_mype/nproc_parent
    !
    !
    ALLOCATE( ortho_cntx_pe( npool, nbgrp, nparent ) )
    ALLOCATE( blacsmap( np_ortho(1), np_ortho(2) ) )

    DO j = 1, nparent

     DO k = 1, nbgrp

       DO i = 1, npool

         CALL BLACS_GET( -1, 0, ortho_cntx_pe( i, k, j ) ) ! take a default value 

         blacsmap = 0
         nprow = np_ortho(1)
         npcol = np_ortho(2)

         IF( ( j == ( my_parent_id + 1 ) ) .and. ( k == ( my_bgrp_id + 1 ) ) .and.  &
             ( i == ( my_pool_id  + 1 ) ) .and. ( ortho_comm_id > 0 ) ) THEN

           blacsmap( me_ortho(1) + 1, me_ortho(2) + 1 ) = BLACS_PNUM( world_cntx, 0, me_blacs )

         END IF

         ! All MPI tasks defined in world comm take part in the definition of the BLACS grid

         CALL mp_sum( blacsmap ) 

         CALL BLACS_GRIDMAP( ortho_cntx_pe(i,k,j), blacsmap, nprow, nprow, npcol )

         CALL BLACS_GRIDINFO( ortho_cntx_pe(i,k,j), nprow, npcol, myrow, mycol )

         IF( ( j == ( my_parent_id + 1 ) ) .and. ( k == ( my_bgrp_id + 1 ) ) .and. &
             ( i == ( my_pool_id  + 1 ) ) .and. ( ortho_comm_id > 0 ) ) THEN

            IF(  np_ortho(1) /= nprow ) &
               CALL errore( ' init_ortho_group ', ' problem with SCALAPACK, wrong no. of task rows ', 1 )
            IF(  np_ortho(2) /= npcol ) &
               CALL errore( ' init_ortho_group ', ' problem with SCALAPACK, wrong no. of task columns ', 1 )
            IF(  me_ortho(1) /= myrow ) &
               CALL errore( ' init_ortho_group ', ' problem with SCALAPACK, wrong task row ID ', 1 )
            IF(  me_ortho(2) /= mycol ) &
               CALL errore( ' init_ortho_group ', ' problem with SCALAPACK, wrong task columns ID ', 1 )

            ortho_cntx = ortho_cntx_pe(i,k,j)

         END IF

       END DO

     END DO

    END DO

    DEALLOCATE( blacsmap )
    DEALLOCATE( ortho_cntx_pe )


!!$ #endif

!!$#else
!!$
!!$    ortho_comm_id = 1

#endif

    first = .false.

    RETURN
  END SUBROUTINE init_ortho_group
  !
  !
  SUBROUTINE distribute_over_bgrp( i2g, nl, nx )
     !
     IMPLICIT NONE
     INTEGER, INTENT(OUT) :: i2g  !  global index of the first local element
     INTEGER, INTENT(OUT) :: nl   !  local number of elements
     INTEGER, INTENT(IN)  :: nx   !  dimension of the global array to be distributed
     !
     INTEGER, EXTERNAL :: ldim_block, gind_block
     !
     nl  = ldim_block( nx, nbgrp, my_bgrp_id )
     i2g = gind_block( 1, nx, nbgrp, my_bgrp_id )
     !
     RETURN
     !
  END SUBROUTINE distribute_over_bgrp
  !
  SUBROUTINE init_index_over_band(comm,nbnd)
    IMPLICIT NONE
#if defined (__MPI)
    !
    include 'mpif.h'
    !
#endif
    INTEGER, INTENT(IN) :: comm, nbnd

    INTEGER :: npe, myrank, ierror, rest, k

    myrank = mp_rank(comm)
    npe = mp_size(comm)

!    call mpi_comm_rank(comm, mp_rank, ierror)
!    call mpi_comm_size(comm, mp_size, ierror)
    rest = mod(nbnd, npe)
    k = int(nbnd/npe)

    if(k.ge.1)then
     if(rest > myrank)then
       ibnd_start = (myrank)*k + (myrank+1)
       ibnd_end  =  (myrank+1)*k + (myrank+1)
     else
       ibnd_start = (myrank)*k + rest + 1
       ibnd_end  =  (myrank+1)*k + rest
     endif
    else
     ibnd_start = 1
     ibnd_end = nbnd
    endif


  END SUBROUTINE init_index_over_band
  !
  !
  FUNCTION get_ntask_groups()
     IMPLICIT NONE
     INTEGER :: get_ntask_groups
     get_ntask_groups = ntask_groups
     RETURN
  END FUNCTION get_ntask_groups
  !
END MODULE mp_global
