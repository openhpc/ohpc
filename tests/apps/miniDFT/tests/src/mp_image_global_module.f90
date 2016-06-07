!
! Copyright (C) 2002-2009 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
MODULE mp_image_global_module
  !----------------------------------------------------------------------------
  !
  USE mp, ONLY : mp_comm_free, mp_size, mp_rank, mp_sum, mp_barrier, &
       mp_bcast, mp_start, mp_end
  USE io_global, ONLY : stdout, io_global_start, io_global_getmeta
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
  ! ... number of processors written in the data file for checkin purposes:
  INTEGER :: nproc_file = 1        ! world group
  INTEGER :: nproc_image_file = 1  ! in an image
  !
  PRIVATE :: mp_images_init
  !
CONTAINS
  !
  !-----------------------------------------------------------------------
  SUBROUTINE mp_image_startup (root,comm) 
    !-----------------------------------------------------------------------
    ! ... This subroutine initializes MPI
    ! ... Processes are organized in NIMAGE images each dealing with a subset of
    ! ... images used to discretize the "path" (only in "path" optimizations)
    !
    IMPLICIT NONE
    !
    INTEGER, INTENT(IN) :: comm
    INTEGER, INTENT(IN) :: root
    !
    INTEGER :: myrank, npe
    !
    ! ... now initialize processors and groups variables
    ! ... set global coordinate for this processor
    ! ... root  = index of the root processor
    !
    myrank = mp_rank(comm)
    npe = mp_size(comm)
    !
    ! ... mp_global_path_start set default values for 
    ! mpime, root, nproc, world_comm, nproc_image, my_image_id,
    ! root_image ...
    !
    CALL mp_image_global_start( root, myrank, comm, npe )
    !
    ! ... initialize input/output, set (and get) the I/O nodes
    ! ... sets meta_ionode and ionode_id (==root)
    !
    CALL io_global_getmeta ( myrank, root )
    !
    IF ( myrank==root ) THEN
       !
       ! ... How many parallel images ?
       !
       CALL get_arg_nimage( nimage )
       !
       nimage = MAX( nimage, 1 )
       nimage = MIN( nimage, npe )
       !
    END IF
    !
    CALL mp_barrier(comm)
    !
    ! ... broadcast input parallelization options to all processors
    !
    CALL mp_bcast( nimage, root, comm )
    !
    ! ... initialize images, band, k-point, ortho groups in sequence
    !
    CALL mp_images_init( )
    !
    !
    RETURN
    !
  END SUBROUTINE mp_image_startup
  !
  !-----------------------------------------------------------------------
  SUBROUTINE mp_image_global_start( root_i, mpime_i, group_i, nproc_i )
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
    nproc_image      = nproc_i
    my_image_id      = 0
    me_image         = mpime
    root_image       = root
    inter_image_comm = group_i
    intra_image_comm = group_i
    !
    RETURN
    !
  END SUBROUTINE mp_image_global_start
  !
  !-----------------------------------------------------------------------
  !----------------------------------------------------------------------------
  SUBROUTINE mp_images_init ()
    !---------------------------------------------------------------------------
    !
    ! ... This routine divides all MPI processors into images
    !
    IMPLICIT NONE
    INTEGER :: ierr = 0
    !
#if defined (__MPI)
    !
    IF ( nimage < 1 .OR. nimage > nproc ) &
       CALL errore( 'init_images', 'invalid number of images, out of range', 1 )
    IF ( MOD( nproc, nimage ) /= 0 ) &
       CALL errore( 'init_images', 'n. of images must be divisor of nprocs', 1 )
    ! 
    ! ... set number of cpus per image
    !
    nproc_image = nproc / nimage
    !
    ! ... set index of image for this processor   ( 0 : nimage - 1 )
    !
    my_image_id = mpime / nproc_image
    !
    ! ... set index of processor within the image ( 0 : nproc_image - 1 )
    !
    me_image    = MOD( mpime, nproc_image )
    !
    CALL mp_barrier(world_comm)
    !
    ! ... the intra_image_comm communicator is created
    !
    CALL MPI_COMM_SPLIT( world_comm, my_image_id, mpime, intra_image_comm, ierr )
    IF ( ierr /= 0 ) CALL errore &
       ( 'init_images', 'intra image communicator initialization', ABS(ierr) )
    !
    CALL mp_barrier(world_comm)
    !
    ! ... the inter_image_comm communicator is created                     
    !     
    CALL MPI_COMM_SPLIT( world_comm, me_image, mpime, inter_image_comm, ierr )  
    IF ( ierr /= 0 ) CALL errore &
       ( 'init_images', 'inter image communicator initialization', ABS(ierr) )
#endif
    RETURN
    !
  END SUBROUTINE mp_images_init
  !
END MODULE mp_image_global_module
