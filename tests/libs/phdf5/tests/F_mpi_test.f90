!
! This example creates HDF5 file in a parallel environment
!

     PROGRAM FILE_CREATE

     USE HDF5 ! This module contains all necessary modules 
        
     IMPLICIT NONE

     INCLUDE 'mpif.h'
     CHARACTER(LEN=256) :: filename

     INTEGER(HID_T) :: file_id       ! File identifier 
     INTEGER(HID_T) :: plist_id      ! Property list identifier 
     INTEGER        :: error

     !
     ! MPI definitions and calls.
     !
     INTEGER :: mpierror       ! MPI error flag
     INTEGER :: comm, info
     INTEGER :: mpi_size, mpi_rank
     comm = MPI_COMM_WORLD
     info = MPI_INFO_NULL

     CALL getarg(1, filename)
     CALL MPI_INIT(mpierror)
     CALL MPI_COMM_SIZE(comm, mpi_size, mpierror)
     CALL MPI_COMM_RANK(comm, mpi_rank, mpierror) 
     !
     ! Initialize FORTRAN predefined datatypes
     !
     CALL h5open_f(error) 

     ! 
     ! Setup file access property list with parallel I/O access.
     !
     CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
     CALL h5pset_fapl_mpio_f(plist_id, comm, info, error)

     !
     ! Create the file collectively.
     ! 
     CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error, access_prp = plist_id)

     !
     ! Close property list and the file.
     !
     CALL h5pclose_f(plist_id, error)
     CALL h5fclose_f(file_id, error)

     !
     ! Close FORTRAN interface
     !
     CALL h5close_f(error)

     CALL MPI_FINALIZE(mpierror)

     END PROGRAM FILE_CREATE
