program helloBPWriter
#if ADIOS2_USE_MPI
    use mpi
#endif
    use adios2

    implicit none

    integer(kind=8), dimension(1) :: shape_dims, start_dims, count_dims
    real, dimension(:), allocatable :: myArray
    integer :: inx, irank, isize, ierr, i
    type(adios2_adios) :: adios
    type(adios2_io) :: io
    type(adios2_variable) :: var1
    type(adios2_engine) :: engine1
    character(len=:), allocatable :: var1_name

#if ADIOS2_USE_MPI
    ! Launch MPI
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, irank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, isize, ierr)
#else
    irank = 0
    isize = 1
#endif

    ! Application variables
    inx = 10
    allocate( myArray(inx) )

    do i=1,inx
        myArray(i) = i-1
    end do

    ! Variable dimensions
    shape_dims(1) = isize * inx
    start_dims(1) = irank * inx
    count_dims(1) = inx

    ! Create adios handler passing the communicator and error flag
#if ADIOS2_USE_MPI
    call adios2_init(adios, MPI_COMM_WORLD, ierr)
#else
    call adios2_init(adios, ierr)
#endif

    ! Declare an IO process configuration inside adios
    call adios2_declare_io(io, adios, "ioWriter", ierr)

    ! Defines a variable to be written in bp format
    call adios2_define_variable(var1, io, "myArray", adios2_type_real, 1, &
                                shape_dims, start_dims, count_dims, &
                                adios2_constant_dims, ierr)

    ! Open myVector_f.bp in write mode, this launches an engine
    call adios2_open(engine1, io, "myVector_f.bp", adios2_mode_write, ierr)

    ! Put myArray contents to bp buffer, based on var1 metadata
    call adios2_put(engine1, var1, myArray, ierr)

    ! Closes engine1 and deallocates it, becomes unreachable
    call adios2_close(engine1, ierr)

    ! Deallocates adios and calls its destructor
    call adios2_finalize(adios, ierr)

    if( allocated(myArray) ) deallocate(myArray)
    if( allocated(var1_name) ) deallocate(var1_name)

#if ADIOS2_USE_MPI
    call MPI_Finalize(ierr)
#endif

end program helloBPWriter
