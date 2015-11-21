program hello
    include 'mpif.h'
    integer rank, size, ierror, tag, status(MPI_STATUS_SIZE)
    integer :: local = 1
    integer :: global

    call MPI_INIT(ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)

    ! Verify a quick collective

    call MPI_ALLREDUCE(local,global,1,MPI_INTEGER,MPI_SUM,MPI_COMM_WORLD,ierr)

    if(global .ne. size)then
       call exit(1)
    endif

    call MPI_FINALIZE(ierror)
end
