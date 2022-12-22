program helloBPReaderHeatMap3D
#if ADIOS2_USE_MPI
    use mpi
#endif
    use adios2

    implicit none

    type(adios2_adios) :: adios
    type(adios2_io) :: ioPut, ioGet
    type(adios2_variable) :: var_temperatures, var_temperaturesIn
    type(adios2_engine) :: bpWriter, bpReader

    integer, dimension(:,:,:), allocatable :: temperatures, sel_temperatures
    integer(kind=8), dimension(3) :: ishape, istart, icount
    integer(kind=8), dimension(3) :: sel_start, sel_count
    integer(kind=8) :: iglobal, value
    integer(kind=8) :: i, j, k
    integer(kind=4) :: ierr, irank, isize, inx, iny, inz

#if ADIOS2_USE_MPI
    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, irank, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, isize, ierr)
#else
    irank = 0
    isize = 1
#endif

    inx = 10
    iny = 10
    inz = 10

    icount = (/         inx,   iny, inz  /)
    istart = (/ irank * inx,     0,   0  /)
    ishape = (/ isize * inx,   iny, inz  /)

    allocate( temperatures( inx, iny, inz ) )
    ! populate temperature
    do k=1, icount(3)
      do j=1, icount(2)
        do i=1, icount(1)
            iglobal = istart(1) + i
            value = (k-1) * ishape(1) * ishape(2) + (j-1) * ishape(1) + &
                    &  (iglobal-1)
            temperatures(i,j,k) = INT(value)
        end do
      end do
    end do

    ! Start adios2 Writer
#if ADIOS2_USE_MPI
    call adios2_init( adios, MPI_COMM_WORLD, ierr )
#else
    call adios2_init( adios, ierr)
#endif
    call adios2_declare_io( ioPut, adios, 'HeatMapWrite', ierr )

    call adios2_define_variable( var_temperatures, ioPut, 'temperatures', &
                                 adios2_type_integer4, 3, &
                                 ishape, istart, icount, adios2_constant_dims, &
                                  ierr )

    call adios2_open( bpWriter, ioPut, 'HeatMap3D_f.bp', adios2_mode_write, &
                      ierr )

    call adios2_put( bpWriter, var_temperatures, temperatures, ierr )

    call adios2_close( bpWriter, ierr )


    if( allocated(temperatures) ) deallocate(temperatures)

    ! Start adios2 Reader in rank 0
    if( irank == 0 ) then

        call adios2_declare_io( ioGet, adios, 'HeatMapRead', ierr )

#if ADIOS2_USE_MPI
        call adios2_open( bpReader, ioGet, 'HeatMap3D_f.bp', &
                          adios2_mode_read, MPI_COMM_SELF, ierr)
#else
        call adios2_open( bpReader, ioGet, 'HeatMap3D_f.bp', &
                          adios2_mode_read, ierr)
#endif

        call adios2_inquire_variable( var_temperaturesIn, ioGet, &
                                      'temperatures', ierr )

        if( ierr == adios2_found ) then

            sel_start = (/ 0, 0, 0 /)
            sel_count = (/ 4, 4, 4 /)
            allocate( sel_temperatures( sel_count(1), sel_count(2), &
                                        sel_count(3) ) )

            call adios2_set_selection( var_temperaturesIn, 3, sel_start, &
                                       sel_count, ierr )

            call adios2_get( bpReader, var_temperaturesIn, &
                             sel_temperatures, adios2_mode_sync, ierr )


            write(*,'(A,3(I1,A),A,3(I1,A),A)') 'Temperature map selection  &
                      & [ start = (', (sel_start(i),',',i=1,3) , ') &
                      &  count =  (', (sel_count(i),',',i=1,3) , ') ]'


            do k=1,sel_count(3)
              do j=1,sel_count(2)
                do i=1,sel_count(1)
                    write(6,'(I5) ', advance="no") sel_temperatures(i,j,k)
                end do
                write(*,*)
              end do
            end do


            if( allocated(sel_temperatures) ) deallocate(sel_temperatures)

        end if

        call adios2_close( bpReader, ierr )

    end if


    call adios2_finalize(adios, ierr)
#if ADIOS2_USE_MPI
    call MPI_Finalize(ierr)
#endif

end program helloBPReaderHeatMap3D
