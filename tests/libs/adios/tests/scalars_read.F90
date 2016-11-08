!  
!  ADIOS is freely available under the terms of the BSD license described
!  in the COPYING file in the top level directory of this source distribution.
!
!  Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
!

!/*************************************************************/
!/*   Example of reading various types of variable in ADIOS   */
!/*************************************************************/
program scalars_read
    use adios_read_mod
    implicit none
    include 'mpif.h'

    character(len=25)   :: filename = "scalars.bp"
    integer             :: rank, size, i, ierr
    integer             :: comm

    ! ADIOS variables declarations 
    integer*8               :: f
    integer                 :: method = ADIOS_READ_METHOD_BP
    integer*8               :: sel

    ! scalar variables to write out (including a string)
    integer*1           :: v1 = 0
    integer*2           :: v2 = 0
    integer*4           :: v3 = 0
    integer*8           :: v4 = 0

    integer*1           :: v5 = 0
    integer*2           :: v6 = 0
    integer*4           :: v7 = 0
    integer*8           :: v8 = 0

    real*4              :: v9 = 0.0
    real*8              :: v10 = 0.0

    character(len=20)   :: v11 = "undefined"

    complex*8           :: v12 = (0.0, 0.0)
    complex*16          :: v13 = (0.0, 0.0)

    call MPI_Init (ierr)
    call MPI_Comm_dup (MPI_COMM_WORLD, comm, ierr)
    call MPI_Comm_rank (comm, rank, ierr)
    call MPI_Comm_size (comm, size, ierr);

    call adios_read_init_method (method, comm, "verbose=3", ierr);

    call adios_read_open (f, filename, method, comm, ADIOS_LOCKMODE_NONE, 1.0, ierr);

    sel = 0  ! sel must be integer*8

    ! option 1 for scalars: get it from the metadata read at open
    call adios_get_scalar (f, "var_byte", v1, ierr)
    call adios_get_scalar (f, "var_short", v2, ierr)
    call adios_get_scalar (f, "var_int", v3, ierr)
    call adios_get_scalar (f, "var_long", v4, ierr)
    ! the above variables contain the value at this point

    ! option 2 for scalars: read them from file
    call adios_schedule_read (f, sel, "var_ubyte", 0, 1, v5, ierr)
    call adios_schedule_read (f, sel, "var_ushort", 0, 1, v6, ierr)
    call adios_schedule_read (f, sel, "var_uint", 0, 1, v7, ierr)
    call adios_schedule_read (f, sel, "var_ulong", 0, 1, v8, ierr)
    call adios_schedule_read (f, sel, "var_real", 0, 1, v9, ierr)
    call adios_schedule_read (f, sel, "var_double", 0, 1, v10, ierr)
    call adios_schedule_read (f, sel, "var_string", 0, 1, v11, ierr)
    call adios_schedule_read (f, sel, "var_complex", 0, 1, v12, ierr)
    call adios_schedule_read (f, sel, "var_double_complex", 0, 1, v13, ierr)
    ! no read has been performed yet!
    call adios_perform_reads (f, ierr)
    ! the above variables contain the value only at this point

    if (rank == 0) then
        write (*, '("int*1      v1  = ",i3)') v1
        write (*, '("int*2      v2  = ",i3)') v2
        write (*, '("int*4      v3  = ",i3)') v3
        write (*, '("int*8      v4  = ",i3)') v4

        write (*, '("int*1      v5  = ",i3)') v5
        write (*, '("int*2      v6  = ",i3)') v6
        write (*, '("int*4      v7  = ",i3)') v7
        write (*, '("int*8      v8  = ",i3)') v8

        write (*, '("real*4     v9  = ",f6.2)') v9
        write (*, '("real*8     v10 = ",f6.2)') v10

        write (*, '("string     v11 = ",a)') trim(v11)

        write (*, '("complex*8  v12 = (",f6.2,", ", f6.2,")")') v12
        write (*, '("complex*16 v13 = (",f6.2,", ", f6.2,")")') v13

        ! quick sanity check
        if(v4 /= -1) then
           call MPI_Abort (ierr)
        endif

    endif

    call adios_read_close (f, ierr)
    call MPI_Barrier (comm, ierr)
    call adios_read_finalize_method (method, ierr)
    call MPI_Finalize (ierr)

end program

