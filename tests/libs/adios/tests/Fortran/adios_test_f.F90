!  
!  ADIOS is freely available under the terms of the BSD license described
!  in the COPYING file in the top level directory of this source distribution.
!
!  Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
!

program adios_test
    use adios_write_mod
    implicit none
    include 'mpif.h'
    character (len=200) :: group
    character (len=200) :: filename
    integer :: group_comm
    integer :: ierr
    integer :: rank

    ! write vars
    integer :: small_int
    integer*8 :: big_int
    real :: small_real
    real*8 :: big_real
    integer :: z_size
    real :: z_array (2)

    ! read vars
    integer :: r_small_int
    integer*8 :: r_big_int
    real :: r_small_real
    real*8 :: r_big_real
    integer :: r_z_size
    real :: r_z_array (2)

    group = "restart"
    filename = "restart.bp"
    small_int = 10
    big_int = 42949672
    small_real = 0.3
    big_real = 0.0000000000004
    z_size = 2
    z_array (1) = 11.1
    z_array (2) = 22.2
    r_z_size = 2

    call MPI_Init (ierr)
    call MPI_Comm_dup (MPI_COMM_WORLD, group_comm, ierr)
    call MPI_Comm_rank (MPI_COMM_WORLD, rank, ierr)

    print '("rank=",i0," group_comm=",i0," ierr=",i0)', rank, group_comm, ierr

    call adios_allocate_buffer (10, ierr);
    call adios_init ("config_fortran.xml", group_comm, ierr)
    call test_write (group, filename, group_comm, small_int, big_int, small_real, big_real, z_size, z_array)

    call MPI_Barrier (MPI_COMM_WORLD, ierr)
write (*,*) "write completed"

    call test_read (group, filename, group_comm, r_small_int, r_big_int, r_small_real, r_big_real, r_z_size, r_z_array)

    if (small_int /= r_small_int .or. big_int /= r_big_int .or. &
     & small_real /= r_small_real .or. big_real /= r_big_real .or.&
      z_size /= r_z_size) then
        write (*,*) 'rank ', rank, ' read did not match write'
    else
        write (*,*) 'rank ', rank, ' read matched write'
    endif

    call MPI_Barrier (MPI_COMM_WORLD, ierr)

    call adios_finalize (rank, ierr)

    call MPI_Finalize (ierr)
end program adios_test

subroutine test_write (group, filename, group_comm, small_int, big_int, small_real, big_real, a_size, a_array)
    implicit none
    include 'mpif.h'
    character (*), intent(in) :: group
    character (*), intent(in) :: filename
    integer, intent (in) :: group_comm
    integer, intent(in) :: small_int
    integer*8, intent(in) :: big_int
    real, intent(in) :: small_real
    real*8, intent(in) :: big_real
    integer, intent(in) :: a_size
    real, intent(in) :: a_array (a_size)

    integer :: a_size2
    real :: a_array2 (a_size, 10)

    integer :: istep1
    integer :: istep2
    integer :: istep3

    character(len=5) :: abcstr = 'abc'

    integer*8 :: handle
    integer*8 :: total_size
    integer :: err
    integer*8 :: size
    integer :: i,j

    a_size2 = 10

    size = 900 * 1024

    istep1 = 11
    istep2 = 22
    istep3 = 33
    do j=1,10
       do i=1,a_size
          a_array2(i,j) = 1.0*i + (j-1)*a_size
       end do
    end do
    size = 4 + 8 + 4 + 8 + 4 + 4 + a_size * 4 + a_size * 10 * 4 + 4 + 4 + 4

    call adios_open (handle, group, filename, "w", group_comm, err)

    call adios_group_size (handle, size, total_size, err)

    call adios_write (handle, "small_int", small_int, err)
    call adios_write (handle, "big_int", big_int, err)
    call adios_write (handle, "small_real", small_real, err)
    call adios_write (handle, "big_real", big_real, err)
    call adios_write (handle, "ze0size", a_size, err)
    call adios_write (handle, "ze1size", a_size2, err)
    call adios_write (handle, "zelectron0", a_array, err)
    call adios_write (handle, "zelectron1", a_array2, err)

    call adios_write (handle, "istep1", istep1, err)
    call adios_write (handle, "istep2", istep2, err)
    call adios_write (handle, "istep3", istep3, err)

    ! When writing out a string type variable, we need to add a char(0) 
    ! ADIOS expects here a C string
    !call adios_write (handle, "str", trim("abc")//char(0), err)
    call adios_write (handle, "str_var", abcstr, err)
    call adios_write (handle, "str_const", "abc", err)
    call adios_write (handle, "str_arr", abcstr, err)

    call adios_close (handle, err)

end subroutine test_write

subroutine test_read (group, filename, group_comm, small_int, big_int, small_real, big_real, a_size, a_array)
    implicit none
    include 'mpif.h'
    character (*), intent(in) :: group
    character (*), intent(in) :: filename
    integer, intent (in) :: group_comm
    integer, intent(out) :: small_int
    integer*8, intent(out) :: big_int
    real, intent(out) :: small_real
    real*8, intent(out) :: big_real
    integer, intent(inout) :: a_size
    real, intent(out) :: a_array (a_size)

    integer*8 :: group_size = 0
    integer*8 :: total_size
    integer :: err

    integer :: istep1
    integer :: istep2
    integer :: istep3

    integer*8 :: handle
    integer*8 :: buffer_size

    character (len=40) :: str

    str = '1234567890123456789012345678901234567890'

    istep1 = 11
    istep2 = 22
    istep3 = 33

    call adios_open (handle, group, filename, "r", group_comm, err)

    call adios_group_size (handle, group_size, total_size, err)
    buffer_size = 4
    call adios_read (handle, "small_int", small_int, buffer_size, err)
    buffer_size = 8
    call adios_read (handle, "big_int", big_int, buffer_size, err)
    buffer_size = 4
    call adios_read (handle, "small_real", small_real, buffer_size, err)
    buffer_size = 8
    call adios_read (handle, "big_real", big_real, buffer_size, err)
    buffer_size = 4
    call adios_read (handle, "ze0size", a_size, buffer_size, err)
    buffer_size = 4 * a_size
    call adios_read (handle, "zelectron0", a_array, buffer_size, err)

    !call adios_read (handle, "str", str, err)

    call adios_close (handle, err)

    !write (*,*) "read str: ", str

end subroutine test_read
