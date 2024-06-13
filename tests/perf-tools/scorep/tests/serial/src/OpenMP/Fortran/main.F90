program main
  implicit none
  integer, parameter :: N = 1000
  real :: a = 2.0
  real :: x(N), y(N), res(N)

  call init(x, N)
  call init(y, N)

  call saxpy(res, a, x, y, N)

contains

  subroutine init(arr, n)
    real, dimension(:), intent(out) :: arr
    integer, intent(in) :: n
    integer :: i

    do i = 1, n
      arr(i) = real(i - 1)
    end do
  end subroutine init

  subroutine saxpy(res, a, x, y, n)
    real, dimension(:), intent(out) :: res
    real, intent(in) :: a
    real, dimension(:), intent(in) :: x, y
    integer, intent(in) :: n
    integer :: i

    !$omp parallel do
    do i = 1, n
      res(i) = a * x(i) + y(i)
    end do
    !$omp end parallel do
  end subroutine saxpy

end program main
