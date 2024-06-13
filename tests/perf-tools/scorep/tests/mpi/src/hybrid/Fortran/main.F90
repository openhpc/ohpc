      program main

      include 'mpif.h'
      double precision  PI25DT
      parameter        (PI25DT = 3.141592653589793238462643d0)
      double precision  mypi, pi, h, sum, x, f, a
      integer num_iters, rank, size, i, rc

!     Function to integrate
      f(a) = 4.d0 / (1.d0 + a * a)

!     Normal MPI startup
      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
      print *, "Process ", rank, " of ", size, " is alive"

!     Loop until finished
      num_iters = 1000
      do iter = 2, num_iters
!     Calculate the interval size
         h = 1.0d0 / iter
         sum  = 0.0d0
         !$omp parallel do private(x) reduction(+:sum)
         do i = rank + 1, iter, size
            x = h * (dble(i) - 0.5d0)
            sum = sum + f(x)
         enddo
         mypi = h * sum
!     Collect all the partial sums
         call MPI_REDUCE(mypi, pi, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
      enddo

!     All finished
      if (rank .eq. 0) then
         write(6, '(F20.16, F20.16)') pi, abs(pi - PI25DT)
      endif

      call MPI_FINALIZE(rc)
      stop

      end
