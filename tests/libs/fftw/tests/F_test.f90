!http://www.nersc.gov/users/software/programming-libraries/math-libraries/fftw/fftw3-serial-1d-example-fortran-source/
implicit none
      include "fftw3.f"

      integer n
      parameter (n=16)

      integer*8 plan, iplan

      double complex in, out
      dimension in(N), out(N)

      integer i

      print *,"n = ",n
      print *,"input = "
      do i=1,n
         in(i) = (1.0, 0.0)
         print *,in(i)
      enddo

      print *,"fftw_fwd =      ",fftw_forward
      print *,"fftw_bkwd =     ",fftw_backward
      print *,"fftw_estimate = ",fftw_estimate

      call dfftw_plan_dft_1d(plan, n,in,out,fftw_forward, FFTW_ESTIMATE)
      call dfftw_plan_dft_1d(iplan,n,out,in,fftw_backward,FFTW_ESTIMATE)

      print *,"address of fwd plan =   ",plan
      print *,"address of bckwd plan = ",iplan

      call dfftw_execute(plan)

      print *, 'output after forward fft:'
      do i=1,n
         print *, out(i)
      enddo

      call dfftw_execute(iplan)

      print *, 'after inverse fft:'
      do i=1,n
         print *, in(i)
      enddo

      call dfftw_destroy_plan(plan)
      call dfftw_destroy_plan(iplan)

      end
