! Copyright (c) 2015, Intel Corporation
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! * Redistributions of source code must retain the above copyright notice,
!   this list of conditions and the following disclaimer.
! * Redistributions in binary form must reproduce the above copyright
!   notice, this list of conditions and the following disclaimer in the
!   documentation and/or other materials provided with the distribution.
! * Neither the name of Intel Corporation nor the names of its contributors
!   may be used to endorse or promote products derived from this software
!   without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

program main
  implicit none

  integer N
  parameter (N=1024)

  real*8 a(N), b(N), c, sum
  integer nthreads, nt, i
  integer omp_get_num_threads

  character (len=32) argv
  integer argc

  argc = command_argument_count()
  if (argc > 0) then
     call get_command_argument(1,argv)
     read (argv, *) nthreads
  else
     nthreads = 2
  endif
     

  print *, "# threads: ", nthreads
  call omp_set_num_threads(nthreads)

  !$omp parallel
  !$omp master
  nt = omp_get_num_threads()
  !$omp end master
  !$omp end parallel

  if (nt /= nthreads) then
    print *, "ERROR: ", nt, " thread(s) detected != ", nthreads
    call exit(-1)
  endif

  sum = 0.0
  do i=1,N
    sum = sum + i * i * 2.0
  enddo

  !$omp parallel do private(i)
  do i=1,N
    a(i) = 1.0 * i
    b(i) = 2.0 * i
  enddo
  !omp end parallel do nowait

  c = 0.0
  !$omp parallel do reduction(+:c)
  do i=1,N
    c = c + a(i) * b(i)
  enddo

  if (c /= sum) then
    print *, "ERROR: computed sum ", c, " != ", sum
    call exit(-1)
  endif

  print *, "final sum: ", c

end program main
