C  Ping-pong with MPI + I/O

      include 'mpif.h'

      integer BUFFSIZE, MSGSIZE, error, rank, size, dest, i
      integer status(MPI_STATUS_SIZE), NITERS, retsize
      parameter (BUFFSIZE=100000, MSGSIZE=10000, NITERS=1000)
      integer*1 buff, foo, sndmsg, rcvmsg
      dimension buff(BUFFSIZE)
      dimension foo(1)
      dimension sndmsg(MSGSIZE)
      dimension rcvmsg(MSGSIZE)
      real*8 r

      call MPI_Init(error)
c      call MPI_Pcontrol(0)
      if (error .ne. MPI_SUCCESS) stop
      call MPI_Comm_rank(MPI_COMM_WORLD, rank, error)
      call MPI_Comm_size(MPI_COMM_WORLD, size, error)

      if (MOD(size,2) .ne. 0) then
         print *, 'Wrong number of processes, must be multiple of 2'
         call MPI_Finalize(error)
         stop
      end if

      call MPI_Buffer_attach(buff, buffsize, error)

      if (MOD(rank,2) .eq. 0) then

         do i=1, NITERS
            call MakeIO(IOSIZE, rank)
            call WasteCPU(IOSIZE, rank, r)
            call MPI_Bsend(r, 1, MPI_REAL8, rank+1, rank,
     1           MPI_COMM_WORLD, error)
            call MPI_Bsend(sndmsg, MSGSIZE, MPI_INTEGER1, rank+1, rank,
     1           MPI_COMM_WORLD, error)
            call MakeIO(IOSIZE, rank)
            call MPI_Recv(rcvmsg, MSGSIZE, MPI_INTEGER1, MPI_ANY_SOURCE,
     1           MPI_ANY_TAG, MPI_COMM_WORLD, status, error)
            call MPI_Barrier(MPI_COMM_WORLD, error)
         end do

      else

         do i=1, NITERS
            call MakeIO(IOSIZE, rank)
            call MPI_Recv(r, 1, MPI_REAL8, MPI_ANY_SOURCE, MPI_ANY_TAG,
     1           MPI_COMM_WORLD, status, error)
            call MPI_Recv(rcvmsg, MSGSIZE, MPI_INTEGER1, MPI_ANY_SOURCE,
     1           MPI_ANY_TAG, MPI_COMM_WORLD, status, error)
            call MakeIO(IOSIZE, rank)
            call WasteCPU(IOSIZE, rank, r)
            call MPI_Bsend(sndmsg, MSGSIZE, MPI_INTEGER1, rank-1, rank,
     1           MPI_COMM_WORLD, error)
            call MPI_Barrier(MPI_COMM_WORLD, error)
         end do

      end if

      call MPI_Buffer_detach(foo, retsize, error)

      call MPI_Finalize(error)
      stop
      end

C   This function performs some I/O
      subroutine MakeIO(size, rank)
         integer size, rank, BUFFERSIZE
         integer*1 buffer
         parameter (BUFFERSIZE=65536)
         dimension buffer(BUFFERSIZE)

         open (10, form='UNFORMATTED', access='SEQUENTIAL',
     1        file='pingtmp'//char(ichar('0') + rank))
         write (10) buffer
         close (10)
         return
      end

C   This function wastes some CPU
C   This is useful to play with bursts mode
      subroutine WasteCPU(size, rank, r)
         integer :: size, rank
         real*8 :: r

         integer :: n, i
         real*8 :: h, tmp

         tmp = 0
         n = 500*1000
         do i=1, n
             x = h * (i - 0.5)
             tmp = tmp + (4.0 / (1.0 + x*x))
         end do
         r = tmp

         return
      end
