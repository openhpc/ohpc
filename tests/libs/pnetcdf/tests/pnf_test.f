!
!  Copyright (C) 2003, Northwestern University and Argonne National Laboratory
!  See COPYRIGHT notice in top-level directory.
!
!  $Id$
!
!=============================================================================
!
! CODE DEVELOPER
!   John Tannahill, LLNL
!   jrt@llnl.gov
!   Note that this code was adapted from csnap.c, which was written by:
!     Woo-Sun Yang and Chris Ding
!     NERSC, Lawrence Berkeley National Laboratory
!
! FILE
!   pnf_test.F
!
! ROUTINES
!   Pnf_Test (Program)
!
! This code writes the array, tt(k)(j)(i), into the file 'pnf_test.nc'. It
! then reads the array from the file, and compares it with the original
! values.
!
! i=longitude, j=latitude, k=level
!
!=============================================================================

      program Pnf_Test

      implicit none

      include "mpif.h"
      include "pnetcdf.inc"


!     -----------------------
!     Parameter declarations.
!     -----------------------

      integer NREADS, NWRITES
      parameter (NREADS = 5, NWRITES = 5 )
      ! number of read samples
      ! number of write samples

      integer*8 TOTSIZ_3D(3) ! global sizes of 3D field


!     ----------------------
!     Variable declarations.
!     ----------------------

      logical reorder

      logical isperiodic(3)

      integer comm_cart                   ! Cartesian communicator
      integer ierr
      integer*8 istart, jstart, kstart ! offsets of 3D field
      integer*8 locsiz
      integer mype                        ! rank in comm_cart
      integer totpes                      ! total number of PEs

      integer*8 locsiz_3d(3) ! local sizes of 3D fields
      integer pe_coords(3)                ! Cartesian PE coords

      integer numpes(3)                      ! number of PEs along axes;
                                             !   determined by MPI where a
                                             !   zero is specified

      double precision  filsiz

      real*4  rdt_g(2)
      real*4  rdt_l(2)
      real*4  wrt_g(2)
      real*4  wrt_l(2)

      real*4  rrates_g(2)
      real*4  rrates_l(2)
      real*4  wrates_g(2)
      real*4  wrates_l(2)


      data reorder / .false. /
      data isperiodic / .false., .false., .false. /
      data numpes / 1, 1, 0 /
      data TOTSIZ_3D / 256, 256, 256 /

!     ----------------
!     Begin execution.
!     ----------------

      call MPI_Init (ierr)

      call MPI_Comm_Size (MPI_COMM_WORLD, totpes, ierr)

      call MPI_Dims_Create (totpes, 3, numpes, ierr)

      call MPI_Cart_Create
     &  (MPI_COMM_WORLD, 3, numpes, isperiodic, reorder,
     &   comm_cart, ierr)

      call MPI_Comm_Rank (comm_cart, mype, ierr)

      call MPI_Cart_Coords (comm_cart, mype, 3, pe_coords, ierr)


      rdt_l(1) = 1.0e38
      rdt_l(2) = 1.0e38
      wrt_l(1) = 1.0e38
      wrt_l(2) = 1.0e38
!      rdt_l(:) = Huge (rdt_l)   ! initialize for timing
!      wrt_l(:) = Huge (wrt_l)


!     ----------------------------------------
!     Determine local size for tt (locsiz_3d).
!     ----------------------------------------

!     ===============
      call Find_Locnx
     &  (TOTSIZ_3D(1), pe_coords(1), numpes(1), locsiz_3d(1), istart)
      call Find_Locnx
     &  (TOTSIZ_3D(2), pe_coords(2), numpes(2), locsiz_3d(2), jstart)
      call Find_Locnx
     &  (TOTSIZ_3D(3), pe_coords(3), numpes(3), locsiz_3d(3), kstart)
!     ===============


!     -------------------------------
!     Compute file size in 1d6 bytes.
!     -------------------------------

      filsiz = (TOTSIZ_3D(1) * TOTSIZ_3D(2) * TOTSIZ_3D(3)) *
     &         1.0d-6 * 4.0d0


!     -------------------------------------
!     Print data decomposition information.
!     -------------------------------------

      if (mype .EQ. 0) Write (6,900)

      call MPI_Barrier (comm_cart, ierr)

      Write (6, 902)
     &  mype, pe_coords(1), pe_coords(2), pe_coords(3),
     &  TOTSIZ_3D(1), TOTSIZ_3D(2), TOTSIZ_3D(3),
     &  locsiz_3d(1), locsiz_3d(2), locsiz_3d(3),
     &  kstart, jstart, istart

 900  format ("mype  pe_coords    totsiz_3d         locsiz_3d       ",
     &        "kstart,jstart,istart")
 902  format (i3,3x,i2,1x,i2,1x,i2,2x,i4,1x,i4,1x,i4,4x,i4,1x,i4,1x,i4,
     &        3x,i6,1x,i6,1x,i6)


!     -------------------------
!     Write and then read back.
!     -------------------------

      locsiz = locsiz_3d(1) * locsiz_3d(2) * locsiz_3d(3)

!     ===============
      call Write_File
!     ===============
     &  ("pnf_test.nc", NWRITES, mype, comm_cart, istart, jstart,
     &   kstart, locsiz, locsiz_3d, TOTSIZ_3D, wrt_l)
!!!   Write (6,*) wrt_l(1), wrt_l(2)

!     ==============
      call Read_File
!     ==============
     &  ("pnf_test.nc", NREADS,  mype, comm_cart, istart, jstart,
     &   kstart, locsiz, locsiz_3d, TOTSIZ_3D, rdt_l)


!     ----------------------------
!     Compute and print I/O rates.
!     ----------------------------

      wrates_l(1) = real(filsiz / wrt_l(2))               ! write rate
      wrates_l(2) = real(filsiz / (wrt_l(1) + wrt_l(2)))  ! effective write rate

      rrates_l(1) = real(filsiz / rdt_l(2))               ! read rate
      rrates_l(2) = real(filsiz / (rdt_l(1) + rdt_l(2)))  ! effective read  rate


      call MPI_Allreduce
     &  (wrates_l, wrates_g, 2, MPI_REAL, MPI_MIN, comm_cart, ierr)
      call MPI_Allreduce
     &  (rrates_l, rrates_g, 2, MPI_REAL, MPI_MIN, comm_cart, ierr)

      call MPI_Allreduce
     &  (wrt_l,    wrt_g,    2, MPI_REAL, MPI_MAX, comm_cart, ierr)
      call MPI_Allreduce
     &  (rdt_l,    rdt_g,    2, MPI_REAL, MPI_MAX, comm_cart, ierr)


      if (mype .EQ. 0) then
        Write (6,905) filsiz
        Write (6,910) wrates_g(1), wrates_g(2)
        Write (6,915) rrates_g(1), rrates_g(2)
        Write (6,920) totpes
        Write (6,922) wrt_g(1), wrt_g(2), wrates_g(2),
     &                rdt_g(1), rdt_g(2), rrates_g(2)
      end if

 905  format ("File size: ", e10.3, " MB")
 910  format ("    Write: ", f9.3, " MB/s  (eff., ", f9.3, " MB/s)")
 915  format ("    Read : ", f9.3, " MB/s  (eff., ", f9.3, " MB/s)")
 920  format ("Total number PEs: ", i4)
 922  format (e11.3, e11.3, f9.3, e11.3, e11.3, f9.3)


      call MPI_Comm_Free (comm_cart, ierr)

      call MPI_Finalize  (ierr)

      Stop

      end ! program Pnf_Test


!     ------------


      subroutine Write_File
     &  (filename, nwrites, mype, comm_cart, istart, jstart, kstart,
     &   locsiz, locsiz_3d, totsiz_3d, wrt_l)

      implicit none

      include "mpif.h"
      include "pnetcdf.inc"


!     ----------------------
!     Argument declarations.
!     ----------------------

      character*(*) filename
      integer nwrites
      integer mype
      integer comm_cart
      integer*8 istart, jstart, kstart
      integer*8 locsiz
      integer*8 locsiz_3d(3)
      integer*8 totsiz_3d(3)
      real*4  wrt_l(2)


!     ----------------------------
!     Local variable declarations.
!     ----------------------------

      integer ierr
      integer lon_id, lat_id, lev_id
      integer ncid
      integer nw
      integer tt_id

      integer*8 count_3d(3)
      integer*8 start_3d(3)

      integer dim_id(3)

      double precision  t1, t2, t3

      integer max_loc_size
      parameter( max_loc_size = 20000000 )
      real*4  tt(max_loc_size)   ! Need tt(locsiz)


      if (locsiz .gt. MAX_LOC_SIZE) then
         print *, 'locsiz = ', locsiz, ' larger than MAX_LOC_SIZE'
         stop
      endif
!     ----------------
!     Begin execution.
!     ----------------

!      start_3d(1:3) = (/ kstart, jstart, istart /)
!      count_3d(:)   = locsiz_3d(:)
      start_3d(1) = istart
      start_3d(2) = jstart
      start_3d(3) = kstart
      count_3d(1) = locsiz_3d(1)
      count_3d(2) = locsiz_3d(2)
      count_3d(3) = locsiz_3d(3)


      do nw = 1, nwrites

!       ==============
        call Get_Field
!       ==============
     &    (istart, jstart, kstart, locsiz, locsiz_3d, totsiz_3d, tt)


        call MPI_Barrier (comm_cart, ierr)
        t1 = MPI_Wtime ( )


!       =================
        ierr = Nfmpi_Create
!       =================
     &    (comm_cart, filename, NF_CLOBBER, MPI_INFO_NULL, ncid)


!       ==================
        ierr = Nfmpi_Def_Dim
     &    (ncid, "level",     totsiz_3d(1), lon_id)
        ierr = Nfmpi_Def_Dim
     &    (ncid, "latitude",  totsiz_3d(2), lat_id)
        ierr = Nfmpi_Def_Dim
     &    (ncid, "longitude", totsiz_3d(3), lev_id)
!       ==================


        dim_id(1) = lon_id
        dim_id(2) = lat_id
        dim_id(3) = lev_id

!       ==================
        ierr = Nfmpi_Def_Var
!       ==================
     &    (ncid, "tt", NF_REAL, 3, dim_id, tt_id)


!       =================
        ierr = Nfmpi_Enddef (ncid)
!       =================


        t2 = MPI_Wtime ( )


!       =============================
        ierr = Nfmpi_Put_Vara_Real_All
!       =============================
     &    (ncid, tt_id, start_3d, count_3d, tt)


!       ================
        ierr = Nfmpi_Close (ncid)
!       ================


        call MPI_Barrier (comm_cart, ierr)
        t3 = MPI_Wtime ( )


        if (t2 - t1 .LT. wrt_l(1)) wrt_l(1) = real(t2 - t1)
        if (t3 - t2 .LT. wrt_l(2)) wrt_l(2) = real(t3 - t2)

        if (mype .EQ. 0) Write (6,950) nw, t2-t1, t3-t2

      end do


 950  format ("write ", i1, ": ", e10.3, 1x, e10.3)


      Return

      end


!     ------------


      subroutine Read_File
     &  (filename, nreads, mype, comm_cart, istart, jstart, kstart,
     &   locsiz, locsiz_3d, totsiz_3d, rdt_l)

      implicit none

      include "mpif.h"
      include "pnetcdf.inc"


!     ----------------------
!     Argument declarations.
!     ----------------------

      character*(*) filename
      integer nreads
      integer mype
      integer comm_cart
      integer*8 istart, jstart, kstart
      integer*8 locsiz
      integer*8 locsiz_3d(3)
      integer*8 totsiz_3d(3)
      real*4  rdt_l(2)


!     ----------------------------
!     Local variable declarations.
!     ----------------------------

      integer ierr
      integer ncid
      integer nr
      integer tt_id
      integer ii

      integer*8 count_3d(3)
      integer*8 start_3d(3)

      double precision  t1, t2, t3

      integer max_loc_size
      parameter( max_loc_size = 20000000 )
      real*4  buf(max_loc_size)
      real*4  tt (max_loc_size)


!     ----------------
!     Begin execution.
!     ----------------

!     ==============
      call Get_Field
!     ==============
     &  (istart, jstart, kstart, locsiz, locsiz_3d, totsiz_3d, tt)


!     start_3d(1:3) = (/ kstart, jstart, istart /)
!     count_3d(:)   = locsiz_3d(:)
      start_3d(1) = istart
      start_3d(2) = jstart
      start_3d(3) = kstart
      count_3d(1) = locsiz_3d(1)
      count_3d(2) = locsiz_3d(2)
      count_3d(3) = locsiz_3d(3)


      do nr = 1, nreads

         do ii=1, int(locsiz)
            buf(ii) =  -99.99
         enddo


        call MPI_Barrier (comm_cart, ierr)
        t1 = MPI_Wtime ( )


!       ===============
        ierr = Nfmpi_Open
!       ===============
     &    (comm_cart, filename, NF_NOWRITE, MPI_INFO_NULL, ncid)


!       ====================
        ierr = Nfmpi_Inq_Varid
!       ====================
     &    (ncid, "tt", tt_id)


        t2 = MPI_Wtime ( )


!       =============================
        ierr = Nfmpi_Get_Vara_Real_All
!       =============================
     &    (ncid, tt_id, start_3d, count_3d, buf)


!       ================
        ierr = Nfmpi_Close (ncid)
!       ================


        call MPI_Barrier (comm_cart, ierr)
        t3 = MPI_Wtime ( )


        if (t2 - t1 .LT. rdt_l(1)) rdt_l(1) = real(t2 - t1)
        if (t3 - t2 .LT. rdt_l(2)) rdt_l(2) = real(t3 - t2)

        if (mype .EQ. 0) Write (6,970) nr, t2-t1, t3-t2


        if (nr .EQ. 1) then
!         ================
          call Compare_Vec
!         ================
     &      (mype, comm_cart, locsiz, tt, buf)
        end if

      end do


 970  format (" read ", i1, ": ", e10.3, 1x, e10.3)


      Return

      end


!     ------------


      subroutine Find_Locnx
     &  (nx, mype, totpes, locnx, ibegin)

      implicit none

      include "mpif.h"
      include "pnetcdf.inc"

!     ----------------------
!     Argument declarations.
!     ----------------------

      integer*8 nx
      integer mype
      integer totpes
      integer*8 locnx
      integer*8 ibegin


!     ----------------------------
!     Local variable declarations.
!     ----------------------------

      integer*8 iremain


!     ----------------
!     Begin execution.
!     ----------------

      locnx = nx / totpes

      iremain = nx - (totpes * locnx)

      if (mype .LT. iremain) locnx = locnx + 1

      ibegin = mype * (nx / totpes) + iremain + 1

      if (mype .LT. iremain) ibegin = ibegin + (mype - iremain)


      Return

      end


!     ------------


      subroutine Get_Field
     &  (istart, jstart, kstart, locsiz, locsiz_3d, totsiz_3d, tt)

      implicit none

      include "mpif.h"
      include "pnetcdf.inc"


!     ----------------------
!     Argument declarations.
!     ----------------------

      integer*8 istart, jstart, kstart
      integer*8 locsiz
      integer*8 locsiz_3d(3)
      integer*8 totsiz_3d(3)
      real*4  tt(locsiz)


!     ----------------------------
!     Local variable declarations.
!     ----------------------------

      integer ii, jj, kk
      integer ind


!     ----------------
!     Begin execution.
!     ----------------

      ind = 1


      do kk = 1, int(locsiz_3d(3))
        do jj = 1, int(locsiz_3d(2))
          do ii = 1, int(locsiz_3d(1))

             tt(ind) = real(
     &         (istart-1 +(ii - 1) + 1 + totsiz_3d(3)*(jstart-1 +
     &                 (jj - 1) + totsiz_3d(2)*(kstart-1 +
     &                 (kk-1)))) * 1.0d-3)
             ind = ind + 1

          end do
        end do
      end do


      Return

      end


!     ------------


      subroutine Compare_Vec
     &  (mype, comm_cart, locsiz, tt, buf)

      implicit none

      include "mpif.h"


!     ----------------------
!     Argument declarations.
!     ----------------------

      integer mype
      integer comm_cart
      integer*8 locsiz
      real*4  tt (locsiz)
      real*4  buf(locsiz)


!     ----------------------------
!     Local variable declarations.
!     ----------------------------

      integer ierr
      integer ii

      real*4  delmax(1), delmin(1), delta
      real*4  diff

      real*4  wr(5)
      real*4  ws(5)


!     ----------------
!     Begin execution.
!     ----------------

      ws(1) = 0.0d0      ! diff
      ws(2) = 0.0d0      ! sumsq
      ws(3) = int(locsiz)     ! locsiz
      ws(4) = 0.0d0      ! delmax(1)
      ws(5) = 1.0d38     ! Huge (ws)  ! delmin(1)


      do ii = 1, int(locsiz)
        delta = (tt(ii) - buf(ii)) * (tt(ii) - buf(ii))
        ws(1) = ws(1) + delta
        ws(2) = ws(2) + tt(ii) * tt(ii)
        if (delta .GT. ws(4)) ws(4) = delta
        if (delta .LT. ws(5)) ws(5) = delta
      end do


      call MPI_Allreduce
     &  (ws,    wr,     3, MPI_REAL, MPI_SUM, comm_cart, ierr)
      call MPI_Allreduce
     &  (ws(4), delmax(1), 1, MPI_REAL, MPI_MAX, comm_cart, ierr)
      call MPI_Allreduce
     &  (ws(5), delmin(1), 1, MPI_REAL, MPI_MIN, comm_cart, ierr)


      diff   = Sqrt (wr(1) / wr(2))         ! normalized error
      delmax(1) = Sqrt (wr(3) * delmax(1)/wr(2))  ! normalized max difference
      delmin(1) = Sqrt (wr(3) * delmin(1)/wr(2))  ! normalized min difference


      if (mype .EQ. 0) Write (6,990) diff, delmax(1), delmin(1)

 990  format ("diff, delmax, delmin = ",
     &        e10.3, 1x, e10.3, 1x, e10.3)



      Return

      end

