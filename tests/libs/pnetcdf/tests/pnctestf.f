!
!   Copyright (C) 2003, Northwestern University and Argonne National Laboratory
!   See COPYRIGHT notice in top-level directory.
!
!   $Id: pnctestf.f 2224 2015-12-16 06:10:36Z wkliao $
!
      program Pnf_Test
! Test program thanks to From: John Tannahill <tannahill1@llnl.gov> 

      implicit none
      include "mpif.h"
      include "pnetcdf.inc"

      integer*8 TOTSIZ_3D(3)

      logical reorder

      logical isperiodic(3)

      integer comm_cart
      integer ierr
      integer lat_id, lev_id, lon_id
      integer ncid
      integer totpes
      integer tt_id

      integer dim_id(3)

      integer numpes(3)
!    number of PEs along axes;
!    determined by MPI where a zero is specified

      data totsiz_3d/10,20,30/
      data reorder /.false./
      data isperiodic /.false., .false., .false./
      data numpes/1,1,0/

      call MPI_Init(ierr)

      call MPI_Comm_Size(MPI_COMM_WORLD, totpes, ierr)

      call MPI_Dims_Create(totpes, 3, numpes, ierr)

      call MPI_Cart_Create(MPI_COMM_WORLD, 3, numpes, isperiodic,
     +                     reorder, comm_cart, ierr)

      ierr = nfmpi_create(comm_cart, "pnf_test.nc", NF_CLOBBER,
     +                    MPI_INFO_NULL, ncid)

      ierr = nfmpi_def_dim(ncid, "level",     totsiz_3d(1), lev_id)
      ierr = nfmpi_def_dim(ncid, "latitude",  totsiz_3d(2), lat_id)
      ierr = nfmpi_def_dim(ncid, "longitude", totsiz_3d(3), lon_id)

      dim_id(1) = lev_id
      dim_id(2) = lat_id
      dim_id(3) = lon_id

      ierr = nfmpi_def_var(ncid, "tt", NF_FLOAT, 3, dim_id, tt_id)

      ierr = nfmpi_enddef(ncid)

      ierr = nfmpi_close(ncid)

      call MPI_Comm_Free(comm_cart, ierr)

      call MPI_Finalize  (ierr)

      Write (6,10)

 10   format(" No Errors")

      Stop

      end ! program Pnf_Test
