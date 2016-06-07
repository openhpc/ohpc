!
! Copyright (C) 2003-2004 Carlo Cavazzoni
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!------------------------------------------------------------------------------!
!   SISSA Code Interface -- Carlo Cavazzoni
!------------------------------------------------------------------------------C
      MODULE parallel_include

!
!     Include file for MPI
!
         INCLUDE 'mpif.h'
         !
         ! this is only for symmetry with respect to the serial build
         LOGICAL :: tparallel = .true.

      END MODULE parallel_include
