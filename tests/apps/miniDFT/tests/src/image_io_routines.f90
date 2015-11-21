!
! Copyright (C) 2002-2006 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------------
MODULE image_io_routines
  !----------------------------------------------------------------------------
  !
  ! ... This module contains all subroutines used for I/O in image
  ! ... parallelization
  !
  ! ... from the orignal path_io Written by Carlo Sbraccia ( 2003-2006 )
  !
  USE kinds,      ONLY : DP
  USE io_global,  ONLY : meta_ionode, meta_ionode_id
  !
  IMPLICIT NONE
  !
  PRIVATE
  !
  PUBLIC :: io_image_start, io_image_stop
  !
  CONTAINS
     !
     !-----------------------------------------------------------------------
     SUBROUTINE io_image_start()
       !-----------------------------------------------------------------------
       !
       USE io_global, ONLY : ionode, ionode_id
       USE mp_global, ONLY : me_image, root_image
       !
       IMPLICIT NONE
       !
       !
       ! ... the I/O node is set again according to the number of parallel
       ! ... images that have been required: for each parallel image there
       ! ... is only one node that does I/O
       !
       ionode = ( me_image == root_image )
       ionode_id = root_image
       !
       RETURN
       !
     END SUBROUTINE io_image_start
     !
     !
     !-----------------------------------------------------------------------
     SUBROUTINE io_image_stop()
       !-----------------------------------------------------------------------
       !
       USE io_global, ONLY : io_global_start
       USE mp_global, ONLY : mpime, root
       !
       IMPLICIT NONE
       !
       !
       ! ... the original I/O node is set again
       !
       CALL io_global_start( mpime, root )
       !
       RETURN
       !
     END SUBROUTINE io_image_stop
     !
END MODULE image_io_routines
