!
! Copyright (C) 2006 Malgorzata Wierbowska and Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
MODULE a2F
  !
  ! This module contains a routine saving variables needed for the
  ! electron-phonon calculation (new algorithm implemeted by MW) 
  !
  USE kinds,      ONLY : DP
  !
  LOGICAL :: la2F = .FALSE.
  !
  PRIVATE
  PUBLIC :: la2F, a2Fsave
  CONTAINS
    !
    SUBROUTINE a2Fsave
  
      USE kinds,        ONLY : DP
      USE klist,        ONLY : nks, nkstot, xk, wk
      USE ions_base,    ONLY : nat
      USE wvfct,        ONLY : et, nbnd
      USE start_k,      ONLY : nk1, nk2, nk3 
      USE symm_base,    ONLY : s, nsym, irt
      USE io_global,    ONLY : ionode
      USE io_files,     ONLY : seqopn
      implicit none
      !
      INTEGER :: iuna2Fsave  = 40, i, j, ik, ns, na
      logical  ::  exst
      !
      ! parallel case: only first node writes
      IF ( ionode ) THEN
         !
         CALL seqopn( iuna2Fsave, 'a2Fsave', 'FORMATTED', exst )
         !===========================================
         !
         WRITE( iuna2Fsave, * ) nbnd, nkstot
         WRITE( iuna2Fsave, * ) et
         WRITE( iuna2Fsave, * ) ((xk(i,ik), i=1,3), ik=1,nkstot)
         WRITE( iuna2Fsave, * ) wk(1:nkstot)
         WRITE( iuna2Fsave, * ) nk1, nk2, nk3
         !
         WRITE( iuna2Fsave, * ) nsym
         do ns=1,nsym
            WRITE( iuna2Fsave, * )  ((s(i,j,ns),j=1,3),i=1,3) 
         enddo
         WRITE( iuna2Fsave, * )  ((irt(ns,na),ns=1,nsym),na=1,nat)
         !
         CLOSE( UNIT = iuna2Fsave, STATUS = 'KEEP' )
         !  
      END IF
      !
      RETURN
    END SUBROUTINE a2Fsave
  END MODULE a2F
