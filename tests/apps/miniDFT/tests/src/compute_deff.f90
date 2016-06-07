!
! Copyright (C) 2009-2010 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!---------------------------------------------------------------------------
SUBROUTINE compute_deff(deff, et)
!
!  This routine computes the effective value of the D-eS coefficients
!  which appear often in many expressions in the US or PAW case. 
!  This routine is for the collinear case.
!
USE kinds, ONLY : DP
USE ions_base, ONLY : nsp, nat, ityp
USE uspp, ONLY : deeq, qq, okvan
USE uspp_param, ONLY : nhm
USE lsda_mod, ONLY : current_spin
IMPLICIT NONE

INTEGER :: nt, na, is
REAL(DP), INTENT(OUT) :: deff(nhm, nhm, nat) 
REAL(DP), INTENT(IN) :: et

deff(:,:,:) = deeq(:,:,:,current_spin) 
RETURN
END SUBROUTINE compute_deff
