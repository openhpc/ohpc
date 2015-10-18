!
! Copyright (C) 2002-2011 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!=----------------------------------------------------------------------------=!
   MODULE wavefunctions_module
!=----------------------------------------------------------------------------=!
     USE kinds, ONLY :  DP

     IMPLICIT NONE
     SAVE

     !
     COMPLEX(DP), ALLOCATABLE, TARGET :: &
       evc(:,:)     ! wavefunctions in the PW basis set
                    ! noncolinear case: first index
                    ! is a combined PW + spin index
     !
     COMPLEX(DP) , ALLOCATABLE, TARGET :: &
       psic(:), &      ! additional memory for FFT
       psic_nc(:,:)    ! as above for the noncolinear case
     !
     !
     ! electronic wave functions, CPV code
     ! distributed over gvector and bands
     !
     COMPLEX(DP), ALLOCATABLE :: c0_bgrp(:,:)  ! wave functions at time t
     COMPLEX(DP), ALLOCATABLE :: cm_bgrp(:,:)  ! wave functions at time t-delta t
     COMPLEX(DP), ALLOCATABLE :: phi_bgrp(:,:) ! |phi> = s'|c0> = |c0> + sum q_ij |i><j|c0>
     ! for hybrid functionals in CP with Wannier functions
     COMPLEX(DP), ALLOCATABLE :: cv0(:,:) ! Lingzhu Kong

   CONTAINS

      SUBROUTINE deallocate_wavefunctions
       IF( ALLOCATED( cv0) ) DEALLOCATE( cv0)   ! Lingzhu Kong
       IF( ALLOCATED( c0_bgrp ) ) DEALLOCATE( c0_bgrp )
       IF( ALLOCATED( cm_bgrp ) ) DEALLOCATE( cm_bgrp )
       IF( ALLOCATED( phi_bgrp ) ) DEALLOCATE( phi_bgrp )
       IF( ALLOCATED( psic_nc ) ) DEALLOCATE( psic_nc )
       IF( ALLOCATED( psic ) ) DEALLOCATE( psic )
       IF( ALLOCATED( evc ) ) DEALLOCATE( evc )
     END SUBROUTINE deallocate_wavefunctions

!=----------------------------------------------------------------------------=!
   END MODULE wavefunctions_module
!=----------------------------------------------------------------------------=!
