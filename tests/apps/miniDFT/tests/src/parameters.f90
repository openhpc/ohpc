!
! Copyright (C) 2001-2009 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
MODULE parameters
  
  IMPLICIT NONE
  SAVE

  INTEGER, PARAMETER :: &
       ntypx  = 10,     &! max number of different types of atom
       npsx   = ntypx,  &! max number of different PPs (obsolete)
       nsx    = ntypx,  &! max number of atomic species (CP)
       npk    = 40000,  &! max number of k-points               
       lmaxx  = 3,      &! max non local angular momentum (l=0 to lmaxx)      
       lqmax= 2*lmaxx+1  ! max number of angular momenta of Q

END MODULE parameters
