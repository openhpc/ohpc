!
! Copyright (C) 2012 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!-----------------------------------------------------------------------
SUBROUTINE divide (comm, ntodiv, startn, lastn)
  !-----------------------------------------------------------------------
  ! Divide ntodiv poins across processors belonging to communicator comm 
  ! Each processor gets points from startn to lastn
  !
  !
  USE mp, ONLY : mp_size, mp_rank
  IMPLICIT NONE
  !
  INTEGER, INTENT(in) :: comm
  INTEGER, INTENT(in) :: ntodiv
  INTEGER, INTENT(out):: startn, lastn
  !
  INTEGER :: me_comm, nproc_comm
  !
  INTEGER :: nb, resto, idx, ip
  ! number of bands per processor
  ! one additional band if me_pool+1 <= resto
  ! counter on bands
  ! counter on processors
  !
  nproc_comm = mp_size(comm)
  me_comm = mp_rank(comm)
  !
  nb = ntodiv / nproc_comm
  resto = ntodiv - nb * nproc_comm
  idx = 0
  DO ip = 1, nproc_comm
     IF (ip <= resto) THEN
        IF (me_comm+1 == ip) THEN
           startn = idx + 1
           lastn = startn + nb
        ENDIF
        idx = idx + nb + 1
     ELSE
        IF (me_comm+1 == ip) THEN
           startn = idx + 1
           lastn = startn + nb - 1
        ENDIF
        idx = idx + nb
     ENDIF
  ENDDO
  RETURN

END SUBROUTINE divide

