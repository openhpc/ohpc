!
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
subroutine date_and_tim (cdate, ctime)
  !
  !     Returns two strings containing the date and the time
  !     in human-readable format. Uses a standard f90 call.
  !
  implicit none
  character (len=9) :: cdate, ctime
  !
  character(len=3), dimension(12) :: months
  data months /'Jan','Feb','Mar','Apr','May','Jun',                     &
       'Jul','Aug','Sep','Oct','Nov','Dec'/
  INTEGER date_time(8)
  !
  call date_and_time(values=date_time)
  !
  write (cdate,'(i2,a3,i4)') date_time(3), months(date_time(2)), date_time(1)
  write (ctime,'(i2,":",i2,":",i2)') date_time(5), date_time(6), date_time(7)

end subroutine date_and_tim
