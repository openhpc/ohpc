! Copyright (C) 2008 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!=----------------------------------------------------------------------------=!
      MODULE upf_module
!=----------------------------------------------------------------------------=!
!  this module handles reading and writing of unified pseudopotential format (UPF)
!  it can manage v2 read/write and v1 read only.
!
! A macro to trim both from left and right
#define TRIM(a) trim(adjustl(a))
      !
      USE kinds,        ONLY: DP
      USE pseudo_types, ONLY: pseudo_upf, deallocate_pseudo_upf
      !
      IMPLICIT NONE
      PUBLIC
      !
      CONTAINS

!------------------------------------------------+
SUBROUTINE read_upf(upf, grid, ierr, unit, filename)             !
   !---------------------------------------------+
   ! Read pseudopotential in UPF format (either v.1 or v.2)
   ! ierr = -1 : read UPF v.1 
   ! ierr =  0 : read UPF v.2 
   ! ierr =  1 : not an UPF file, or error while reading
   !
   USE radial_grids, ONLY: radial_grid_type, deallocate_radial_grid
   use upf_nml, only: upf_nml_read
   IMPLICIT NONE
   INTEGER,INTENT(IN),OPTIONAL             :: unit      ! i/o unit
   CHARACTER(len=*),INTENT(IN),OPTIONAL    :: filename  ! i/o filename
   TYPE(pseudo_upf),INTENT(INOUT) :: upf       ! the pseudo data
   TYPE(radial_grid_type),OPTIONAL,INTENT(INOUT),TARGET :: grid
   INTEGER,INTENT(OUT) :: ierr
   !
   INTEGER :: u         ! i/o unit

   ierr = 0

   IF(.not. present(unit)) THEN
      IF (.not. present(filename)) &
         CALL errore('read_upf',&
         'You have to specify at least one between filename and unit',1)
   ELSE
      u = unit
   ENDIF
   !
   !call infomsg( "upf.f90:59", "replacing read_upf_v2 with upf_nml_read to avoit iotk" )
   IF( present(filename) ) &
        open( unit=u, file=trim(filename), status='old', delim='APOSTROPHE', iostat=ierr )
   IF(ierr>0) CALL errore('read_upf', 'Cannot open file: '//TRIM(filename),1)
   call upf_nml_read( u, upf, grid, ierr )
   close( u, status='keep' )
   !

   RETURN

END SUBROUTINE read_upf



!=----------------------------------------------------------------------------=!
      END MODULE upf_module
!=----------------------------------------------------------------------------=!
#undef TRIM

