!
! Copyright (C) 2010 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Extracted from "cp_interfaces", written by Carlo Cavazzoni

!=----------------------------------------------------------------------------=!
   MODULE fft_interfaces
!=----------------------------------------------------------------------------=!

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: fwfft, invfft

   INTERFACE invfft
      SUBROUTINE invfft_x( grid_type, f, dfft, ia )
         USE fft_types,  only: fft_dlay_descriptor
         USE kinds,      ONLY: DP
         IMPLICIT NONE
         INTEGER, OPTIONAL, INTENT(IN) :: ia
         CHARACTER(LEN=*),  INTENT(IN) :: grid_type
         TYPE(fft_dlay_descriptor), INTENT(IN) :: dfft
         COMPLEX(DP) :: f(:)
      END SUBROUTINE
   END INTERFACE

   INTERFACE fwfft
      SUBROUTINE fwfft_x( grid_type, f, dfft )
         USE fft_types,  only: fft_dlay_descriptor
         USE kinds,      ONLY: DP
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: grid_type
         TYPE(fft_dlay_descriptor), INTENT(IN) :: dfft
         COMPLEX(DP) :: f(:)
      END SUBROUTINE
   END INTERFACE

!=----------------------------------------------------------------------------=!
   END MODULE
!=----------------------------------------------------------------------------=!
!
!
! Copyright (C) 2002-2010 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!  ----------------------------------------------
!  These subroutines written by Carlo Cavazzoni 
!  Last modified August 2010 by Paolo Giannozzi
!  ----------------------------------------------

!-----------------------------------------------------------------------
  subroutine invfft_x( grid_type, f, dfft, ia )
!-----------------------------------------------------------------------
! grid_type = 'Dense'
!   inverse fourier transform of potentials and charge density
!   on the dense grid . On output, f is overwritten
! grid_type = 'Smooth'
!   inverse fourier transform of  potentials and charge density
!   on the smooth grid . On output, f is overwritten
! grid_type = 'Wave'
!   inverse fourier transform of  wave functions
!   on the smooth grid . On output, f is overwritten
! grid_type = 'Box'
!   not-so-parallel 3d fft for box grid, implemented only for sign=1
!   G-space to R-space, output = \sum_G f(G)exp(+iG*R)
!   The array f (overwritten on output) is NOT distributed:
!   a copy is present on each processor.
!   The fft along z  is done on the entire grid.
!   The fft along xy is done only on planes that have components on the
!   dense grid for each processor. Note that the final array will no
!   longer be the same on all processors.
! grid_type = 'Custom'
!   inverse fourier transform of potentials and charge density
!   on a custom defined grid specified by dfft. On output, f 
!   is overwritten
! grid_type = 'CustomWave'
!   inverse fourier transform of  wave functions
!   on a custom defined grid specified by dfft. On output, f 
!   is overwritten
!
!
      USE kinds,         ONLY: DP
      use fft_base,      only: dfftp, dffts, dfftb
      use fft_scalar,    only: cft_b
      use fft_parallel,  only: tg_cft3s
      USE fft_types,     only: fft_dlay_descriptor

      IMPLICIT none

      TYPE(fft_dlay_descriptor), INTENT(IN) :: dfft
      INTEGER, OPTIONAL, INTENT(IN) :: ia
      CHARACTER(LEN=*), INTENT(IN) :: grid_type
      COMPLEX(DP) :: f(:)
      !
      INTEGER :: imin3, imax3, np3

      IF( grid_type == 'Dense' ) THEN
         IF( dfft%nr1  /= dfftp%nr1  .OR. dfft%nr2  /= dfftp%nr2  .OR. &
             dfft%nr3  /= dfftp%nr3  .OR. dfft%nr1x /= dfftp%nr1x .OR. &
             dfft%nr2x /= dfftp%nr2x .OR. dfft%nr3x /= dfftp%nr3x ) &
         CALL errore( ' invfft ', ' inconsistent descriptor for Dense fft ', 1 )
         call start_clock( 'fft' )
      ELSE IF( grid_type == 'Smooth' ) THEN
         IF( dfft%nr1  /= dffts%nr1  .OR. dfft%nr2  /= dffts%nr2  .OR. &
             dfft%nr3  /= dffts%nr3  .OR. dfft%nr1x /= dffts%nr1x .OR. &
             dfft%nr2x /= dffts%nr2x .OR. dfft%nr3x /= dffts%nr3x ) &
         CALL errore( ' invfft ', ' inconsistent descriptor for Smooth fft ', 1)
         call start_clock( 'ffts' )
      ELSE IF( grid_type == 'Wave' ) THEN
         IF( dfft%nr1  /= dffts%nr1  .OR. dfft%nr2  /= dffts%nr2  .OR. &
             dfft%nr3  /= dffts%nr3  .OR. dfft%nr1x /= dffts%nr1x .OR. &
             dfft%nr2x /= dffts%nr2x .OR. dfft%nr3x /= dffts%nr3x ) &
         CALL errore( ' invfft ', ' inconsistent descriptor for Wave fft ' , 1 )
         call start_clock('fftw')
      ELSE IF( grid_type == 'Box' ) THEN
         IF( dfft%nr1  /= dfftb%nr1  .OR. dfft%nr2  /= dfftb%nr2  .OR. &
             dfft%nr3  /= dfftb%nr3  .OR. dfft%nr1x /= dfftb%nr1x .OR. &
             dfft%nr2x /= dfftb%nr2x .OR. dfft%nr3x /= dfftb%nr3x ) &
         CALL errore( ' invfft ', ' inconsistent descriptor for Box fft ', 1 )
!$omp master
         !
         ! clocks called inside a parallel region do not work properly!
         ! in the future we probably need a thread safe version of the clock
         !
         call start_clock( 'fftb' )
!$omp end master 
      ELSE IF( grid_type == 'Custom' ) THEN
         call start_clock('fftc')
      ELSE IF( grid_type == 'CustomWave' ) THEN
         call start_clock('fftcw')
      ELSE 
         call errore( ' invfft ', ' unknown grid: '//grid_type , 1 )
      END IF


      IF( grid_type == 'Dense' ) THEN
         call tg_cft3s( f, dfftp, 1 )
      ELSE IF( grid_type == 'Smooth' ) THEN
         call tg_cft3s( f, dffts, 1 )
      ELSE IF( grid_type == 'Wave' ) THEN
         call tg_cft3s( f, dffts, 2, dffts%have_task_groups )
      ELSE IF( grid_type == 'Custom' ) THEN
         CALL tg_cft3s( f, dfft, 1 )
      ELSE IF( grid_type == 'CustomWave' ) THEN
         CALL tg_cft3s( f, dfft, 2, dfft%have_task_groups )
      ELSE IF( grid_type == 'Box' .AND.  dfftb%np3( ia ) > 0 ) THEN
         call cft_b( f, dfftb%nr1, dfftb%nr2, dfftb%nr3, &
                        dfftb%nr1x, dfftb%nr2x, dfftb%nr3x, &
                        dfftb%imin3( ia ), dfftb%imax3( ia ), 1 )
      END IF


      IF( grid_type == 'Dense' ) THEN
         call stop_clock( 'fft' )
      ELSE IF( grid_type == 'Smooth' ) THEN
         call stop_clock( 'ffts' )
      ELSE IF( grid_type == 'Wave' ) THEN
         call stop_clock('fftw')
      ELSE IF( grid_type == 'Box' ) THEN
!$omp master
         call stop_clock( 'fftb' )
!$omp end master
      ELSE IF( grid_type == 'Custom' ) THEN
         call stop_clock('fftc')
      ELSE IF( grid_type == 'CustomWave' ) THEN
         call stop_clock('fftcw')
      END IF
!
      return
      end subroutine invfft_x

!-----------------------------------------------------------------------
      subroutine fwfft_x( grid_type, f, dfft )
!-----------------------------------------------------------------------
! grid_type = 'Dense'
!   forward fourier transform of potentials and charge density 
!   on the dense grid . On output, f is overwritten
! grid_type = 'Smooth'
!   forward fourier transform of potentials and charge density
!   on the smooth grid . On output, f is overwritten
! grid_type = 'Wave'
!   forward fourier transform of  wave functions
!   on the smooth grid . On output, f is overwritten
! grid_type = 'Custom'
!   forward fourier transform of potentials and charge density
!   on a custom defined grid specified by dfft. On output, f 
!   is overwritten
! grid_type = 'CustomWave'
!   forward fourier transform of  wave functions
!   on a custom defined grid specified by dfft. On output, f 
!   is overwritten
! 
      USE kinds,         ONLY: DP
      use fft_base,      only: dfftp, dffts
      use fft_parallel,  only: tg_cft3s
      USE fft_types,     only: fft_dlay_descriptor

      implicit none

      TYPE(fft_dlay_descriptor), INTENT(IN) :: dfft
      CHARACTER(LEN=*), INTENT(IN) :: grid_type
      COMPLEX(DP) :: f(:)

      IF( grid_type == 'Dense' ) THEN
         IF( dfft%nr1  /= dfftp%nr1  .OR. dfft%nr2  /= dfftp%nr2  .OR. &
             dfft%nr3  /= dfftp%nr3  .OR. dfft%nr1x /= dfftp%nr1x .OR. &
             dfft%nr2x /= dfftp%nr2x .OR. dfft%nr3x /= dfftp%nr3x ) &
         CALL errore( ' fwfft ', ' inconsistent descriptor for Dense fft ', 1 )
         call start_clock( 'fft' )
      ELSE IF( grid_type == 'Smooth' ) THEN
         IF( dfft%nr1  /= dffts%nr1  .OR. dfft%nr2  /= dffts%nr2  .OR. &
             dfft%nr3  /= dffts%nr3  .OR. dfft%nr1x /= dffts%nr1x .OR. &
             dfft%nr2x /= dffts%nr2x .OR. dfft%nr3x /= dffts%nr3x ) &
         CALL errore( ' fwfft ', ' inconsistent descriptor for Smooth fft ', 1 )
         call start_clock( 'ffts' )
      ELSE IF( grid_type == 'Wave' ) THEN
         IF( dfft%nr1  /= dffts%nr1  .OR. dfft%nr2  /= dffts%nr2  .OR. &
             dfft%nr3  /= dffts%nr3  .OR. dfft%nr1x /= dffts%nr1x .OR. &
            dfft%nr2x /= dffts%nr2x .OR. dfft%nr3x /= dffts%nr3x ) &
         CALL errore( ' fwfft ', ' inconsistent descriptor for Wave fft ', 1 )
         call start_clock( 'fftw' )
      ELSE IF( grid_type == 'Custom' ) THEN
         call start_clock('fftc')
      ELSE IF( grid_type == 'CustomWave' ) THEN
         call start_clock('fftcw')
      ELSE
         call errore( ' fwfft ', ' unknown grid: '//grid_type , 1 )
      END IF


      IF( grid_type == 'Dense' ) THEN
         call tg_cft3s(f,dfftp,-1)
      ELSE IF( grid_type == 'Smooth' ) THEN
         call tg_cft3s(f,dffts,-1)
      ELSE IF( grid_type == 'Wave' ) THEN
         call tg_cft3s(f,dffts,-2, dffts%have_task_groups )
      ELSE  IF( grid_type == 'Custom' ) THEN
         CALL tg_cft3s( f, dfft, -1 )
      ELSE IF( grid_type == 'CustomWave' ) THEN
         CALL tg_cft3s( f, dfft, -2, dfft%have_task_groups )
      END IF


      IF( grid_type == 'Dense' ) THEN
         call stop_clock( 'fft' )
      ELSE IF( grid_type == 'Smooth' ) THEN
         call stop_clock( 'ffts' )
      ELSE IF( grid_type == 'Wave' ) THEN
         call stop_clock( 'fftw' )
      ELSE IF( grid_type == 'Custom' ) THEN
         call stop_clock('fftc')
      ELSE IF( grid_type == 'CustomWave' ) THEN
         call stop_clock('fftcw')
      END IF

      return
      end subroutine fwfft_x
