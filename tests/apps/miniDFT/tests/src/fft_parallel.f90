!
! Copyright (C) 2001-2009 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!=---------------------------------------------------------------------==!
!
!
!     Parallel 3D FFT high level Driver
!     ( Charge density and Wave Functions )
!
!     Written and maintained by Carlo Cavazzoni
!     Last update Apr. 2009
!
!=---------------------------------------------------------------------==!
!
MODULE fft_parallel
!
   IMPLICIT NONE
   SAVE
!
CONTAINS
!
!  General purpose driver, including Task groups parallelization
!
!----------------------------------------------------------------------------
SUBROUTINE tg_cft3s( f, dfft, isgn, use_task_groups )
  !----------------------------------------------------------------------------
  !
  ! ... isgn = +-1 : parallel 3d fft for rho and for the potential
  !                  NOT IMPLEMENTED WITH TASK GROUPS
  ! ... isgn = +-2 : parallel 3d fft for wavefunctions
  !
  ! ... isgn = +   : G-space to R-space, output = \sum_G f(G)exp(+iG*R)
  ! ...              fft along z using pencils        (cft_1z)
  ! ...              transpose across nodes           (fft_scatter)
  ! ...                 and reorder
  ! ...              fft along y (using planes) and x (cft_2xy)
  ! ... isgn = -   : R-space to G-space, output = \int_R f(R)exp(-iG*R)/Omega
  ! ...              fft along x and y(using planes)  (cft_2xy)
  ! ...              transpose across nodes           (fft_scatter)
  ! ...                 and reorder
  ! ...              fft along z using pencils        (cft_1z)
  !
  ! ...  The array "planes" signals whether a fft is needed along y :
  ! ...    planes(i)=0 : column f(i,*,*) empty , don't do fft along y
  ! ...    planes(i)=1 : column f(i,*,*) filled, fft along y needed
  ! ...  "empty" = no active components are present in f(i,*,*)
  ! ...            after (isgn>0) or before (isgn<0) the fft on z direction
  !
  ! ...  Note that if isgn=+/-1 (fft on rho and pot.) all fft's are needed
  ! ...  and all planes(i) are set to 1
  !
  ! This driver is based on code written by Stefano de Gironcoli for PWSCF.
  ! Task Group added by Costas Bekas, Oct. 2005, adapted from the CPMD code
  ! (Alessandro Curioni) and revised by Carlo Cavazzoni 2007.
  !
  USE fft_scalar, ONLY : cft_1z, cft_2xy
  USE fft_base,   ONLY : fft_scatter
  USE kinds,      ONLY : DP
  USE fft_types,  ONLY : fft_dlay_descriptor
  USE parallel_include

  !
  IMPLICIT NONE
  !
  COMPLEX(DP), INTENT(inout)    :: f( : )  ! array containing data to be transformed
  TYPE (fft_dlay_descriptor), INTENT(in) :: dfft
                                           ! descriptor of fft data layout
  INTEGER, INTENT(in)           :: isgn    ! fft direction
  LOGICAL, OPTIONAL, INTENT(in) :: use_task_groups
                                           ! specify if you want to use task groups parallelization
  !
  INTEGER                    :: me_p
  INTEGER                    :: n1, n2, n3, nx1, nx2, nx3
  COMPLEX(DP), ALLOCATABLE   :: yf(:), aux (:)
  INTEGER                    :: planes( dfft%nr1x )
  LOGICAL                    :: use_tg
  !
  !
  IF( present( use_task_groups ) ) THEN
     use_tg = use_task_groups
  ELSE
     use_tg = .false.
  ENDIF
  !
  IF( use_tg .and. .not. dfft%have_task_groups ) &
     CALL errore( ' tg_cft3s ', ' call requiring task groups for a descriptor without task groups ', 1 )
  !
  n1  = dfft%nr1
  n2  = dfft%nr2
  n3  = dfft%nr3
  nx1 = dfft%nr1x
  nx2 = dfft%nr2x
  nx3 = dfft%nr3x
  !
  IF( use_tg ) THEN
     ALLOCATE( aux( dfft%nogrp * dfft%tg_nnr ) )
     ALLOCATE( YF ( dfft%nogrp * dfft%tg_nnr ) )
  ELSE
     ALLOCATE( aux( dfft%tg_nnr ) )
  ENDIF
  !
  me_p = dfft%mype + 1
  !
  IF ( isgn > 0 ) THEN
     !
     IF ( isgn /= 2 ) THEN
        !
        IF( use_tg ) &
           CALL errore( ' tg_cfft ', ' task groups on large mesh not implemented ', 1 )
        !
        CALL cft_1z( f, dfft%nsp( me_p ), n3, nx3, isgn, aux )
        !
        planes = dfft%iplp
        !
     ELSE
        !
        CALL pack_group_sticks()
        !
        IF( use_tg ) THEN
           CALL cft_1z( yf, dfft%tg_nsw( me_p ), n3, nx3, isgn, aux )
        ELSE
           CALL cft_1z( f, dfft%nsw( me_p ), n3, nx3, isgn, aux )
        ENDIF
        !
        planes = dfft%iplw
        !
     ENDIF
     !
     CALL fw_scatter( isgn ) ! forwart scatter from stick to planes
     !
     IF( use_tg ) THEN
        CALL cft_2xy( f, dfft%tg_npp( me_p ), n1, n2, nx1, nx2, isgn, planes )
     ELSE
        CALL cft_2xy( f, dfft%npp( me_p ), n1, n2, nx1, nx2, isgn, planes )
     ENDIF
     !
  ELSE
     !
     IF ( isgn /= -2 ) THEN
        !
        IF( use_tg ) &
           CALL errore( ' tg_cfft ', ' task groups on large mesh not implemented ', 1 )
        !
        planes = dfft%iplp
        !
     ELSE
        !
        planes = dfft%iplw
        !
     ENDIF

     IF( use_tg ) THEN
        CALL cft_2xy( f, dfft%tg_npp( me_p ), n1, n2, nx1, nx2, isgn, planes )
     ELSE
        CALL cft_2xy( f, dfft%npp( me_p ), n1, n2, nx1, nx2, isgn, planes)
     ENDIF
     !
     CALL bw_scatter( isgn )
     !
     IF ( isgn /= -2 ) THEN
        !
        CALL cft_1z( aux, dfft%nsp( me_p ), n3, nx3, isgn, f )
         !
     ELSE
        !
        IF( use_tg ) THEN
           CALL cft_1z( aux, dfft%tg_nsw( me_p ), n3, nx3, isgn, yf )
        ELSE
           CALL cft_1z( aux, dfft%nsw( me_p ), n3, nx3, isgn, f )
        ENDIF
        !
        CALL unpack_group_sticks()
        !
     ENDIF
     !
  ENDIF
  !
  DEALLOCATE( aux )
  !
  IF( use_tg ) THEN
     DEALLOCATE( yf )
  ENDIF
  !
  RETURN
  !
CONTAINS
  !

  SUBROUTINE pack_group_sticks()

     INTEGER                     :: ierr
     !
     IF( .not. use_tg ) RETURN
     !
     IF( dfft%tg_rdsp(dfft%nogrp) + dfft%tg_rcv(dfft%nogrp) > size( yf ) ) THEN
        CALL errore( ' tg_cfft ', ' inconsistent size ', 1 )
     ENDIF
     IF( dfft%tg_psdsp(dfft%nogrp) + dfft%tg_snd(dfft%nogrp) > size( f ) ) THEN
        CALL errore( ' tg_cfft ', ' inconsistent size ', 2 )
     ENDIF

     CALL start_clock( 'ALLTOALL' )
     !
     !  Collect all the sticks of the different states,
     !  in "yf" processors will have all the sticks of the OGRP


     CALL MPI_ALLTOALLV( f(1), dfft%tg_snd, dfft%tg_psdsp, MPI_DOUBLE_COMPLEX, yf(1), dfft%tg_rcv, &
      &                     dfft%tg_rdsp, MPI_DOUBLE_COMPLEX, dfft%ogrp_comm, IERR)
     IF( ierr /= 0 ) THEN
        CALL errore( ' tg_cfft ', ' alltoall error 1 ', abs(ierr) )
     ENDIF


     CALL stop_clock( 'ALLTOALL' )
     !
     !YF Contains all ( ~ NOGRP*dfft%nsw(me) ) Z-sticks
     !
     RETURN
  END SUBROUTINE pack_group_sticks

  !

  SUBROUTINE unpack_group_sticks()
     !
     !  Bring pencils back to their original distribution
     !
     INTEGER                     :: ierr
     !
     IF( .not. use_tg ) RETURN
     !
     IF( dfft%tg_usdsp(dfft%nogrp) + dfft%tg_snd(dfft%nogrp) > size( f ) ) THEN
        CALL errore( ' tg_cfft ', ' inconsistent size ', 3 )
     ENDIF
     IF( dfft%tg_rdsp(dfft%nogrp) + dfft%tg_rcv(dfft%nogrp) > size( yf ) ) THEN
        CALL errore( ' tg_cfft ', ' inconsistent size ', 4 )
     ENDIF

     CALL start_clock( 'ALLTOALL' )

     CALL MPI_Alltoallv( yf(1), &
          dfft%tg_rcv, dfft%tg_rdsp, MPI_DOUBLE_COMPLEX, f(1), &
          dfft%tg_snd, dfft%tg_usdsp, MPI_DOUBLE_COMPLEX, dfft%ogrp_comm, IERR)
     IF( ierr /= 0 ) THEN
        CALL errore( ' tg_cfft ', ' alltoall error 2 ', abs(ierr) )
     ENDIF

     CALL stop_clock( 'ALLTOALL' )

     RETURN
  END SUBROUTINE unpack_group_sticks

  !

  SUBROUTINE fw_scatter( iopt )

        !Transpose data for the 2-D FFT on the x-y plane
        !
        !NOGRP*dfft%nnr: The length of aux and f
        !nr3x: The length of each Z-stick
        !aux: input - output
        !f: working space
        !isgn: type of scatter
        !dfft%nsw(me) holds the number of Z-sticks proc. me has.
        !dfft%npp: number of planes per processor
        !
     !
     USE fft_base, ONLY: fft_scatter
     !
     INTEGER, INTENT(in) :: iopt
     !
     IF( iopt == 2 ) THEN
        !
        IF( use_tg ) THEN
           !
           CALL fft_scatter( dfft, aux, nx3, dfft%nogrp*dfft%tg_nnr, f, dfft%tg_nsw, dfft%tg_npp, iopt, use_tg )
           !
        ELSE
           !
           CALL fft_scatter( dfft, aux, nx3, dfft%nnr, f, dfft%nsw, dfft%npp, iopt )
           !
        ENDIF
        !
     ELSEIF( iopt == 1 ) THEN
        !
        CALL fft_scatter( dfft, aux, nx3, dfft%nnr, f, dfft%nsp, dfft%npp, iopt )
        !
     ENDIF
     !
     RETURN
  END SUBROUTINE fw_scatter

  !

  SUBROUTINE bw_scatter( iopt )
     !
     USE fft_base, ONLY: fft_scatter
     !
     INTEGER, INTENT(in) :: iopt
     !
     IF( iopt == -2 ) THEN
        !
        IF( use_tg ) THEN
           !
           CALL fft_scatter( dfft, aux, nx3, dfft%nogrp*dfft%tg_nnr, f, dfft%tg_nsw, dfft%tg_npp, iopt, use_tg )
           !
        ELSE
           !
           CALL fft_scatter( dfft, aux, nx3, dfft%nnr, f, dfft%nsw, dfft%npp, iopt )
           !
        ENDIF
        !
     ELSEIF( iopt == -1 ) THEN
        !
        CALL fft_scatter( dfft, aux, nx3, dfft%nnr, f, dfft%nsp, dfft%npp, iopt )
        !
     ENDIF
     !
     RETURN
  END SUBROUTINE bw_scatter
  !
END SUBROUTINE tg_cft3s
!
END MODULE fft_parallel
