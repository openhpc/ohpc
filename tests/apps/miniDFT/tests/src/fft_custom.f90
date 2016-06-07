!
! Copyright (C) 2011 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!--------------------------------------------------------------------
! Module containing routines for fft with an custom energy cutoff
!--------------------------------------------------------------------
!
MODULE fft_custom

  USE kinds, ONLY: DP
  USE parallel_include
  
  USE fft_types, ONLY: fft_dlay_descriptor
  
  IMPLICIT NONE

  TYPE fft_cus
  
     ! ... data structure containing all information
     ! ... about fft data distribution for a given
     ! ... potential grid, and its wave functions sub-grid.

     TYPE ( fft_dlay_descriptor ) :: dfftt 
     ! descriptor for the custom grid

     REAL(kind=DP) :: ecutt
     ! Custom cutoff (rydberg)
     REAL(kind=DP) :: dual_t
     ! Dual factor
     REAL(kind=DP) :: gcutmt
     INTEGER :: nr1t,nr2t,nr3t
     INTEGER :: nrx1t,nrx2t,nrx3t
     INTEGER :: nrxxt
     INTEGER :: ngmt,ngmt_l,ngmt_g
     INTEGER, DIMENSION(:), POINTER :: nlt,nltm
     REAL(kind=DP), DIMENSION(:), POINTER :: ggt
     REAL(kind=DP), DIMENSION(:,:),POINTER :: gt
     INTEGER, DIMENSION(:), POINTER :: ig_l2gt 
     INTEGER :: gstart_t
     INTEGER,  DIMENSION(:), POINTER :: ig1t,ig2t,ig3t
     INTEGER :: nlgt
     INTEGER :: npwt,npwxt
     LOGICAL :: initalized = .FALSE.
     
  END TYPE fft_cus


!--------------------------------------------------------------------
CONTAINS
!=----------------------------------------------------------------------------=!

     SUBROUTINE gvec_init( fc, ngm_, comm )
       !
       ! Set local and global dimensions, allocate arrays
       !
       USE mp, ONLY: mp_max, mp_sum
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: ngm_
       INTEGER, INTENT(IN) :: comm  ! communicator of the group on which g-vecs are distributed
       TYPE(fft_cus), INTENT(INOUT) :: fc
       !
       fc%ngmt = ngm_
       !
       !  calculate maximum over all processors
       !
       fc%ngmt_l = ngm_
       CALL mp_max( fc%ngmt_l, comm )
       !
       !  calculate sum over all processors
       !
       fc%ngmt_g = ngm_
       CALL mp_sum( fc%ngmt_g, comm )
       !
       !  allocate arrays - only those that are always kept until the end
       !
       ALLOCATE( fc%ggt(fc%ngmt) )
       ALLOCATE( fc%gt (3, fc%ngmt) )
!       ALLOCATE( mill(3, fc%ngmt) )
       ALLOCATE( fc%nlt (fc%ngmt) )
       ALLOCATE( fc%nltm(fc%ngmt) )
       ALLOCATE( fc%ig_l2gt(fc%ngmt) )
!       ALLOCATE( igtongl(fc%ngmt) )
       !
       RETURN 
       !
     END SUBROUTINE gvec_init



  !--------------------------------------------------------------------
  !
  SUBROUTINE set_custom_grid(fc)
    !-----------------------------------------------------------------------
    !     This routine computes the dimensions of the minimum FFT grid
    !     compatible with the input cut-off
    !
    !     NB: The values of nr1, nr2, nr3 are computed only if they are not
    !     given as input parameters. Input values are kept otherwise.
    !
    USE cell_base,   ONLY : at, tpiba2
    USE fft_scalar,  ONLY : allowed
    
    IMPLICIT NONE
    
    TYPE(fft_cus) :: fc
    
    INTEGER, PARAMETER :: nmax = 5000
    ! an unreasonably big number for a FFT grid
    !
    ! the values of nr1, nr2, nr3 are computed only if they are not given
    ! as input parameters
    !

    fc%nr1t=0
    fc%nr2t=0
    fc%nr3t=0
    
    IF (fc%nr1t == 0) THEN
       !
       ! estimate nr1 and check if it is an allowed value for FFT
       !
       fc%nr1t = INT(2 * SQRT(fc%gcutmt) * SQRT(at(1, 1)**2 + &
            &at(2, 1)**2 + at(3, 1)**2) ) + 1  
10     CONTINUE
       IF (fc%nr1t > nmax) &
            CALL errore ('set_fft_dim', 'nr1 is unreasonably large', fc%nr1t)
       IF (allowed (fc%nr1t) ) GOTO 15
       fc%nr1t = fc%nr1t + 1
       GOTO 10
    ELSE
       IF (.NOT.allowed (fc%nr1t) ) CALL errore ('set_fft_dim', &
            'input nr1t value not allowed', 1)
    ENDIF
15  CONTINUE
    !
    IF (fc%nr2t == 0) THEN
       !
       ! estimate nr1 and check if it is an allowed value for FFT
       !
       fc%nr2t = INT(2 * SQRT(fc%gcutmt) * SQRT(at(1, 2)**2 + &
            &at(2, 2)**2 + at(3, 2)**2) ) + 1 
20     CONTINUE
       IF (fc%nr2t > nmax) &
            CALL errore ('set_fft_dim', 'nr2t is unreasonably large', fc%nr2t)
       IF (allowed (fc%nr2t) ) GOTO 25
       fc%nr2t = fc%nr2t + 1
       GOTO 20
    ELSE
       IF (.NOT.allowed (fc%nr2t) ) CALL errore ('set_fft_dim', &
            'input nr2t value not allowed', 2)
    ENDIF
25  CONTINUE
    !
    IF (fc%nr3t == 0) THEN
       !
       ! estimate nr3 and check if it is an allowed value for FFT
       !
       fc%nr3t = INT(2 * SQRT(fc%gcutmt) * SQRT(at(1, 3) **2 + &
            &at(2, 3)**2 + at(3, 3) **2) ) + 1
30     CONTINUE
       IF (fc%nr3t > nmax) &
            CALL errore ('set_fft_dim', 'nr3 is unreasonably large', fc%nr3t)
       IF (allowed (fc%nr3t) ) GOTO 35
       fc%nr3t = fc%nr3t + 1
       GOTO 30
    ELSE
       IF (.NOT.allowed (fc%nr3t) ) CALL errore ('set_fft_dim', &
            'input nr3t value not allowed', 3)
    ENDIF
35  CONTINUE
    !
    !    here we compute nr3s if it is not in input
    !
    RETURN
  END SUBROUTINE set_custom_grid

  SUBROUTINE ggent(fc)
    USE kinds,              ONLY : DP
    USE cell_base,          ONLY : at, bg, tpiba2
    USE constants,          ONLY : eps8
    
    IMPLICIT NONE
    
    TYPE(fft_cus) :: fc
    
    !
    REAL(DP) ::  t (3), tt, swap
    !
    INTEGER :: ngmx, n1, n2, n3, n1s, n2s, n3s
    !
    REAL(DP), ALLOCATABLE :: g2sort_g(:)
    ! array containing all g vectors, on all processors: replicated data
    INTEGER, ALLOCATABLE :: mill_g(:,:), mill_unsorted(:,:)
    ! array containing all g vectors generators, on all processors:
    !     replicated data
    INTEGER, ALLOCATABLE :: igsrt(:)
    !

    INTEGER :: m1, m2, mc
    !
    INTEGER :: i, j, k, ipol, ng, igl, iswap, indsw, ni, nj, nk
    
    
!    ALLOCATE( fc%gt(3,fc%ngmt), fc%ggt(fc%ngmt) )
!    ALLOCATE( fc%ig_l2gt( fc%ngmt_l ) )
    ALLOCATE( mill_g( 3, fc%ngmt_g ), mill_unsorted( 3, fc%ngmt_g ) )
    ALLOCATE( igsrt( fc%ngmt_g ) )
    ALLOCATE( g2sort_g( fc%ngmt_g ) )
    ALLOCATE( fc%ig1t(fc%ngmt), fc%ig2t(fc%ngmt), fc%ig3t(fc%ngmt) )
   
    g2sort_g(:) = 1.0d20
    !
    ! save present value of ngm in ngmx variable
    !
    ngmx = fc%ngmt
    !
    fc%ngmt = 0
    !
    ! max miller indices (same convention as in module stick_set)
    !
    ni = (fc%dfftt%nr1-1)/2
    nj = (fc%dfftt%nr2-1)/2
    nk = (fc%dfftt%nr3-1)/2
    !
    iloop: DO i = -ni, ni
       !
       ! gamma-only: exclude space with x < 0
       !
       jloop: DO j = -nj, nj
          !
          ! gamma-only: exclude plane with x = 0, y < 0
          !
          kloop: DO k = -nk, nk
             !
             ! gamma-only: exclude line with x = 0, y = 0, z < 0
             !
             t(:) = i * bg (:,1) + j * bg (:,2) + k * bg (:,3)
             tt = SUM(t(:)**2)
             IF (tt <= fc%gcutmt) THEN
                fc%ngmt = fc%ngmt + 1
                IF (fc%ngmt > fc%ngmt_g) CALL errore ('ggent', 'too ma&
                     &ny g-vectors', fc%ngmt)
                mill_unsorted( :, fc%ngmt ) = (/ i,j,k /)
                IF ( tt > eps8 ) THEN
                   g2sort_g(fc%ngmt) = tt
                ELSE
                   g2sort_g(fc%ngmt) = 0.d0
                ENDIF
             ENDIF
          ENDDO kloop
       ENDDO jloop
    ENDDO iloop
    
    IF (fc%ngmt  /= fc%ngmt_g ) &
         CALL errore ('ggen', 'g-vectors missing !', ABS(fc%ngmt - fc%ngmt_g))

    igsrt(1) = 0
    CALL hpsort_eps( fc%ngmt_g, g2sort_g, igsrt, eps8 )
    mill_g(1,:) = mill_unsorted(1,igsrt(:))
    mill_g(2,:) = mill_unsorted(2,igsrt(:))
    mill_g(3,:) = mill_unsorted(3,igsrt(:))
    DEALLOCATE( g2sort_g, igsrt, mill_unsorted )
    fc%ngmt = 0
    
    ngloop: DO ng = 1, fc%ngmt_g

       i = mill_g(1, ng)
       j = mill_g(2, ng)
       k = mill_g(3, ng)
       
       m1 = MOD (i, fc%dfftt%nr1) + 1
       IF (m1 < 1) m1 = m1 + fc%dfftt%nr1
       m2 = MOD (j, fc%dfftt%nr2) + 1
       IF (m2 < 1) m2 = m2 + fc%dfftt%nr2
       mc = m1 + (m2 - 1) * fc%dfftt%nr1x
       IF ( fc%dfftt%isind ( mc ) == 0) CYCLE ngloop
       
       fc%ngmt = fc%ngmt + 1
       
       !  Here map local and global g index !!!
       !  N.B. the global G vectors arrangement depends on the number of processors
       !
       fc%ig_l2gt( fc%ngmt ) = ng
       
       fc%gt (1:3, fc%ngmt) = i * bg (:, 1) + j * bg (:, 2) + k * bg (:, 3)
       fc%ggt (fc%ngmt) = SUM(fc%gt (1:3, fc%ngmt)**2)
       
       IF (fc%ngmt > ngmx) CALL errore ('ggen', 'too many g-vectors', fc%ngmt)
    ENDDO ngloop

    IF (fc%ngmt /= ngmx) &
         CALL errore ('ggent', 'g-vectors missing !', ABS(fc%ngmt - ngmx))
    !
    !     determine first nonzero g vector
    !
    IF (fc%ggt(1).LE.eps8) THEN
       fc%gstart_t=2
    ELSE
       fc%gstart_t=1
    ENDIF
    !
    !     Now set nl and nls with the correct fft correspondence
    !
    DO ng = 1, fc%ngmt
       n1 = NINT (SUM(fc%gt (:, ng) * at (:, 1))) + 1
       fc%ig1t (ng) = n1 - 1
       IF (n1<1) n1 = n1 + fc%dfftt%nr1
       
       
       n2 = NINT (SUM(fc%gt (:, ng) * at (:, 2))) + 1
       fc%ig2t (ng) = n2 - 1
       IF (n2<1) n2 = n2 + fc%dfftt%nr2
       
       
       n3 = NINT (SUM(fc%gt (:, ng) * at (:, 3))) + 1
       fc%ig3t (ng) = n3 - 1
       IF (n3<1) n3 = n3 + fc%dfftt%nr3
       
       
       IF (n1>fc%dfftt%nr1 .OR. n2>fc%dfftt%nr2 .OR. n3>fc%dfftt%nr3) &
            CALL errore('ggent','Mesh too small?',ng)
       
       fc%nlt (ng) = n3 + ( fc%dfftt%isind (n1 + (n2 - 1) * fc%dfftt%nr1x)&
            & - 1) * fc%dfftt%nr3x
    ENDDO
    !
    DEALLOCATE( mill_g )
    !
    ! calculate number of G shells: ngl
    
       
    
    !set npwt,npwxt
    !This should eventually be calcualted somewhere else with 
    !n_plane_waves() but it is good enough for gamma_only

!    IF( ALLOCATED( ngmpe ) ) DEALLOCATE( ngmpe )

    RETURN
    
  END SUBROUTINE ggent
  !-----------------------------------------------------------------------
  
  SUBROUTINE deallocate_fft_custom(fc)
    !this subroutine deallocates all the fft custom stuff
    USE fft_types, ONLY : fft_dlay_deallocate
    
    IMPLICIT NONE

    TYPE(fft_cus) :: fc

    IF(.NOT. fc%initalized) RETURN

    DEALLOCATE(fc%nlt,fc%nltm)
    CALL fft_dlay_deallocate(fc%dfftt)
    DEALLOCATE(fc%ig_l2gt,fc%ggt,fc%gt)
    DEALLOCATE(fc%ig1t,fc%ig2t,fc%ig3t)
    fc%initalized=.FALSE.

    RETURN

  END SUBROUTINE deallocate_fft_custom
  
END MODULE fft_custom
