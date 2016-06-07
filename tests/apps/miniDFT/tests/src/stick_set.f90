!
! Copyright (C) 2011 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!=----------------------------------------------------------------------=
   MODULE stick_set
!=----------------------------------------------------------------------=

!  ... Distribute G-vectors across processors as sticks and planes,
!  ... initialize FFT descriptors for both dense and smooth grids

!  ... Most important dependencies: next three modules
      USE stick_base
!
      USE kinds, ONLY: DP
      USE io_global, ONLY: ionode, stdout
      USE fft_types, ONLY: fft_dlay_descriptor, fft_dlay_allocate, &
                           fft_dlay_set, fft_dlay_scalar

      IMPLICIT NONE
      PRIVATE
      SAVE

      PUBLIC :: pstickset, pstickset_custom

!=----------------------------------------------------------------------=
   CONTAINS
!=----------------------------------------------------------------------=

      SUBROUTINE pstickset( gamma_only, bg, gcut, gkcut, gcuts, &
          dfftp, dffts, ngw, ngm, ngs, mype, root, nproc, comm, nogrp_ )

          LOGICAL, INTENT(in) :: gamma_only
! ...     bg(:,1), bg(:,2), bg(:,3) reciprocal space base vectors.
          REAL(DP), INTENT(in) :: bg(3,3)
          REAL(DP), INTENT(in) :: gcut, gkcut, gcuts
          TYPE(fft_dlay_descriptor), INTENT(inout) :: dfftp, dffts
          INTEGER, INTENT(out) :: ngw, ngm, ngs

          INTEGER, INTENT(IN) :: mype, root, nproc, comm
          INTEGER, INTENT(IN) :: nogrp_


          LOGICAL :: tk

          INTEGER :: ub(3), lb(3)
! ...     ub(i), lb(i) upper and lower miller indexes

!
! ...     Plane Waves
!
        INTEGER, ALLOCATABLE :: stw(:,:)
! ...   stick map (wave functions), stw(i,j) = number of G-vector in the
! ...     stick whose x and y miller index are i and j

        INTEGER, ALLOCATABLE :: nstpw(:)
! ...   number of sticks (wave functions), nstpw(ip) = number of stick
! ...     for processor ip

        INTEGER, ALLOCATABLE :: sstpw(:)
! ...   number of G-vectors (wave functions), sstpw(ip) = sum of the
! ...     sticks length for processor ip = number of G-vectors
! ...     owned by the processor ip

        INTEGER :: nstw, nstpwx
! ...   nstw     local number of sticks (wave functions)
! ...   nstpwx   maximum among all processors of nstw

!
! ...     Potentials
!

        INTEGER, ALLOCATABLE :: st(:,:)
! ...   stick map (potentials), st(i,j) = number of G-vector in the
! ...     stick whose x and y miller index are i and j

        INTEGER, ALLOCATABLE :: nstp(:)
! ...   number of sticks (potentials), nstp(ip) = number of stick
! ...     for processor ip

        INTEGER, ALLOCATABLE :: sstp(:)
! ...   number of G-vectors (potentials), sstp(ip) = sum of the
! ...     sticks length for processor ip = number of G-vectors
! ...     owned by the processor ip

        INTEGER :: nst, nstpx
! ...   nst      local number of sticks (potentials)
! ...   nstpx    maximum among all processors of nst

!
! ...     Smooth Mesh
!

        INTEGER, ALLOCATABLE :: sts(:,:)
! ...   stick map (smooth mesh), sts(i,j) = number of G-vector in the
! ...     stick whose x and y miller index are i and j

        INTEGER, ALLOCATABLE :: nstps(:)
! ...   number of sticks (smooth mesh), nstp(ip) = number of stick
! ...     for processor ip

        INTEGER, ALLOCATABLE :: sstps(:)
! ...   number of G-vectors (smooth mesh), sstps(ip) = sum of the
! ...     sticks length for processor ip = number of G-vectors
! ...     owned by the processor ip

        INTEGER :: nsts
! ...   nsts      local number of sticks (smooth mesh)


        INTEGER, ALLOCATABLE :: ist(:,:)    ! sticks indices ordered
          INTEGER :: ip, ngm_ , ngs_
          INTEGER, ALLOCATABLE :: idx(:)

          tk    = .not. gamma_only
          ub(1) = ( dfftp%nr1 - 1 ) / 2
          ub(2) = ( dfftp%nr2 - 1 ) / 2
          ub(3) = ( dfftp%nr3 - 1 ) / 2
          lb    = - ub

          ! ...       Allocate maps

          ALLOCATE( stw ( lb(1):ub(1), lb(2):ub(2) ) )
          ALLOCATE( st  ( lb(1):ub(1), lb(2):ub(2) ) )
          ALLOCATE( sts ( lb(1):ub(1), lb(2):ub(2) ) )

          st  = 0
          stw = 0
          sts = 0

! ...       Fill in the stick maps, for given g-space base and cut-off

          CALL sticks_maps( tk, ub, lb, bg(:,1), bg(:,2), bg(:,3), &
                            gcut, gkcut, gcuts, st, stw, sts, mype, &
                            nproc, comm )

! ...       Now count the number of stick nst and nstw

          nst  = count( st  > 0 )
          nstw = count( stw > 0 )
          nsts = count( sts > 0 )

          ALLOCATE(ist(nst,5))

          ALLOCATE(nstp(nproc))
          ALLOCATE(sstp(nproc))

          ALLOCATE(nstpw(nproc))
          ALLOCATE(sstpw(nproc))

          ALLOCATE(nstps(nproc))
          ALLOCATE(sstps(nproc))

! ...       initialize the sticks indexes array ist

          CALL sticks_countg( tk, ub, lb, st, stw, sts, &
            ist(:,1), ist(:,2), ist(:,4), ist(:,3), ist(:,5) )

! ...       Sorts the sticks according to their length

          ALLOCATE( idx( nst ) )

          CALL sticks_sort( ist(:,4), ist(:,3), ist(:,5), nst, idx, nproc )

          ! ... Set as first stick the stick containing the G=0
          !
          !  DO iss = 1, nst
          !    IF( ist( idx( iss ), 1 ) == 0 .AND. ist( idx( iss ), 2 ) == 0 )  EXIT
          !  END DO
          !  itmp         = idx( 1 )
          !  idx( 1 )   = idx( iss )
          !  idx( iss ) = itmp

          CALL sticks_dist( tk, ub, lb, idx, ist(:,1), ist(:,2), ist(:,4), ist(:,3), ist(:,5), &
             nst, nstp, nstpw, nstps, sstp, sstpw, sstps, st, stw, sts, nproc )

          ngw = sstpw( mype + 1 )
          ngm = sstp( mype + 1 )
          ngs = sstps( mype + 1 )

          CALL sticks_pairup( tk, ub, lb, idx, ist(:,1), ist(:,2), ist(:,4), ist(:,3), ist(:,5), &
             nst, nstp, nstpw, nstps, sstp, sstpw, sstps, st, stw, sts, nproc )

          ! ...   Allocate and Set fft data layout descriptors


          CALL fft_dlay_allocate( dfftp, mype, root, nproc, comm, nogrp_ , dfftp%nr1x,  dfftp%nr2x )
          CALL fft_dlay_allocate( dffts, mype, root, nproc, comm, nogrp_ , dffts%nr1x, dffts%nr2x )

          CALL fft_dlay_set( dfftp, tk, nst, dfftp%nr1, dfftp%nr2, dfftp%nr3, dfftp%nr1x, dfftp%nr2x, dfftp%nr3x, &
            ub, lb, idx, ist(:,1), ist(:,2), nstp, nstpw, sstp, sstpw, st, stw )
          CALL fft_dlay_set( dffts, tk, nsts, dffts%nr1, dffts%nr2, dffts%nr3, dffts%nr1x, dffts%nr2x, dffts%nr3x, &
            ub, lb, idx, ist(:,1), ist(:,2), nstps, nstpw, sstps, sstpw, sts, stw )


! ...     Maximum number of sticks (potentials)
          nstpx  = maxval( nstp )
! ...     Maximum number of sticks (wave func.)
          nstpwx = maxval( nstpw  )

          IF( dffts%have_task_groups ) THEN
            !
            !  Initialize task groups.
            !  Note that this call modify dffts adding task group data.
            !
            CALL task_groups_init( dffts )
            !
          END IF

          IF (ionode) THEN
             WRITE( stdout,*)
             IF ( nproc > 1 ) THEN
                WRITE( stdout, '(5X,"Parallelization info")')
             ELSE
                WRITE( stdout, '(5X,"G-vector sticks info")')
             ENDIF
             WRITE( stdout, '(5X,"--------------------")')
             WRITE( stdout, '(5X,"sticks:   dense  smooth     PW", &
                            & 5X,"G-vecs:    dense   smooth      PW")') 
             IF ( nproc > 1 ) THEN
                WRITE( stdout,'(5X,"Min",4X,2I8,I7,12X,2I9,I8)') &
                   minval(nstp), minval(nstps), minval(nstpw), &
                   minval(sstp), minval(sstps), minval(sstpw)
                WRITE( stdout,'(5X,"Max",4X,2I8,I7,12X,2I9,I8)') &
                   maxval(nstp), maxval(nstps), maxval(nstpw), &
                   maxval(sstp), maxval(sstps), maxval(sstpw)
             END IF
             WRITE( stdout,'(5X,"Sum",4X,2I8,I7,12X,2I9,I8)') &
                sum(nstp), sum(nstps), sum(nstpw), &
                sum(sstp), sum(sstps), sum(sstpw)
             ! in the case k=0, the lines above and below differ:
             ! above all sticks, below only those in the half sphere
             IF ( .NOT. tk ) &
                 WRITE( stdout,'(5X,"Tot",4X,2I8,I7)') nst, nsts, nstw
          ENDIF

          DEALLOCATE( ist )
          DEALLOCATE( idx )

          DEALLOCATE( st, stw, sts )
          DEALLOCATE( sstp )
          DEALLOCATE( nstp )
          DEALLOCATE( sstpw )
          DEALLOCATE( nstpw )
          DEALLOCATE( sstps )
          DEALLOCATE( nstps )

          IF(ionode) WRITE( stdout,*)

          RETURN
        END SUBROUTINE pstickset

!----------------------------------------------------------------------

      SUBROUTINE pstickset_custom( gamma_only, bg, gcut, gkcut, &
          dfftp, ngw, ngm, mype, root, nproc, comm, nogrp_ )

          LOGICAL, INTENT(in) :: gamma_only
! ...     bg(:,1), bg(:,2), bg(:,3) reciprocal space base vectors.
          REAL(DP), INTENT(in) :: bg(3,3)
          REAL(DP), INTENT(in) :: gcut, gkcut
          TYPE(fft_dlay_descriptor), INTENT(inout) :: dfftp
          INTEGER, INTENT(out) :: ngw, ngm

          INTEGER, INTENT(IN) :: mype, root, nproc, comm
          INTEGER, INTENT(IN) :: nogrp_


          LOGICAL :: tk

          INTEGER :: ub(3), lb(3)
! ...     ub(i), lb(i) upper and lower miller indexes

!
! ...     Plane Waves
!
        INTEGER, ALLOCATABLE :: stw(:,:)
! ...   stick map (wave functions), stw(i,j) = number of G-vector in the
! ...     stick whose x and y miller index are i and j

        INTEGER, ALLOCATABLE :: nstpw(:)
! ...   number of sticks (wave functions), nstpw(ip) = number of stick
! ...     for processor ip

        INTEGER, ALLOCATABLE :: sstpw(:)
! ...   number of G-vectors (wave functions), sstpw(ip) = sum of the
! ...     sticks length for processor ip = number of G-vectors
! ...     owned by the processor ip

        INTEGER :: nstw, nstpwx
! ...   nstw     local number of sticks (wave functions)
! ...   nstpwx   maximum among all processors of nstw

!
! ...     Potentials
!

        INTEGER, ALLOCATABLE :: st(:,:)
! ...   stick map (potentials), st(i,j) = number of G-vector in the
! ...     stick whose x and y miller index are i and j

        INTEGER, ALLOCATABLE :: nstp(:)
! ...   number of sticks (potentials), nstp(ip) = number of stick
! ...     for processor ip

        INTEGER, ALLOCATABLE :: sstp(:)
! ...   number of G-vectors (potentials), sstp(ip) = sum of the
! ...     sticks length for processor ip = number of G-vectors
! ...     owned by the processor ip

        INTEGER :: nst, nstpx
! ...   nst      local number of sticks (potentials)
! ...   nstpx    maximum among all processors of nst

!
! ...     Smooth Mesh
!

        INTEGER, ALLOCATABLE :: sts(:,:)
! ...   stick map (smooth mesh), sts(i,j) = number of G-vector in the
! ...     stick whose x and y miller index are i and j

        INTEGER, ALLOCATABLE :: nstps(:)
! ...   number of sticks (smooth mesh), nstp(ip) = number of stick
! ...     for processor ip

        INTEGER, ALLOCATABLE :: sstps(:)
! ...   number of G-vectors (smooth mesh), sstps(ip) = sum of the
! ...     sticks length for processor ip = number of G-vectors
! ...     owned by the processor ip

        INTEGER :: nsts
! ...   nsts      local number of sticks (smooth mesh)


        INTEGER, ALLOCATABLE :: ist(:,:)    ! sticks indices ordered
          INTEGER :: ip, ngm_ , ngs_
          INTEGER, ALLOCATABLE :: idx(:)

          tk    = .not. gamma_only
          ub(1) = ( dfftp%nr1 - 1 ) / 2
          ub(2) = ( dfftp%nr2 - 1 ) / 2
          ub(3) = ( dfftp%nr3 - 1 ) / 2
          lb    = - ub

          ! ...       Allocate maps

          ALLOCATE( stw ( lb(1):ub(1), lb(2):ub(2) ) )
          ALLOCATE( st  ( lb(1):ub(1), lb(2):ub(2) ) )
          ALLOCATE( sts ( lb(1):ub(1), lb(2):ub(2) ) )

          st  = 0
          stw = 0
          sts = 0

! ...       Fill in the stick maps, for given g-space base and cut-off

          CALL sticks_maps( tk, ub, lb, bg(:,1), bg(:,2), bg(:,3), &
                            gcut, gkcut, gcut, st, stw, sts, mype, &
                            nproc, comm )

! ...       Now count the number of stick nst and nstw

          nst  = count( st  > 0 )
          nstw = count( stw > 0 )
          nsts = count( sts > 0 )

          ALLOCATE(ist(nst,5))

          ALLOCATE(nstp(nproc))
          ALLOCATE(sstp(nproc))

          ALLOCATE(nstpw(nproc))
          ALLOCATE(sstpw(nproc))

          ALLOCATE(nstps(nproc))
          ALLOCATE(sstps(nproc))

! ...       initialize the sticks indexes array ist

          CALL sticks_countg( tk, ub, lb, st, stw, sts, &
            ist(:,1), ist(:,2), ist(:,4), ist(:,3), ist(:,5) )

! ...       Sorts the sticks according to their length

          ALLOCATE( idx( nst ) )

          CALL sticks_sort( ist(:,4), ist(:,3), ist(:,5), nst, idx, nproc )

          ! ... Set as first stick the stick containing the G=0
          !
          !  DO iss = 1, nst
          !    IF( ist( idx( iss ), 1 ) == 0 .AND. ist( idx( iss ), 2 ) == 0 )  EXIT
          !  END DO
          !  itmp         = idx( 1 )
          !  idx( 1 )   = idx( iss )
          !  idx( iss ) = itmp

          CALL sticks_dist( tk, ub, lb, idx, ist(:,1), ist(:,2), ist(:,4), ist(:,3), ist(:,5), &
             nst, nstp, nstpw, nstps, sstp, sstpw, sstps, st, stw, sts, nproc )

          ngw = sstpw( mype + 1 )
          ngm = sstp( mype + 1 )
!          ngs = sstps( mype + 1 )

          CALL sticks_pairup( tk, ub, lb, idx, ist(:,1), ist(:,2), ist(:,4), ist(:,3), ist(:,5), &
             nst, nstp, nstpw, nstps, sstp, sstpw, sstps, st, stw, sts, nproc )

          ! ...   Allocate and Set fft data layout descriptors


          CALL fft_dlay_allocate( dfftp, mype, root, nproc, comm, nogrp_ , dfftp%nr1x,  dfftp%nr2x )
!          CALL fft_dlay_allocate( dffts, mype, root, nproc, comm, nogrp_ , dffts%nr1x, dffts%nr2x )

          CALL fft_dlay_set( dfftp, tk, nst, dfftp%nr1, dfftp%nr2, dfftp%nr3, dfftp%nr1x, dfftp%nr2x, dfftp%nr3x, &
            ub, lb, idx, ist(:,1), ist(:,2), nstp, nstpw, sstp, sstpw, st, stw )
!          CALL fft_dlay_set( dffts, tk, nsts, dffts%nr1, dffts%nr2, dffts%nr3, dffts%nr1x, dffts%nr2x, dffts%nr3x, &
!            ub, lb, idx, ist(:,1), ist(:,2), nstps, nstpw, sstps, sstpw, sts, stw )


! ...     Maximum number of sticks (potentials)
          nstpx  = maxval( nstp )
! ...     Maximum number of sticks (wave func.)
          nstpwx = maxval( nstpw  )

!          IF( dffts%have_task_groups ) THEN
            !
            !  Initialize task groups.
            !  Note that this call modify dffts adding task group data.
            !
!            CALL task_groups_init( dffts )
            !
!          END IF

!!$          IF (ionode) THEN
!!$             WRITE( stdout,*)
!!$             IF ( nproc > 1 ) THEN
!!$                WRITE( stdout, '(5X,"Parallelization info")')
!!$             ELSE
!!$                WRITE( stdout, '(5X,"G-vector sticks info")')
!!$             ENDIF
!!$             WRITE( stdout, '(5X,"--------------------")')
!!$             WRITE( stdout, '(5X,"sticks:   dense  smooth     PW", &
!!$                            & 5X,"G-vecs:    dense   smooth      PW")') 
!!$             IF ( nproc > 1 ) THEN
!!$                WRITE( stdout,'(5X,"Min",4X,2I8,I7,12X,2I9,I8)') &
!!$                   minval(nstp), minval(nstps), minval(nstpw), &
!!$                   minval(sstp), minval(sstps), minval(sstpw)
!!$                WRITE( stdout,'(5X,"Max",4X,2I8,I7,12X,2I9,I8)') &
!!$                   maxval(nstp), maxval(nstps), maxval(nstpw), &
!!$                   maxval(sstp), maxval(sstps), maxval(sstpw)
!!$             END IF
!!$             WRITE( stdout,'(5X,"Sum",4X,2I8,I7,12X,2I9,I8)') &
!!$                sum(nstp), sum(nstps), sum(nstpw), &
!!$                sum(sstp), sum(sstps), sum(sstpw)
!!$             ! in the case k=0, the lines above and below differ:
!!$             ! above all sticks, below only those in the half sphere
!!$             IF ( .NOT. tk ) &
!!$                 WRITE( stdout,'(5X,"Tot",4X,2I8,I7)') nst, nsts, nstw
!!$          ENDIF

          DEALLOCATE( ist )
          DEALLOCATE( idx )

          DEALLOCATE( st, stw, sts )
          DEALLOCATE( sstp )
          DEALLOCATE( nstp )
          DEALLOCATE( sstpw )
          DEALLOCATE( nstpw )
          DEALLOCATE( sstps )
          DEALLOCATE( nstps )

          IF(ionode) WRITE( stdout,*)

          RETURN
        END SUBROUTINE pstickset_custom

!-----------------------------------------
! Task groups Contributed by C. Bekas, October 2005
! Revised by C. Cavazzoni
!--------------------------------------------

SUBROUTINE task_groups_init( dffts )

   USE parallel_include
   !
   USE io_global,      ONLY : stdout
   USE fft_types,      ONLY : fft_dlay_descriptor

   ! T.G.
   ! NPGRP:      Number of processors per group
   ! NOGRP:      Number of processors per orbital task group

   IMPLICIT NONE

   TYPE(fft_dlay_descriptor), INTENT(inout) :: dffts

   !----------------------------------
   !Local Variables declaration
   !----------------------------------

   INTEGER  :: I
   INTEGER  :: IERR
   INTEGER  :: num_planes, num_sticks
   INTEGER  :: nnrsx_vec ( dffts%nproc )
   INTEGER  :: pgroup( dffts%nproc )
   INTEGER  :: strd

   CALL task_groups_init_first( dffts )
   !
   IF ( dffts%nogrp > 1 ) WRITE( stdout, 100 ) dffts%nogrp, dffts%npgrp

100 FORMAT( /,3X,'Task Groups are in USE',/,3X,'groups and procs/group : ',I5,I5 )

   !Find maximum chunk of local data concerning coefficients of eigenfunctions in g-space

   CALL MPI_Allgather( dffts%nnr, 1, MPI_INTEGER, nnrsx_vec, 1, MPI_INTEGER, dffts%comm, IERR)
   strd = maxval( nnrsx_vec( 1:dffts%nproc ) )

   IF( strd /= dffts%tg_nnr ) CALL errore( ' task_groups_init ', ' inconsistent nnr ', 1 )

   !-------------------------------------------------------------------------------------
   !C. Bekas...TASK GROUP RELATED. FFT DATA STRUCTURES ARE ALREADY DEFINED ABOVE
   !-------------------------------------------------------------------------------------
   !dfft%nsw(me) holds the number of z-sticks for the current processor per wave-function
   !We can either send these in the group with an mpi_allgather...or put the
   !in the PSIS vector (in special positions) and send them with them.
   !Otherwise we can do this once at the beginning, before the loop.
   !we choose to do the latter one.
   !-------------------------------------------------------------------------------------
   !
   !
   ALLOCATE( dffts%tg_nsw(dffts%nproc))
   ALLOCATE( dffts%tg_npp(dffts%nproc))

   num_sticks = 0
   num_planes = 0
   DO i = 1, dffts%nogrp
      num_sticks = num_sticks + dffts%nsw( dffts%nolist(i) + 1 )
      num_planes = num_planes + dffts%npp( dffts%nolist(i) + 1 )
   ENDDO

   CALL MPI_ALLGATHER(num_sticks, 1, MPI_INTEGER, dffts%tg_nsw(1), 1, MPI_INTEGER, dffts%comm, IERR)
   CALL MPI_ALLGATHER(num_planes, 1, MPI_INTEGER, dffts%tg_npp(1), 1, MPI_INTEGER, dffts%comm, IERR)

   ALLOCATE( dffts%tg_snd( dffts%nogrp ) )
   ALLOCATE( dffts%tg_rcv( dffts%nogrp ) )
   ALLOCATE( dffts%tg_psdsp( dffts%nogrp ) )
   ALLOCATE( dffts%tg_usdsp( dffts%nogrp ) )
   ALLOCATE( dffts%tg_rdsp( dffts%nogrp ) )

   dffts%tg_snd(1)   = dffts%nr3x * dffts%nsw( dffts%mype + 1 )
   IF( dffts%nr3x * dffts%nsw( dffts%mype + 1 ) > dffts%tg_nnr ) THEN
      CALL errore( ' task_groups_init ', ' inconsistent dffts%tg_nnr ', 1 )
   ENDIF
   dffts%tg_psdsp(1) = 0
   dffts%tg_usdsp(1) = 0
   dffts%tg_rcv(1)  = dffts%nr3x * dffts%nsw( dffts%nolist(1) + 1 )
   dffts%tg_rdsp(1) = 0
   DO i = 2, dffts%nogrp
      dffts%tg_snd(i)  = dffts%nr3x * dffts%nsw( dffts%mype + 1 )
      dffts%tg_psdsp(i) = dffts%tg_psdsp(i-1) + dffts%tg_nnr
      dffts%tg_usdsp(i) = dffts%tg_usdsp(i-1) + dffts%tg_snd(i-1)
      dffts%tg_rcv(i)  = dffts%nr3x * dffts%nsw( dffts%nolist(i) + 1 )
      dffts%tg_rdsp(i) = dffts%tg_rdsp(i-1) + dffts%tg_rcv(i-1)
   ENDDO

   RETURN

END SUBROUTINE task_groups_init


  !
SUBROUTINE task_groups_init_first( dffts )

   USE parallel_include
   !
   USE fft_types,      ONLY : fft_dlay_descriptor
   !
   IMPLICIT NONE
   !
   TYPE(fft_dlay_descriptor), INTENT(inout) :: dffts
    !
    INTEGER :: i, n1, ipos, color, key, ierr, itsk, ntsk
    INTEGER :: pgroup( dffts%nproc )
    !
    !SUBDIVIDE THE PROCESSORS IN GROUPS
    !
    DO i = 1, dffts%nproc
       pgroup( i ) = i - 1
    ENDDO
    !
    !LIST OF PROCESSORS IN MY ORBITAL GROUP
    !
    !  processors in these group have contiguous indexes
    !
    n1 = ( dffts%mype / dffts%nogrp ) * dffts%nogrp - 1
    DO i = 1, dffts%nogrp
       dffts%nolist( i ) = pgroup( n1 + i + 1 )
       IF( dffts%mype == dffts%nolist( i ) ) ipos = i - 1
    ENDDO
    !
    !LIST OF PROCESSORS IN MY PLANE WAVE GROUP
    !
    DO I = 1, dffts%npgrp
       dffts%nplist( i ) = pgroup( ipos + ( i - 1 ) * dffts%nogrp + 1 )
    ENDDO
    !
    !SET UP THE GROUPS
    !
    !
    !CREATE ORBITAL GROUPS
    !
    color = dffts%mype / dffts%nogrp
    key   = MOD( dffts%mype , dffts%nogrp )
    CALL MPI_COMM_SPLIT( dffts%comm, color, key, dffts%ogrp_comm, ierr )
    if( ierr /= 0 ) &
         CALL errore( ' init_task_groups ', ' creating ogrp_comm ', ABS(ierr) )
    CALL MPI_COMM_RANK( dffts%ogrp_comm, itsk, IERR )
    CALL MPI_COMM_SIZE( dffts%ogrp_comm, ntsk, IERR )
    IF( dffts%nogrp /= ntsk ) CALL errore( ' init_task_groups ', ' ogrp_comm size ', ntsk )
    DO i = 1, dffts%nogrp
       IF( dffts%mype == dffts%nolist( i ) ) THEN
          IF( (i-1) /= itsk ) CALL errore( ' init_task_groups ', ' ogrp_comm rank ', itsk )
       END IF
    END DO
    !
    !CREATE PLANEWAVE GROUPS
    !
    color = MOD( dffts%mype , dffts%nogrp )
    key   = dffts%mype / dffts%nogrp
    CALL MPI_COMM_SPLIT( dffts%comm, color, key, dffts%pgrp_comm, ierr )
    if( ierr /= 0 ) &
         CALL errore( ' init_task_groups ', ' creating pgrp_comm ', ABS(ierr) )
    CALL MPI_COMM_RANK( dffts%pgrp_comm, itsk, IERR )
    CALL MPI_COMM_SIZE( dffts%pgrp_comm, ntsk, IERR )
    IF( dffts%npgrp /= ntsk ) CALL errore( ' init_task_groups ', ' pgrp_comm size ', ntsk )
    DO i = 1, dffts%npgrp
       IF( dffts%mype == dffts%nplist( i ) ) THEN
          IF( (i-1) /= itsk ) CALL errore( ' init_task_groups ', ' pgrp_comm rank ', itsk )
       END IF
    END DO
    dffts%me_pgrp = itsk

    RETURN
  END SUBROUTINE task_groups_init_first
  !
!=----------------------------------------------------------------------=
   END MODULE stick_set
!=----------------------------------------------------------------------=
