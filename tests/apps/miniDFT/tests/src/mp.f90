!
! Copyright (C) 2002-2009 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

#if defined __HPM
#  include "/cineca/prod/hpm/include/f_hpm.h"
#endif

!------------------------------------------------------------------------------!
    MODULE mp
!------------------------------------------------------------------------------!
      USE kinds,     ONLY : DP
      USE io_global, ONLY : stdout
      USE parallel_include
      !
      IMPLICIT NONE

      PUBLIC :: mp_start, mp_end, &
        mp_bcast, mp_sum, mp_max, mp_min, mp_rank, mp_size, &
        mp_gather, mp_get, mp_put, mp_barrier, mp_report, mp_group_free, &
        mp_root_sum, mp_comm_free, mp_comm_create, mp_comm_group, &
        mp_group_create, mp_comm_split, mp_set_displs
!
      INTERFACE mp_bcast
        MODULE PROCEDURE mp_bcast_i1, mp_bcast_r1, mp_bcast_c1, &
          mp_bcast_z, mp_bcast_zv, &
          mp_bcast_iv, mp_bcast_rv, mp_bcast_cv, mp_bcast_l, mp_bcast_rm, &
          mp_bcast_cm, mp_bcast_im, mp_bcast_it, mp_bcast_rt, mp_bcast_lv, &
          mp_bcast_lm, mp_bcast_r4d, mp_bcast_r5d, mp_bcast_ct,  mp_bcast_c4d,&
          mp_bcast_c5d
      END INTERFACE

      INTERFACE mp_sum
        MODULE PROCEDURE mp_sum_i1, mp_sum_iv, mp_sum_im, mp_sum_it, &
          mp_sum_r1, mp_sum_rv, mp_sum_rm, mp_sum_rt, mp_sum_r4d, &
          mp_sum_c1, mp_sum_cv, mp_sum_cm, mp_sum_ct, mp_sum_c4d, &
          mp_sum_c5d, mp_sum_c6d, mp_sum_rmm, mp_sum_cmm, mp_sum_r5d
      END INTERFACE

      INTERFACE mp_root_sum
        MODULE PROCEDURE mp_root_sum_rm, mp_root_sum_cm
      END INTERFACE

      INTERFACE mp_get
        MODULE PROCEDURE mp_get_r1, mp_get_rv, mp_get_cv, mp_get_i1, mp_get_iv, &
          mp_get_rm
      END INTERFACE

      INTERFACE mp_put
        MODULE PROCEDURE mp_put_rv, mp_put_cv, mp_put_i1, mp_put_iv, &
          mp_put_rm
      END INTERFACE

      INTERFACE mp_max
        MODULE PROCEDURE mp_max_i, mp_max_r, mp_max_rv, mp_max_iv
      END INTERFACE
      INTERFACE mp_min
        MODULE PROCEDURE mp_min_i, mp_min_r, mp_min_rv, mp_min_iv
      END INTERFACE
      INTERFACE mp_gather
        MODULE PROCEDURE mp_gather_i1, mp_gather_iv, mp_gatherv_rv, mp_gatherv_iv, &
          mp_gatherv_rm, mp_gatherv_im, mp_gatherv_cv
      END INTERFACE
      INTERFACE mp_alltoall
        MODULE PROCEDURE mp_alltoall_c3d, mp_alltoall_i3d
      END INTERFACE
      INTERFACE mp_circular_shift_left
        MODULE PROCEDURE mp_circular_shift_left_d2d_int,mp_circular_shift_left_d2d_double
      END INTERFACE


      CHARACTER(LEN=80), PRIVATE :: err_msg = ' '

!------------------------------------------------------------------------------!
!
    CONTAINS
!
!------------------------------------------------------------------------------!
!
!------------------------------------------------------------------------------!
!..mp_gather_i1
      SUBROUTINE mp_gather_i1(mydata, alldata, root, gid)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: mydata, root
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER, INTENT(OUT) :: alldata(:)
        INTEGER :: ierr


!!$ #if defined (__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL MPI_GATHER(mydata, 1, MPI_INTEGER, alldata, 1, MPI_INTEGER, root, group, IERR)
        IF (ierr/=0) CALL mp_stop( 8001 )
!!$#else
!!$        alldata(1) = mydata
!!$#endif
        RETURN
      END SUBROUTINE mp_gather_i1

!------------------------------------------------------------------------------!
!..mp_gather_iv
!..Carlo Cavazzoni
      SUBROUTINE mp_gather_iv(mydata, alldata, root, gid)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: mydata(:), root
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER, INTENT(OUT) :: alldata(:,:)
        INTEGER :: msglen, ierr


!!$#if defined (__MPI)
        msglen = SIZE(mydata)
        IF( msglen .NE. SIZE(alldata, 1) ) CALL mp_stop( 8000 )
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL MPI_GATHER(mydata, msglen, MPI_INTEGER, alldata, msglen, MPI_INTEGER, root, group, IERR)
        IF (ierr/=0) CALL mp_stop( 8001 )
!!$#else
!!$        msglen = SIZE(mydata)
!!$        IF( msglen .NE. SIZE(alldata, 1) ) CALL mp_stop( 8002 )
!!$        alldata(:,1) = mydata(:)
!!$#endif
        RETURN
      END SUBROUTINE mp_gather_iv

!
!------------------------------------------------------------------------------!
!..mp_start
      SUBROUTINE mp_start(numtask, taskid, groupid)

! ...
        IMPLICIT NONE
        INTEGER, INTENT (OUT) :: numtask, taskid, groupid
        INTEGER :: ierr
! ...
        ierr = 0
        numtask = 1
        taskid = 0
        groupid = 0

#  if defined(__MPI)
        CALL mpi_init(ierr)
        IF (ierr/=0) CALL mp_stop( 8003 )
        CALL mpi_comm_rank(mpi_comm_world,taskid,ierr)
        IF (ierr/=0) CALL mp_stop( 8005 )
#if defined __HPM
        !   initialize the IBM Harware performance monitor
        CALL f_hpminit( taskid, 'profiling' )
#endif
        CALL mpi_comm_size(mpi_comm_world,numtask,ierr)
        groupid = mpi_comm_world
        IF (ierr/=0) CALL mp_stop( 8006 )
#  endif

        RETURN
      END SUBROUTINE mp_start
!
!------------------------------------------------------------------------------!
!..mp_end

      SUBROUTINE mp_end
        IMPLICIT NONE
        INTEGER :: ierr, taskid

        ierr = 0
        taskid = 0

#if defined __HPM

        !   terminate the IBM Harware performance monitor

#if defined(__MPI)
        CALL mpi_comm_rank( mpi_comm_world, taskid, ierr)
#endif
        CALL f_hpmterminate( taskid )
#endif

#if defined(__MPI)
        CALL mpi_finalize(ierr)
        IF (ierr/=0) CALL mp_stop( 8004 )
#endif
        RETURN
      END SUBROUTINE mp_end

!------------------------------------------------------------------------------!
!..mp_group

      SUBROUTINE mp_comm_group( comm, group )
         IMPLICIT NONE
         INTEGER, INTENT (IN) :: comm
         INTEGER, INTENT (OUT) :: group
         INTEGER :: ierr
         ierr = 0
#if defined(__MPI)
         CALL mpi_comm_group( comm, group, ierr )
         IF (ierr/=0) CALL mp_stop( 8007 )
#else
         group = 0
#endif
      END SUBROUTINE  mp_comm_group

      SUBROUTINE mp_comm_split( old_comm, color, key, new_comm )
         IMPLICIT NONE
         INTEGER, INTENT (IN) :: old_comm
         INTEGER, INTENT (IN) :: color, key
         INTEGER, INTENT (OUT) :: new_comm
         INTEGER :: ierr
         ierr = 0
#if defined(__MPI)
         CALL MPI_COMM_SPLIT( old_comm, color, key, new_comm, ierr )
         IF (ierr/=0) CALL mp_stop( 8008 )
#else
         new_comm = old_comm
#endif
      END SUBROUTINE  mp_comm_split


      SUBROUTINE mp_group_create( group_list, group_size, old_grp, new_grp )
        IMPLICIT NONE
        INTEGER, INTENT (IN) :: group_list(:), group_size, old_grp
        INTEGER, INTENT (OUT) :: new_grp
        INTEGER :: ierr

        ierr = 0
        new_grp = old_grp
#if defined(__MPI)
        CALL mpi_group_incl( old_grp, group_size, group_list, new_grp, ierr )
        IF (ierr/=0) CALL mp_stop( 8009 )
#endif
      END SUBROUTINE mp_group_create

!------------------------------------------------------------------------------!
      SUBROUTINE mp_comm_create( old_comm, new_grp, new_comm )
        IMPLICIT NONE
        INTEGER, INTENT (IN) :: old_comm
        INTEGER, INTENT (IN) :: new_grp
        INTEGER, INTENT (OUT) :: new_comm
        INTEGER :: ierr

        ierr = 0
        new_comm = old_comm
#if defined(__MPI)
        CALL mpi_comm_create( old_comm, new_grp, new_comm, ierr )
        IF (ierr/=0) CALL mp_stop( 8010 )
#endif
      END SUBROUTINE mp_comm_create

!------------------------------------------------------------------------------!
!..mp_group_free
      SUBROUTINE mp_group_free( group )
        IMPLICIT NONE
        INTEGER, INTENT (INOUT) :: group
        INTEGER :: ierr
        ierr = 0
#if defined(__MPI)
        CALL mpi_group_free( group, ierr )
        IF (ierr/=0) CALL mp_stop( 8011 )
#endif
      END SUBROUTINE mp_group_free
!------------------------------------------------------------------------------!

      SUBROUTINE mp_comm_free( comm )
         IMPLICIT NONE
         INTEGER, INTENT (INOUT) :: comm
         INTEGER :: ierr
         ierr = 0
#if defined(__MPI)
         IF( comm /= MPI_COMM_NULL ) THEN
            CALL mpi_comm_free( comm, ierr )
            IF (ierr/=0) CALL mp_stop( 8012 )
         END IF
#endif
         RETURN
      END SUBROUTINE mp_comm_free

!------------------------------------------------------------------------------!
!..mp_bcast

      SUBROUTINE mp_bcast_i1(msg,source,gid)
        IMPLICIT NONE
        INTEGER :: msg
        INTEGER :: source
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen

#if defined(__MPI)
        msglen = 1
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL BCAST_INTEGER( msg, msglen, source, group )
#endif
      END SUBROUTINE mp_bcast_i1
!
!------------------------------------------------------------------------------!
      SUBROUTINE mp_bcast_iv(msg,source,gid)
        IMPLICIT NONE
        INTEGER :: msg(:)
        INTEGER :: source
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL BCAST_INTEGER( msg, msglen, source, group )
#endif
      END SUBROUTINE mp_bcast_iv
!
!------------------------------------------------------------------------------!
      SUBROUTINE mp_bcast_im( msg, source, gid )
        IMPLICIT NONE
        INTEGER :: msg(:,:)
        INTEGER :: source
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL BCAST_INTEGER( msg, msglen, source, group )
#endif
      END SUBROUTINE mp_bcast_im
!
!------------------------------------------------------------------------------!
!
! Carlo Cavazzoni
!
      SUBROUTINE mp_bcast_it(msg,source,gid)
        IMPLICIT NONE
        INTEGER :: msg(:,:,:)
        INTEGER :: source
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL BCAST_INTEGER( msg, msglen, source, group )
#endif
      END SUBROUTINE mp_bcast_it
!
!------------------------------------------------------------------------------!
!
      SUBROUTINE mp_bcast_r1(msg,source,gid)
        IMPLICIT NONE
        REAL (DP) :: msg
        INTEGER :: msglen, source
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
#if defined(__MPI)
        msglen = 1
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL bcast_real( msg, msglen, source, group )
#endif
      END SUBROUTINE mp_bcast_r1
!
!------------------------------------------------------------------------------!
!
      SUBROUTINE mp_bcast_rv(msg,source,gid)
        IMPLICIT NONE
        REAL (DP) :: msg(:)
        INTEGER :: source
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen

#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL bcast_real( msg, msglen, source, group )
#endif
      END SUBROUTINE mp_bcast_rv
!
!------------------------------------------------------------------------------!
!
      SUBROUTINE mp_bcast_rm(msg,source,gid)
        IMPLICIT NONE
        REAL (DP) :: msg(:,:)
        INTEGER :: source
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL bcast_real( msg, msglen, source, group )
#endif
      END SUBROUTINE mp_bcast_rm
!
!------------------------------------------------------------------------------!
!
! Carlo Cavazzoni
!
      SUBROUTINE mp_bcast_rt(msg,source,gid)
        IMPLICIT NONE
        REAL (DP) :: msg(:,:,:)
        INTEGER :: source
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL bcast_real( msg, msglen, source, group )
#endif
      END SUBROUTINE mp_bcast_rt
!
!------------------------------------------------------------------------------!
!
! Carlo Cavazzoni
!
      SUBROUTINE mp_bcast_r4d(msg, source, gid)
        IMPLICIT NONE
        REAL (DP) :: msg(:,:,:,:)
        INTEGER :: source
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL bcast_real( msg, msglen, source, group )
#endif
      END SUBROUTINE mp_bcast_r4d

!
!------------------------------------------------------------------------------!
!
! Carlo Cavazzoni
!
      SUBROUTINE mp_bcast_r5d(msg, source, gid)
        IMPLICIT NONE
        REAL (DP) :: msg(:,:,:,:,:)
        INTEGER :: source
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL bcast_real( msg, msglen, source, group )
#endif
      END SUBROUTINE mp_bcast_r5d

!------------------------------------------------------------------------------!
!
      SUBROUTINE mp_bcast_c1(msg,source,gid)
        IMPLICIT NONE
        COMPLEX (DP) :: msg
        INTEGER :: source
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = 1
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL bcast_real( msg, 2 * msglen, source, group )
#endif
      END SUBROUTINE mp_bcast_c1
!
!------------------------------------------------------------------------------!
      SUBROUTINE mp_bcast_cv(msg,source,gid)
        IMPLICIT NONE
        COMPLEX (DP) :: msg(:)
        INTEGER :: source
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL bcast_real( msg, 2 * msglen, source, group )
#endif
      END SUBROUTINE mp_bcast_cv
!
!------------------------------------------------------------------------------!
      SUBROUTINE mp_bcast_cm(msg,source,gid)
        IMPLICIT NONE
        COMPLEX (DP) :: msg(:,:)
        INTEGER :: source
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL bcast_real( msg, 2 * msglen, source, group )
#endif
      END SUBROUTINE mp_bcast_cm
!
!------------------------------------------------------------------------------!
      SUBROUTINE mp_bcast_ct(msg,source,gid)
        IMPLICIT NONE
        COMPLEX (DP) :: msg(:,:,:)
        INTEGER :: source
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL bcast_real( msg, 2 * msglen, source, group )
#endif
      END SUBROUTINE mp_bcast_ct

!
!------------------------------------------------------------------------------!
      SUBROUTINE mp_bcast_c4d(msg,source,gid)
        IMPLICIT NONE
        COMPLEX (DP) :: msg(:,:,:,:)
        INTEGER :: source
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL bcast_real( msg, 2 * msglen, source, group )
#endif
      END SUBROUTINE mp_bcast_c4d

      SUBROUTINE mp_bcast_c5d(msg,source,gid)
        IMPLICIT NONE
        COMPLEX (DP) :: msg(:,:,:,:,:)
        INTEGER :: source
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL bcast_real( msg, 2 * msglen, source, group )
#endif
      END SUBROUTINE mp_bcast_c5d

!
!------------------------------------------------------------------------------!

      SUBROUTINE mp_bcast_l(msg,source,gid)
        IMPLICIT NONE
        LOGICAL :: msg
        INTEGER :: source
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = 1
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL bcast_logical( msg, msglen, source, group )
#endif
      END SUBROUTINE mp_bcast_l
!
!------------------------------------------------------------------------------!
!
! Carlo Cavazzoni
!
      SUBROUTINE mp_bcast_lv(msg,source,gid)
        IMPLICIT NONE
        LOGICAL :: msg(:)
        INTEGER :: source
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL bcast_logical( msg, msglen, source, group )
#endif
      END SUBROUTINE mp_bcast_lv

!------------------------------------------------------------------------------!
!
! Carlo Cavazzoni
!
      SUBROUTINE mp_bcast_lm(msg,source,gid)
        IMPLICIT NONE
        LOGICAL :: msg(:,:)
        INTEGER :: source
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL bcast_logical( msg, msglen, source, group )
#endif
      END SUBROUTINE mp_bcast_lm


!
!------------------------------------------------------------------------------!
!
      SUBROUTINE mp_bcast_z(msg,source,gid)
        IMPLICIT NONE
        CHARACTER (len=*) :: msg
        INTEGER :: source
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen, ierr, i
        INTEGER, ALLOCATABLE :: imsg(:)
#if defined(__MPI)
        ierr = 0
        msglen = len(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        IF (ierr/=0) CALL mp_stop( 8014 )
        ALLOCATE (imsg(1:msglen), STAT=ierr)
        IF (ierr/=0) CALL mp_stop( 8015 )
        DO i = 1, msglen
          imsg(i) = ichar(msg(i:i))
        END DO
        CALL bcast_integer( imsg, msglen, source, group )
        DO i = 1, msglen
          msg(i:i) = char(imsg(i))
        END DO
        DEALLOCATE (imsg, STAT=ierr)
        IF (ierr/=0) CALL mp_stop( 8016 )
#endif
      END SUBROUTINE mp_bcast_z
!
!------------------------------------------------------------------------------!
!
!------------------------------------------------------------------------------!
!
      SUBROUTINE mp_bcast_zv(msg,source,gid)
        IMPLICIT NONE
        CHARACTER (len=*) :: msg(:)
        INTEGER :: source
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen, m1, m2, ierr, i, j
        INTEGER, ALLOCATABLE :: imsg(:,:)
#if defined(__MPI)
        ierr = 0
        m1 = LEN(msg)
        m2 = SIZE(msg)
        msglen = LEN(msg)*SIZE(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        ALLOCATE (imsg(1:m1,1:m2), STAT=ierr)
        IF (ierr/=0) CALL mp_stop( 8017 )
        DO j = 1, m2
          DO i = 1, m1
            imsg(i,j) = ichar(msg(j)(i:i))
          END DO
        END DO
        CALL bcast_integer( imsg, msglen, source, group )
        DO j = 1, m2
          DO i = 1, m1
            msg(j)(i:i) = char(imsg(i,j))
          END DO
        END DO
        DEALLOCATE (imsg, STAT=ierr)
        IF (ierr/=0) CALL mp_stop( 8018 )
#endif
      END SUBROUTINE mp_bcast_zv
!
!------------------------------------------------------------------------------!
!
! Carlo Cavazzoni
!
      SUBROUTINE mp_get_i1(msg_dest, msg_sour, mpime, dest, sour, ip, gid)
        INTEGER :: msg_dest, msg_sour
        INTEGER, INTENT(IN) :: dest, sour, ip, mpime
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
#if defined(__MPI)
        INTEGER :: istatus(MPI_STATUS_SIZE)
#endif
        INTEGER :: ierr, nrcv
        INTEGER :: msglen = 1

#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
#endif

        ! processors not taking part in the communication have 0 length message

        msglen = 0

        IF(dest .NE. sour) THEN
#if defined(__MPI)
           IF(mpime .EQ. sour) THEN
             msglen=1
             CALL MPI_SEND( msg_sour, msglen, MPI_INTEGER, dest, ip, group, ierr)
             IF (ierr/=0) CALL mp_stop( 8019 )
           ELSE IF(mpime .EQ. dest) THEN
             msglen=1
             CALL MPI_RECV( msg_dest, msglen, MPI_INTEGER, sour, ip, group, istatus, IERR )
             IF (ierr/=0) CALL mp_stop( 8020 )
             CALL MPI_GET_COUNT(istatus, MPI_INTEGER, nrcv, ierr)
             IF (ierr/=0) CALL mp_stop( 8021 )
           END IF
#endif
        ELSEIF(mpime .EQ. sour)THEN
          msg_dest = msg_sour
          msglen = 1
        END IF

#if defined(__MPI)
        CALL MPI_BARRIER(group, IERR)
        IF (ierr/=0) CALL mp_stop( 8022 )
#endif


        RETURN
      END SUBROUTINE mp_get_i1

!------------------------------------------------------------------------------!
!
! Carlo Cavazzoni
!
      SUBROUTINE mp_get_iv(msg_dest, msg_sour, mpime, dest, sour, ip, gid)
        INTEGER :: msg_dest(:), msg_sour(:)
        INTEGER, INTENT(IN) :: dest, sour, ip, mpime
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
#if defined(__MPI)
        INTEGER :: istatus(MPI_STATUS_SIZE)
#endif
        INTEGER :: ierr, nrcv
        INTEGER :: msglen

#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
#endif

        ! processors not taking part in the communication have 0 length message

        msglen = 0

        IF(sour .NE. dest) THEN
#if defined(__MPI)
           IF(mpime .EQ. sour) THEN
             msglen = SIZE(msg_sour)
             CALL MPI_SEND( msg_sour, SIZE(msg_sour), MPI_INTEGER, dest, ip, group, ierr)
             IF (ierr/=0) CALL mp_stop( 8023 )
           ELSE IF(mpime .EQ. dest) THEN
             CALL MPI_RECV( msg_dest, SIZE(msg_dest), MPI_INTEGER, sour, ip, group, istatus, IERR )
             IF (ierr/=0) CALL mp_stop( 8024 )
             CALL MPI_GET_COUNT(istatus, MPI_INTEGER, nrcv, ierr)
             IF (ierr/=0) CALL mp_stop( 8025 )
             msglen = nrcv
           END IF
#endif
        ELSEIF(mpime .EQ. sour)THEN
          msg_dest(1:SIZE(msg_sour)) = msg_sour(:)
          msglen = SIZE(msg_sour)
        END IF
#if defined(__MPI)
        CALL MPI_BARRIER(group, IERR)
        IF (ierr/=0) CALL mp_stop( 8026 )
#endif
        RETURN
      END SUBROUTINE mp_get_iv

!------------------------------------------------------------------------------!

      SUBROUTINE mp_get_r1(msg_dest, msg_sour, mpime, dest, sour, ip, gid)
        REAL (DP) :: msg_dest, msg_sour
        INTEGER, INTENT(IN) :: dest, sour, ip, mpime
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
#if defined(__MPI)
        INTEGER :: istatus(MPI_STATUS_SIZE)
#endif
        INTEGER :: ierr, nrcv
        INTEGER :: msglen

#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
#endif

        ! processors not taking part in the communication have 0 length message

        msglen = 0

        IF(sour .NE. dest) THEN
#if defined(__MPI)
           IF(mpime .EQ. sour) THEN
             msglen = 1
             CALL MPI_SEND( msg_sour, msglen, MPI_DOUBLE_PRECISION, dest, ip, group, ierr)
             IF (ierr/=0) CALL mp_stop( 8027 )
           ELSE IF(mpime .EQ. dest) THEN
             CALL MPI_RECV( msg_dest, msglen, MPI_DOUBLE_PRECISION, sour, ip, group, istatus, IERR )
             IF (ierr/=0) CALL mp_stop( 8028 )
             CALL MPI_GET_COUNT(istatus, MPI_DOUBLE_PRECISION, nrcv, ierr)
             IF (ierr/=0) CALL mp_stop( 8029 )
             msglen = nrcv
           END IF
#endif
        ELSEIF(mpime .EQ. sour)THEN
          msg_dest = msg_sour
          msglen = 1
        END IF
#if defined(__MPI)
        CALL MPI_BARRIER(group, IERR)
        IF (ierr/=0) CALL mp_stop( 8030 )
#endif
        RETURN
      END SUBROUTINE mp_get_r1

!------------------------------------------------------------------------------!
!
! Carlo Cavazzoni
!
      SUBROUTINE mp_get_rv(msg_dest, msg_sour, mpime, dest, sour, ip, gid)
        REAL (DP) :: msg_dest(:), msg_sour(:)
        INTEGER, INTENT(IN) :: dest, sour, ip, mpime
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
#if defined(__MPI)
        INTEGER :: istatus(MPI_STATUS_SIZE)
#endif
        INTEGER :: ierr, nrcv
        INTEGER :: msglen

#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
#endif

        ! processors not taking part in the communication have 0 length message

        msglen = 0

        IF(sour .NE. dest) THEN
#if defined(__MPI)
           IF(mpime .EQ. sour) THEN
             msglen = SIZE(msg_sour)
             CALL MPI_SEND( msg_sour, SIZE(msg_sour), MPI_DOUBLE_PRECISION, dest, ip, group, ierr)
             IF (ierr/=0) CALL mp_stop( 8027 )
           ELSE IF(mpime .EQ. dest) THEN
             CALL MPI_RECV( msg_dest, SIZE(msg_dest), MPI_DOUBLE_PRECISION, sour, ip, group, istatus, IERR )
             IF (ierr/=0) CALL mp_stop( 8028 )
             CALL MPI_GET_COUNT(istatus, MPI_DOUBLE_PRECISION, nrcv, ierr)
             IF (ierr/=0) CALL mp_stop( 8029 )
             msglen = nrcv
           END IF
#endif
        ELSEIF(mpime .EQ. sour)THEN
          msg_dest(1:SIZE(msg_sour)) = msg_sour(:)
          msglen = SIZE(msg_sour)
        END IF
#if defined(__MPI)
        CALL MPI_BARRIER(group, IERR)
        IF (ierr/=0) CALL mp_stop( 8030 )
#endif
        RETURN
      END SUBROUTINE mp_get_rv

!------------------------------------------------------------------------------!
!
! Carlo Cavazzoni
!
      SUBROUTINE mp_get_rm(msg_dest, msg_sour, mpime, dest, sour, ip, gid)
        REAL (DP) :: msg_dest(:,:), msg_sour(:,:)
        INTEGER, INTENT(IN) :: dest, sour, ip, mpime
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
#if defined(__MPI)
        INTEGER :: istatus(MPI_STATUS_SIZE)
#endif
        INTEGER :: ierr, nrcv
        INTEGER :: msglen

#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
#endif

        ! processors not taking part in the communication have 0 length message

        msglen = 0

        IF(sour .NE. dest) THEN
#if defined(__MPI)
           IF(mpime .EQ. sour) THEN
             CALL MPI_SEND( msg_sour, SIZE(msg_sour), MPI_DOUBLE_PRECISION, dest, ip, group, ierr)
             IF (ierr/=0) CALL mp_stop( 8031 )
             msglen = SIZE(msg_sour)
           ELSE IF(mpime .EQ. dest) THEN
             CALL MPI_RECV( msg_dest, SIZE(msg_dest), MPI_DOUBLE_PRECISION, sour, ip, group, istatus, IERR )
             IF (ierr/=0) CALL mp_stop( 8032 )
             CALL MPI_GET_COUNT(istatus, MPI_DOUBLE_PRECISION, nrcv, ierr)
             IF (ierr/=0) CALL mp_stop( 8033 )
             msglen = nrcv
           END IF
#endif
        ELSEIF(mpime .EQ. sour)THEN
          msg_dest(1:SIZE(msg_sour,1), 1:SIZE(msg_sour,2)) = msg_sour(:,:)
          msglen = SIZE( msg_sour )
        END IF
#if defined(__MPI)
        CALL MPI_BARRIER(group, IERR)
        IF (ierr/=0) CALL mp_stop( 8034 )
#endif
        RETURN
      END SUBROUTINE mp_get_rm


!------------------------------------------------------------------------------!
!
! Carlo Cavazzoni
!
      SUBROUTINE mp_get_cv(msg_dest, msg_sour, mpime, dest, sour, ip, gid)
        COMPLEX (DP) :: msg_dest(:), msg_sour(:)
        INTEGER, INTENT(IN) :: dest, sour, ip, mpime
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
#if defined(__MPI)
        INTEGER :: istatus(MPI_STATUS_SIZE)
#endif
        INTEGER :: ierr, nrcv
        INTEGER :: msglen

#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
#endif

        ! processors not taking part in the communication have 0 length message

        msglen = 0

        IF( dest .NE. sour ) THEN
#if defined(__MPI)
           IF(mpime .EQ. sour) THEN
             CALL MPI_SEND( msg_sour, SIZE(msg_sour), MPI_DOUBLE_COMPLEX, dest, ip, group, ierr)
             IF (ierr/=0) CALL mp_stop( 8035 )
             msglen = SIZE(msg_sour)
           ELSE IF(mpime .EQ. dest) THEN
             CALL MPI_RECV( msg_dest, SIZE(msg_dest), MPI_DOUBLE_COMPLEX, sour, ip, group, istatus, IERR )
             IF (ierr/=0) CALL mp_stop( 8036 )
             CALL MPI_GET_COUNT(istatus, MPI_DOUBLE_COMPLEX, nrcv, ierr)
             IF (ierr/=0) CALL mp_stop( 8037 )
             msglen = nrcv
           END IF
#endif
        ELSEIF(mpime .EQ. sour)THEN
          msg_dest(1:SIZE(msg_sour)) = msg_sour(:)
          msglen = SIZE(msg_sour)
        END IF
#if defined(__MPI)
        CALL MPI_BARRIER(group, IERR)
        IF (ierr/=0) CALL mp_stop( 8038 )
#endif
        RETURN
      END SUBROUTINE mp_get_cv
!------------------------------------------------------------------------------!
!
!
!------------------------------------------------------------------------------!


      SUBROUTINE mp_put_i1(msg_dest, msg_sour, mpime, sour, dest, ip, gid)
        INTEGER :: msg_dest, msg_sour
        INTEGER, INTENT(IN) :: dest, sour, ip, mpime
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
#if defined(__MPI)
        INTEGER :: istatus(MPI_STATUS_SIZE)
#endif
        INTEGER :: ierr, nrcv
        INTEGER :: msglen

#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
#endif

        ! processors not taking part in the communication have 0 length message

        msglen = 0

        IF(dest .NE. sour) THEN
#if defined(__MPI)
           IF(mpime .EQ. sour) THEN
             CALL MPI_SEND( msg_sour, 1, MPI_INTEGER, dest, ip, group, ierr)
             IF (ierr/=0) CALL mp_stop( 8039 )
             msglen = 1
           ELSE IF(mpime .EQ. dest) THEN
             CALL MPI_RECV( msg_dest, 1, MPI_INTEGER, sour, ip, group, istatus, IERR )
             IF (ierr/=0) CALL mp_stop( 8040 )
             CALL MPI_GET_COUNT(istatus, MPI_INTEGER, nrcv, ierr)
             IF (ierr/=0) CALL mp_stop( 8041 )
             msglen = 1
           END IF
#endif
        ELSEIF(mpime .EQ. sour)THEN
          msg_dest = msg_sour
          msglen = 1
        END IF
#if defined(__MPI)
        CALL MPI_BARRIER(group, IERR)
        IF (ierr/=0) CALL mp_stop( 8042 )
#endif
        RETURN
      END SUBROUTINE mp_put_i1

!------------------------------------------------------------------------------!
!
!
      SUBROUTINE mp_put_iv(msg_dest, msg_sour, mpime, sour, dest, ip, gid)
        INTEGER :: msg_dest(:), msg_sour(:)
        INTEGER, INTENT(IN) :: dest, sour, ip, mpime
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
#if defined(__MPI)
        INTEGER :: istatus(MPI_STATUS_SIZE)
#endif
        INTEGER :: ierr, nrcv
        INTEGER :: msglen
#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
#endif
        ! processors not taking part in the communication have 0 length message

        msglen = 0

        IF(sour .NE. dest) THEN
#if defined(__MPI)
           IF(mpime .EQ. sour) THEN
             CALL MPI_SEND( msg_sour, SIZE(msg_sour), MPI_INTEGER, dest, ip, group, ierr)
             IF (ierr/=0) CALL mp_stop( 8043 )
             msglen = SIZE(msg_sour)
           ELSE IF(mpime .EQ. dest) THEN
             CALL MPI_RECV( msg_dest, SIZE(msg_dest), MPI_INTEGER, sour, ip, group, istatus, IERR )
             IF (ierr/=0) CALL mp_stop( 8044 )
             CALL MPI_GET_COUNT(istatus, MPI_INTEGER, nrcv, ierr)
             IF (ierr/=0) CALL mp_stop( 8045 )
             msglen = nrcv
           END IF
#endif
        ELSEIF(mpime .EQ. sour)THEN
          msg_dest(1:SIZE(msg_sour)) = msg_sour(:)
          msglen = SIZE(msg_sour)
        END IF
#if defined(__MPI)
        CALL MPI_BARRIER(group, IERR)
        IF (ierr/=0) CALL mp_stop( 8046 )
#endif
        RETURN
      END SUBROUTINE mp_put_iv

!------------------------------------------------------------------------------!
!
!
      SUBROUTINE mp_put_rv(msg_dest, msg_sour, mpime, sour, dest, ip, gid)
        REAL (DP) :: msg_dest(:), msg_sour(:)
        INTEGER, INTENT(IN) :: dest, sour, ip, mpime
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
#if defined(__MPI)
        INTEGER :: istatus(MPI_STATUS_SIZE)
#endif
        INTEGER :: ierr, nrcv
        INTEGER :: msglen
#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
#endif
        ! processors not taking part in the communication have 0 length message

        msglen = 0

        IF(sour .NE. dest) THEN
#if defined(__MPI)
           IF(mpime .EQ. sour) THEN
             CALL MPI_SEND( msg_sour, SIZE(msg_sour), MPI_DOUBLE_PRECISION, dest, ip, group, ierr)
             IF (ierr/=0) CALL mp_stop( 8047 )
             msglen = SIZE(msg_sour)
           ELSE IF(mpime .EQ. dest) THEN
             CALL MPI_RECV( msg_dest, SIZE(msg_dest), MPI_DOUBLE_PRECISION, sour, ip, group, istatus, IERR )
             IF (ierr/=0) CALL mp_stop( 8048 )
             CALL MPI_GET_COUNT(istatus, MPI_DOUBLE_PRECISION, nrcv, ierr)
             IF (ierr/=0) CALL mp_stop( 8049 )
             msglen = nrcv
           END IF
#endif
        ELSEIF(mpime .EQ. sour)THEN
          msg_dest(1:SIZE(msg_sour)) = msg_sour(:)
          msglen = SIZE(msg_sour)
        END IF
#if defined(__MPI)
        CALL MPI_BARRIER(group, IERR)
        IF (ierr/=0) CALL mp_stop( 8050 )
#endif
        RETURN
      END SUBROUTINE mp_put_rv

!------------------------------------------------------------------------------!
!
!
      SUBROUTINE mp_put_rm(msg_dest, msg_sour, mpime, sour, dest, ip, gid)
        REAL (DP) :: msg_dest(:,:), msg_sour(:,:)
        INTEGER, INTENT(IN) :: dest, sour, ip, mpime
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
#if defined(__MPI)
        INTEGER :: istatus(MPI_STATUS_SIZE)
#endif
        INTEGER :: ierr, nrcv
        INTEGER :: msglen
#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
#endif
        ! processors not taking part in the communication have 0 length message

        msglen = 0

        IF(sour .NE. dest) THEN
#if defined(__MPI)
           IF(mpime .EQ. sour) THEN
             CALL MPI_SEND( msg_sour, SIZE(msg_sour), MPI_DOUBLE_PRECISION, dest, ip, group, ierr)
             IF (ierr/=0) CALL mp_stop( 8051 )
             msglen = SIZE(msg_sour)
           ELSE IF(mpime .EQ. dest) THEN
             CALL MPI_RECV( msg_dest, SIZE(msg_dest), MPI_DOUBLE_PRECISION, sour, ip, group, istatus, IERR )
             IF (ierr/=0) CALL mp_stop( 8052 )
             CALL MPI_GET_COUNT(istatus, MPI_DOUBLE_PRECISION, nrcv, ierr)
             IF (ierr/=0) CALL mp_stop( 8053 )
             msglen = nrcv
           END IF
#endif
        ELSEIF(mpime .EQ. sour)THEN
          msg_dest(1:SIZE(msg_sour,1),1:SIZE(msg_sour,2)) = msg_sour(:,:)
          msglen = SIZE(msg_sour)
        END IF
#if defined(__MPI)
        CALL MPI_BARRIER(group, IERR)
        IF (ierr/=0) CALL mp_stop( 8054 )
#endif
        RETURN
      END SUBROUTINE mp_put_rm


!------------------------------------------------------------------------------!
!
!
      SUBROUTINE mp_put_cv(msg_dest, msg_sour, mpime, sour, dest, ip, gid)
        COMPLEX (DP) :: msg_dest(:), msg_sour(:)
        INTEGER, INTENT(IN) :: dest, sour, ip, mpime
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
#if defined(__MPI)
        INTEGER :: istatus(MPI_STATUS_SIZE)
#endif
        INTEGER :: ierr, nrcv
        INTEGER :: msglen
#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
#endif
        ! processors not taking part in the communication have 0 length message

        msglen = 0

        IF( dest .NE. sour ) THEN
#if defined(__MPI)
           IF(mpime .EQ. sour) THEN
             CALL MPI_SEND( msg_sour, SIZE(msg_sour), MPI_DOUBLE_COMPLEX, dest, ip, group, ierr)
             IF (ierr/=0) CALL mp_stop( 8055 )
             msglen = SIZE(msg_sour)
           ELSE IF(mpime .EQ. dest) THEN
             CALL MPI_RECV( msg_dest, SIZE(msg_dest), MPI_DOUBLE_COMPLEX, sour, ip, group, istatus, IERR )
             IF (ierr/=0) CALL mp_stop( 8056 )
             CALL MPI_GET_COUNT(istatus, MPI_DOUBLE_COMPLEX, nrcv, ierr)
             IF (ierr/=0) CALL mp_stop( 8057 )
             msglen = nrcv
           END IF
#endif
        ELSEIF(mpime .EQ. sour)THEN
          msg_dest(1:SIZE(msg_sour)) = msg_sour(:)
          msglen = SIZE(msg_sour)
        END IF
#if defined(__MPI)
        CALL MPI_BARRIER(group, IERR)
        IF (ierr/=0) CALL mp_stop( 8058 )
#endif
        RETURN
      END SUBROUTINE mp_put_cv

!
!------------------------------------------------------------------------------!
!
!..mp_stop
!
      SUBROUTINE mp_stop(code)
        IMPLICIT NONE
        INTEGER, INTENT (IN) :: code
        WRITE( stdout, fmt='( "*** error in Message Passing (mp) module ***")' )
        WRITE( stdout, fmt='( "*** error msg:  ",A60)' ) TRIM( err_msg )
        WRITE( stdout, fmt='( "*** error code: ",I5)' ) code
#if defined(__MPI)
        CALL mpi_abort(mpi_comm_world,code)
#endif
        STOP
      END SUBROUTINE mp_stop
!------------------------------------------------------------------------------!
!
!..mp_sum
      SUBROUTINE mp_sum_i1(msg,gid)
        IMPLICIT NONE
        INTEGER, INTENT (INOUT) :: msg
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = 1
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL reduce_base_integer( msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_sum_i1
!
!------------------------------------------------------------------------------!
      SUBROUTINE mp_sum_iv(msg,gid)
        IMPLICIT NONE
        INTEGER, INTENT (INOUT) :: msg(:)
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        msglen = size(msg)
        CALL reduce_base_integer( msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_sum_iv
!
!------------------------------------------------------------------------------!

      SUBROUTINE mp_sum_im(msg,gid)
        IMPLICIT NONE
        INTEGER, INTENT (INOUT) :: msg(:,:)
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        msglen = size(msg)
        CALL reduce_base_integer( msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_sum_im
!
!------------------------------------------------------------------------------!

      SUBROUTINE mp_sum_it(msg,gid)
        IMPLICIT NONE
        INTEGER, INTENT (INOUT) :: msg(:,:,:)
        INTEGER, OPTIONAL, INTENT (IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        msglen = size(msg)
        CALL reduce_base_integer( msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_sum_it

!------------------------------------------------------------------------------!

      SUBROUTINE mp_sum_r1(msg,gid)
        IMPLICIT NONE
        REAL (DP), INTENT (INOUT) :: msg
        INTEGER, OPTIONAL, INTENT (IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = 1
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL reduce_base_real( msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_sum_r1

!
!------------------------------------------------------------------------------!

      SUBROUTINE mp_sum_rv(msg,gid)
        IMPLICIT NONE
        REAL (DP), INTENT (INOUT) :: msg(:)
        INTEGER, OPTIONAL, INTENT (IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL reduce_base_real( msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_sum_rv
!
!------------------------------------------------------------------------------!


      SUBROUTINE mp_sum_rm(msg, gid)
        IMPLICIT NONE
        REAL (DP), INTENT (INOUT) :: msg(:,:)
        INTEGER, OPTIONAL, INTENT (IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL reduce_base_real( msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_sum_rm


      SUBROUTINE mp_root_sum_rm( msg, res, root, gid )
        IMPLICIT NONE
        REAL (DP), INTENT (IN)  :: msg(:,:)
        REAL (DP), INTENT (OUT) :: res(:,:)
        INTEGER,   INTENT (IN)  :: root
        INTEGER, OPTIONAL, INTENT (IN) :: gid
        INTEGER :: group
        INTEGER :: msglen, ierr, taskid

#if defined(__MPI)

        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid

        CALL mpi_comm_rank( group, taskid, ierr)
        IF( ierr /= 0 ) CALL mp_stop( 8059 )
        !
        IF( taskid == root ) THEN
           IF( msglen > size(res) ) CALL mp_stop( 8060 )
        END IF

        CALL reduce_base_real_to( msglen, msg, res, group, root )


#else

        res = msg

#endif

      END SUBROUTINE mp_root_sum_rm


      SUBROUTINE mp_root_sum_cm( msg, res, root, gid )
        IMPLICIT NONE
        COMPLEX (DP), INTENT (IN)  :: msg(:,:)
        COMPLEX (DP), INTENT (OUT) :: res(:,:)
        INTEGER,   INTENT (IN)  :: root
        INTEGER, OPTIONAL, INTENT (IN) :: gid
        INTEGER :: group
        INTEGER :: msglen, ierr, taskid

#if defined(__MPI)

        msglen = size(msg)

        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid

        CALL mpi_comm_rank( group, taskid, ierr)
        IF( ierr /= 0 ) CALL mp_stop( 8061 )

        IF( taskid == root ) THEN
           IF( msglen > size(res) ) CALL mp_stop( 8062 )
        END IF

        CALL reduce_base_real_to( 2 * msglen, msg, res, group, root )


#else

        res = msg

#endif

      END SUBROUTINE mp_root_sum_cm

!
!------------------------------------------------------------------------------!


!------------------------------------------------------------------------------!
!

      SUBROUTINE mp_sum_rmm( msg, res, root, gid )
        IMPLICIT NONE
        REAL (DP), INTENT (IN) :: msg(:,:)
        REAL (DP), INTENT (OUT) :: res(:,:)
        INTEGER, OPTIONAL, INTENT (IN) :: root
        INTEGER, OPTIONAL, INTENT (IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
        INTEGER :: taskid, ierr

        msglen = size(msg)

#if defined(__MPI)

        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid

        IF( PRESENT( root ) ) THEN
           !
           CALL mpi_comm_rank( group, taskid, ierr)
           IF( ierr /= 0 ) CALL mp_stop( 8063 )

           IF( taskid == root ) THEN
              IF( msglen > size(res) ) CALL mp_stop( 8064 )
           END IF
           !
           CALL reduce_base_real_to( msglen, msg, res, group, root )
           !
        ELSE
           !
           IF( msglen > size(res) ) CALL mp_stop( 8065 )
           !
           CALL reduce_base_real_to( msglen, msg, res, group, -1 )
           !
        END IF


#else
        res = msg
#endif

      END SUBROUTINE mp_sum_rmm


!
!------------------------------------------------------------------------------!


      SUBROUTINE mp_sum_rt( msg, gid )
        IMPLICIT NONE
        REAL (DP), INTENT (INOUT) :: msg(:,:,:)
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL reduce_base_real( msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_sum_rt

!
!------------------------------------------------------------------------------!
!
! Carlo Cavazzoni
!
      SUBROUTINE mp_sum_r4d(msg,gid)
        IMPLICIT NONE
        REAL (DP), INTENT (INOUT) :: msg(:,:,:,:)
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL reduce_base_real( msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_sum_r4d



!------------------------------------------------------------------------------!

      SUBROUTINE mp_sum_c1(msg,gid)
        IMPLICIT NONE
        COMPLEX (DP), INTENT (INOUT) :: msg
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen

#if defined(__MPI)
        msglen = 1
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL reduce_base_real( 2 * msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_sum_c1
!
!------------------------------------------------------------------------------!

      SUBROUTINE mp_sum_cv(msg,gid)
        IMPLICIT NONE
        COMPLEX (DP), INTENT (INOUT) :: msg(:)
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL reduce_base_real( 2 * msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_sum_cv
!
!------------------------------------------------------------------------------!

      SUBROUTINE mp_sum_cm(msg, gid)
        IMPLICIT NONE
        COMPLEX (DP), INTENT (INOUT) :: msg(:,:)
        INTEGER, OPTIONAL, INTENT (IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL reduce_base_real( 2 * msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_sum_cm
!
!------------------------------------------------------------------------------!


      SUBROUTINE mp_sum_cmm(msg, res, gid)
        IMPLICIT NONE
        COMPLEX (DP), INTENT (IN) :: msg(:,:)
        COMPLEX (DP), INTENT (OUT) :: res(:,:)
        INTEGER, OPTIONAL, INTENT (IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL reduce_base_real_to( 2 * msglen, msg, res, group, -1 )
#else
        res = msg
#endif
      END SUBROUTINE mp_sum_cmm


!
!------------------------------------------------------------------------------!
!
! Carlo Cavazzoni
!
      SUBROUTINE mp_sum_ct(msg,gid)
        IMPLICIT NONE
        COMPLEX (DP), INTENT (INOUT) :: msg(:,:,:)
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = SIZE(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL reduce_base_real( 2 * msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_sum_ct

!
!------------------------------------------------------------------------------!
!
! Carlo Cavazzoni
!
      SUBROUTINE mp_sum_c4d(msg,gid)
        IMPLICIT NONE
        COMPLEX (DP), INTENT (INOUT) :: msg(:,:,:,:)
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL reduce_base_real( 2 * msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_sum_c4d
!
!------------------------------------------------------------------------------!
!
! Carlo Cavazzoni
!
      SUBROUTINE mp_sum_c5d(msg,gid)
        IMPLICIT NONE
        COMPLEX (DP), INTENT (INOUT) :: msg(:,:,:,:,:)
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL reduce_base_real( 2 * msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_sum_c5d

!------------------------------------------------------------------------------!
!
! Carlo Cavazzoni
!
      SUBROUTINE mp_sum_r5d(msg,gid)
        IMPLICIT NONE
        REAL (DP), INTENT (INOUT) :: msg(:,:,:,:,:)
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL reduce_base_real( msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_sum_r5d



!
!------------------------------------------------------------------------------!
!
! Carlo Cavazzoni
!
      SUBROUTINE mp_sum_c6d(msg,gid)
        IMPLICIT NONE
        COMPLEX (DP), INTENT (INOUT) :: msg(:,:,:,:,:,:)
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = size(msg)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL reduce_base_real( 2 * msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_sum_c6d



!------------------------------------------------------------------------------!
      SUBROUTINE mp_max_i(msg,gid)
        IMPLICIT NONE
        INTEGER, INTENT (INOUT) :: msg
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        msglen = 1
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL parallel_max_integer( msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_max_i
!
!------------------------------------------------------------------------------!
!
!..mp_max_iv
!..Carlo Cavazzoni
!
      SUBROUTINE mp_max_iv(msg,gid)
        IMPLICIT NONE
        INTEGER, INTENT (INOUT) :: msg(:)
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        msglen = size(msg)
        CALL parallel_max_integer( msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_max_iv
!
!----------------------------------------------------------------------

      SUBROUTINE mp_max_r(msg,gid)
        IMPLICIT NONE
        REAL (DP), INTENT (INOUT) :: msg
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        msglen = 1
        CALL parallel_max_real( msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_max_r
!
!------------------------------------------------------------------------------!
      SUBROUTINE mp_max_rv(msg,gid)
        IMPLICIT NONE
        REAL (DP), INTENT (INOUT) :: msg(:)
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        msglen = size(msg)
        CALL parallel_max_real( msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_max_rv
!------------------------------------------------------------------------------!
      SUBROUTINE mp_min_i(msg,gid)
        IMPLICIT NONE
        INTEGER, INTENT (INOUT) :: msg
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        msglen = 1
        CALL parallel_min_integer( msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_min_i
!------------------------------------------------------------------------------!
      SUBROUTINE mp_min_iv(msg,gid)
        IMPLICIT NONE
        INTEGER, INTENT (INOUT) :: msg(:)
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        msglen = SIZE(msg)
        CALL parallel_min_integer( msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_min_iv
!------------------------------------------------------------------------------!
      SUBROUTINE mp_min_r(msg,gid)
        IMPLICIT NONE
        REAL (DP), INTENT (INOUT) :: msg
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        msglen = 1
        CALL parallel_min_real( msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_min_r
!
!------------------------------------------------------------------------------!
      SUBROUTINE mp_min_rv(msg,gid)
        IMPLICIT NONE
        REAL (DP), INTENT (INOUT) :: msg(:)
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: msglen
#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        msglen = size(msg)
        CALL parallel_min_real( msglen, msg, group, -1 )
#endif
      END SUBROUTINE mp_min_rv

!------------------------------------------------------------------------------!

      SUBROUTINE mp_barrier(gid)
        IMPLICIT NONE
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: ierr
#if defined(__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL MPI_BARRIER(group,IERR)
        IF (ierr/=0) CALL mp_stop( 8066 )
#endif
      END SUBROUTINE mp_barrier

!------------------------------------------------------------------------------!
!.. Carlo Cavazzoni
!..mp_rank
      FUNCTION mp_rank( comm )
        IMPLICIT NONE
        INTEGER :: mp_rank
        INTEGER, OPTIONAL, INTENT(IN) :: comm
        INTEGER :: ierr, taskid

        ierr = 0
        taskid = 0
#if defined(__MPI)
        IF( PRESENT( comm ) ) THEN
           CALL mpi_comm_rank(comm,taskid,ierr)
        ELSE
           CALL mpi_comm_rank(mpi_comm_world,taskid,ierr)
        END IF
        IF (ierr/=0) CALL mp_stop( 8067 )
#endif
        mp_rank = taskid
      END FUNCTION mp_rank

!------------------------------------------------------------------------------!
!.. Carlo Cavazzoni
!..mp_size
      FUNCTION mp_size( comm )
        IMPLICIT NONE
        INTEGER :: mp_size
        INTEGER, OPTIONAL, INTENT(IN) :: comm
        INTEGER :: ierr, numtask

        ierr = 0
        numtask = 1
#if defined(__MPI)
        IF( PRESENT( comm ) ) THEN
           CALL mpi_comm_size(comm,numtask,ierr)
        ELSE
           CALL mpi_comm_size(mpi_comm_world,numtask,ierr)
        END IF
        IF (ierr/=0) CALL mp_stop( 8068 )
#endif
        mp_size = numtask
      END FUNCTION mp_size

      SUBROUTINE mp_report
        INTEGER :: i
        WRITE( stdout, *)
#if defined(__MPI)
#  if defined (__MP_STAT)
        WRITE( stdout, 20 )
#  endif
20      FORMAT(3X,'please use an MPI profiler to analisy communications ')
#else
        WRITE( stdout, *)
#endif
        RETURN
      END SUBROUTINE mp_report


!------------------------------------------------------------------------------!
!..mp_gatherv_rv
!..Carlo Cavazzoni

      SUBROUTINE mp_gatherv_rv( mydata, alldata, recvcount, displs, root, gid)
        IMPLICIT NONE
        REAL(DP) :: mydata(:)
        REAL(DP) :: alldata(:)
        INTEGER, INTENT(IN) :: recvcount(:), displs(:), root
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: ierr, npe, myid

#if defined (__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL mpi_comm_size( group, npe, ierr )
        IF (ierr/=0) CALL mp_stop( 8069 )
        CALL mpi_comm_rank( group, myid, ierr )
        IF (ierr/=0) CALL mp_stop( 8070 )
        !
        IF ( SIZE( recvcount ) < npe .OR. SIZE( displs ) < npe ) CALL mp_stop( 8071 )
        IF ( myid == root ) THEN
           IF ( SIZE( alldata ) < displs( npe ) + recvcount( npe ) ) CALL mp_stop( 8072 )
        END IF
        IF ( SIZE( mydata ) < recvcount( myid + 1 ) ) CALL mp_stop( 8073 )
        !
        CALL MPI_GATHERV( mydata, recvcount( myid + 1 ), MPI_DOUBLE_PRECISION, &
                         alldata, recvcount, displs, MPI_DOUBLE_PRECISION, root, group, ierr )
        IF (ierr/=0) CALL mp_stop( 8074 )
#else
        IF ( SIZE( alldata ) < recvcount( 1 ) ) CALL mp_stop( 8075 )
        IF ( SIZE( mydata  ) < recvcount( 1 ) ) CALL mp_stop( 8076 )
        !
        alldata( 1:recvcount( 1 ) ) = mydata( 1:recvcount( 1 ) )
#endif
        RETURN
      END SUBROUTINE mp_gatherv_rv

!------------------------------------------------------------------------------!
!..mp_gatherv_cv
!..Carlo Cavazzoni

      SUBROUTINE mp_gatherv_cv( mydata, alldata, recvcount, displs, root, gid)
        IMPLICIT NONE
        COMPLEX(DP) :: mydata(:)
        COMPLEX(DP) :: alldata(:)
        INTEGER, INTENT(IN) :: recvcount(:), displs(:), root
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: ierr, npe, myid

#if defined (__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL mpi_comm_size( group, npe, ierr )
        IF (ierr/=0) CALL mp_stop( 8069 )
        CALL mpi_comm_rank( group, myid, ierr )
        IF (ierr/=0) CALL mp_stop( 8070 )
        !
        IF ( SIZE( recvcount ) < npe .OR. SIZE( displs ) < npe ) CALL mp_stop( 8071 )
        IF ( myid == root ) THEN
           IF ( SIZE( alldata ) < displs( npe ) + recvcount( npe ) ) CALL mp_stop( 8072 )
        END IF
        IF ( SIZE( mydata ) < recvcount( myid + 1 ) ) CALL mp_stop( 8073 )
        !
        CALL MPI_GATHERV( mydata, recvcount( myid + 1 ), MPI_DOUBLE_COMPLEX, &
                         alldata, recvcount, displs, MPI_DOUBLE_COMPLEX, root, group, ierr )
        IF (ierr/=0) CALL mp_stop( 8074 )
#else
        IF ( SIZE( alldata ) < recvcount( 1 ) ) CALL mp_stop( 8075 )
        IF ( SIZE( mydata  ) < recvcount( 1 ) ) CALL mp_stop( 8076 )
        !
        alldata( 1:recvcount( 1 ) ) = mydata( 1:recvcount( 1 ) )
#endif
        RETURN
      END SUBROUTINE mp_gatherv_cv

!------------------------------------------------------------------------------!
!..mp_gatherv_rv
!..Carlo Cavazzoni

      SUBROUTINE mp_gatherv_iv( mydata, alldata, recvcount, displs, root, gid)
        IMPLICIT NONE
        INTEGER :: mydata(:)
        INTEGER :: alldata(:)
        INTEGER, INTENT(IN) :: recvcount(:), displs(:), root
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: ierr, npe, myid

#if defined (__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL mpi_comm_size( group, npe, ierr )
        IF (ierr/=0) CALL mp_stop( 8069 )
        CALL mpi_comm_rank( group, myid, ierr )
        IF (ierr/=0) CALL mp_stop( 8070 )
        !
        IF ( SIZE( recvcount ) < npe .OR. SIZE( displs ) < npe ) CALL mp_stop( 8071 )
        IF ( myid == root ) THEN
           IF ( SIZE( alldata ) < displs( npe ) + recvcount( npe ) ) CALL mp_stop( 8072 )
        END IF
        IF ( SIZE( mydata ) < recvcount( myid + 1 ) ) CALL mp_stop( 8073 )
        !
        CALL MPI_GATHERV( mydata, recvcount( myid + 1 ), MPI_INTEGER, &
                         alldata, recvcount, displs, MPI_INTEGER, root, group, ierr )
        IF (ierr/=0) CALL mp_stop( 8074 )
#else
        IF ( SIZE( alldata ) < recvcount( 1 ) ) CALL mp_stop( 8075 )
        IF ( SIZE( mydata  ) < recvcount( 1 ) ) CALL mp_stop( 8076 )
        !
        alldata( 1:recvcount( 1 ) ) = mydata( 1:recvcount( 1 ) )
#endif
        RETURN
      END SUBROUTINE mp_gatherv_iv


!------------------------------------------------------------------------------!
!..mp_gatherv_rm
!..Carlo Cavazzoni

      SUBROUTINE mp_gatherv_rm( mydata, alldata, recvcount, displs, root, gid)
        IMPLICIT NONE
        REAL(DP) :: mydata(:,:)  ! Warning first dimension is supposed constant!
        REAL(DP) :: alldata(:,:)
        INTEGER, INTENT(IN) :: recvcount(:), displs(:), root
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: ierr, npe, myid, nsiz
        INTEGER, ALLOCATABLE :: nrecv(:), ndisp(:)


#if defined (__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL mpi_comm_size( group, npe, ierr )
        IF (ierr/=0) CALL mp_stop( 8069 )
        CALL mpi_comm_rank( group, myid, ierr )
        IF (ierr/=0) CALL mp_stop( 8070 )
        !
        IF ( SIZE( recvcount ) < npe .OR. SIZE( displs ) < npe ) CALL mp_stop( 8071 )
        IF ( myid == root ) THEN
           IF ( SIZE( alldata, 2 ) < displs( npe ) + recvcount( npe ) ) CALL mp_stop( 8072 )
           IF ( SIZE( alldata, 1 ) /= SIZE( mydata, 1 ) ) CALL mp_stop( 8072 )
        END IF
        IF ( SIZE( mydata, 2 ) < recvcount( myid + 1 ) ) CALL mp_stop( 8073 )
        !
        ALLOCATE( nrecv( npe ), ndisp( npe ) )
        !
        nrecv( 1:npe ) = recvcount( 1:npe ) * SIZE( mydata, 1 )
        ndisp( 1:npe ) = displs( 1:npe ) * SIZE( mydata, 1 )
        !
        CALL MPI_GATHERV( mydata, nrecv( myid + 1 ), MPI_DOUBLE_PRECISION, &
                         alldata, nrecv, ndisp, MPI_DOUBLE_PRECISION, root, group, ierr )
        IF (ierr/=0) CALL mp_stop( 8074 )
        !
        DEALLOCATE( nrecv, ndisp )
        !
#else
        IF ( SIZE( alldata, 1 ) /= SIZE( mydata, 1 ) ) CALL mp_stop( 8075 )
        IF ( SIZE( alldata, 2 ) < recvcount( 1 ) ) CALL mp_stop( 8075 )
        IF ( SIZE( mydata, 2  ) < recvcount( 1 ) ) CALL mp_stop( 8076 )
        !
        alldata( :, 1:recvcount( 1 ) ) = mydata( :, 1:recvcount( 1 ) )
#endif
        RETURN
      END SUBROUTINE mp_gatherv_rm

!------------------------------------------------------------------------------!
!..mp_gatherv_im
!..Carlo Cavazzoni

      SUBROUTINE mp_gatherv_im( mydata, alldata, recvcount, displs, root, gid)
        IMPLICIT NONE
        INTEGER :: mydata(:,:)  ! Warning first dimension is supposed constant!
        INTEGER :: alldata(:,:)
        INTEGER, INTENT(IN) :: recvcount(:), displs(:), root
        INTEGER, OPTIONAL, INTENT(IN) :: gid
        INTEGER :: group
        INTEGER :: ierr, npe, myid, nsiz
        INTEGER, ALLOCATABLE :: nrecv(:), ndisp(:)


#if defined (__MPI)
        group = mpi_comm_world
        IF( PRESENT( gid ) ) group = gid
        CALL mpi_comm_size( group, npe, ierr )
        IF (ierr/=0) CALL mp_stop( 8069 )
        CALL mpi_comm_rank( group, myid, ierr )
        IF (ierr/=0) CALL mp_stop( 8070 )
        !
        IF ( SIZE( recvcount ) < npe .OR. SIZE( displs ) < npe ) CALL mp_stop( 8071 )
        IF ( myid == root ) THEN
           IF ( SIZE( alldata, 2 ) < displs( npe ) + recvcount( npe ) ) CALL mp_stop( 8072 )
           IF ( SIZE( alldata, 1 ) /= SIZE( mydata, 1 ) ) CALL mp_stop( 8072 )
        END IF
        IF ( SIZE( mydata, 2 ) < recvcount( myid + 1 ) ) CALL mp_stop( 8073 )
        !
        ALLOCATE( nrecv( npe ), ndisp( npe ) )
        !
        nrecv( 1:npe ) = recvcount( 1:npe ) * SIZE( mydata, 1 )
        ndisp( 1:npe ) = displs( 1:npe ) * SIZE( mydata, 1 )
        !
        CALL MPI_GATHERV( mydata, nrecv( myid + 1 ), MPI_INTEGER, &
                         alldata, nrecv, ndisp, MPI_INTEGER, root, group, ierr )
        IF (ierr/=0) CALL mp_stop( 8074 )
        !
        DEALLOCATE( nrecv, ndisp )
        !
#else
        IF ( SIZE( alldata, 1 ) /= SIZE( mydata, 1 ) ) CALL mp_stop( 8075 )
        IF ( SIZE( alldata, 2 ) < recvcount( 1 ) ) CALL mp_stop( 8075 )
        IF ( SIZE( mydata, 2  ) < recvcount( 1 ) ) CALL mp_stop( 8076 )
        !
        alldata( :, 1:recvcount( 1 ) ) = mydata( :, 1:recvcount( 1 ) )
#endif
        RETURN
      END SUBROUTINE mp_gatherv_im


!------------------------------------------------------------------------------!

      SUBROUTINE mp_set_displs( recvcount, displs, ntot, nproc )
        !  Given the number of elements on each processor (recvcount), this subroutine
        !  sets the correct offsets (displs) to collect them on a single
        !  array with contiguous elemets
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: recvcount(:) ! number of elements on each processor
        INTEGER, INTENT(OUT) :: displs(:)   ! offsets/displacements
        INTEGER, INTENT(OUT) :: ntot
        INTEGER, INTENT(IN) :: nproc
        INTEGER :: i

        displs( 1 ) = 0
        !
#if defined (__MPI)
        IF( nproc < 1 ) CALL mp_stop( 8090 )
        DO i = 2, nproc
           displs( i ) = displs( i - 1 ) + recvcount( i - 1 )
        END DO
        ntot = displs( nproc ) + recvcount( nproc )
#else
        ntot = recvcount( 1 )
#endif
        RETURN
      END SUBROUTINE mp_set_displs

!------------------------------------------------------------------------------!


SUBROUTINE mp_alltoall_c3d( sndbuf, rcvbuf, gid )
   IMPLICIT NONE
   COMPLEX(DP) :: sndbuf( :, :, : )
   COMPLEX(DP) :: rcvbuf( :, :, : )
   INTEGER, OPTIONAL, INTENT(IN) :: gid
   INTEGER :: nsiz, group, ierr, npe

#if defined (__MPI)

   group = mpi_comm_world
   IF( PRESENT( gid ) ) group = gid

   CALL mpi_comm_size( group, npe, ierr )
   IF (ierr/=0) CALL mp_stop( 8069 )

   IF ( SIZE( sndbuf, 3 ) < npe ) CALL mp_stop( 8069 )
   IF ( SIZE( rcvbuf, 3 ) < npe ) CALL mp_stop( 8069 )

   nsiz = SIZE( sndbuf, 1 ) * SIZE( sndbuf, 2 )

   CALL MPI_ALLTOALL( sndbuf, nsiz, MPI_DOUBLE_COMPLEX, &
                      rcvbuf, nsiz, MPI_DOUBLE_COMPLEX, group, ierr )

   IF (ierr/=0) CALL mp_stop( 8074 )

#else

   rcvbuf = sndbuf

#endif

   RETURN
END SUBROUTINE mp_alltoall_c3d


!------------------------------------------------------------------------------!

SUBROUTINE mp_alltoall_i3d( sndbuf, rcvbuf, gid )
   IMPLICIT NONE
   INTEGER :: sndbuf( :, :, : )
   INTEGER :: rcvbuf( :, :, : )
   INTEGER, OPTIONAL, INTENT(IN) :: gid
   INTEGER :: nsiz, group, ierr, npe

#if defined (__MPI)

   group = mpi_comm_world
   IF( PRESENT( gid ) ) group = gid

   CALL mpi_comm_size( group, npe, ierr )
   IF (ierr/=0) CALL mp_stop( 8069 )

   IF ( SIZE( sndbuf, 3 ) < npe ) CALL mp_stop( 8069 )
   IF ( SIZE( rcvbuf, 3 ) < npe ) CALL mp_stop( 8069 )

   nsiz = SIZE( sndbuf, 1 ) * SIZE( sndbuf, 2 )

   CALL MPI_ALLTOALL( sndbuf, nsiz, MPI_INTEGER, &
                      rcvbuf, nsiz, MPI_INTEGER, group, ierr )

   IF (ierr/=0) CALL mp_stop( 8074 )

#else

   rcvbuf = sndbuf

#endif

   RETURN
END SUBROUTINE mp_alltoall_i3d

SUBROUTINE mp_circular_shift_left_d2d_int( buf, itag, gid )
   IMPLICIT NONE
   INTEGER :: buf
   INTEGER, INTENT(IN) :: itag
   INTEGER, OPTIONAL, INTENT(IN) :: gid
   INTEGER :: nsiz, group, ierr, npe, sour, dest, mype

#if defined (__MPI)

   INTEGER :: istatus( mpi_status_size )
   !
   group = mpi_comm_world
   IF( PRESENT( gid ) ) group = gid
   !
   CALL mpi_comm_size( group, npe, ierr )
   IF (ierr/=0) CALL mp_stop( 8100 )
   CALL mpi_comm_rank( group, mype, ierr )
   IF (ierr/=0) CALL mp_stop( 8101 )
   !
   sour = mype + 1
   IF( sour == npe ) sour = 0
   dest = mype - 1
   IF( dest == -1 ) dest = npe - 1
   !
   CALL MPI_Sendrecv_replace( buf, 1, MPI_INTEGER, &
        dest, itag, sour, itag, group, istatus, ierr)
   !
   IF (ierr/=0) CALL mp_stop( 8102 )
   !
#else
   ! do nothing
#endif
   RETURN
END SUBROUTINE mp_circular_shift_left_d2d_int



SUBROUTINE mp_circular_shift_left_d2d_double( buf, itag, gid )
   IMPLICIT NONE
   REAL(DP) :: buf( :, : )
   INTEGER, INTENT(IN) :: itag
   INTEGER, OPTIONAL, INTENT(IN) :: gid
   INTEGER :: nsiz, group, ierr, npe, sour, dest, mype

#if defined (__MPI)

   INTEGER :: istatus( mpi_status_size )
   !
   group = mpi_comm_world
   IF( PRESENT( gid ) ) group = gid
   !
   CALL mpi_comm_size( group, npe, ierr )
   IF (ierr/=0) CALL mp_stop( 8100 )
   CALL mpi_comm_rank( group, mype, ierr )
   IF (ierr/=0) CALL mp_stop( 8101 )
   !
   sour = mype + 1
   IF( sour == npe ) sour = 0
   dest = mype - 1
   IF( dest == -1 ) dest = npe - 1
   !
   CALL MPI_Sendrecv_replace( buf, SIZE(buf), MPI_DOUBLE_PRECISION, &
        dest, itag, sour, itag, group, istatus, ierr)
   !
   IF (ierr/=0) CALL mp_stop( 8102 )
   !
#else
   ! do nothing
#endif
   RETURN
END SUBROUTINE mp_circular_shift_left_d2d_double


      FUNCTION mp_get_comm_null( )
        IMPLICIT NONE
        INTEGER :: mp_get_comm_null
#if defined(__MPI)
           mp_get_comm_null = MPI_COMM_NULL
#else
           mp_get_comm_null = 0
#endif
      END FUNCTION mp_get_comm_null

      FUNCTION mp_get_comm_self( )
        IMPLICIT NONE
        INTEGER :: mp_get_comm_self
#if defined(__MPI)
           mp_get_comm_self = MPI_COMM_SELF
#else
           mp_get_comm_self = 0
#endif
      END FUNCTION mp_get_comm_self

!------------------------------------------------------------------------------!
    END MODULE mp
!------------------------------------------------------------------------------!

