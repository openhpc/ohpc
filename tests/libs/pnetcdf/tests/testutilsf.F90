!
!  Copyright (C) 2015, Northwestern University and Argonne National Laboratory
!  See COPYRIGHT notice in top-level directory.
!
!     This is part of the PnetCDF package.
!
!     $Id$

      ! This function gets the executable name and output file name from the
      ! command line.
      integer function get_args(cmd, filename)
#ifdef NAGFOR
          USE F90_UNIX_ENV, only : iargc, getarg
          implicit none
#else
          implicit none
          integer iargc
#endif
          integer argc, i
          character(len=*) cmd, filename
          character(len=256) full_cmd

          get_args = 1
          call getarg(0, full_cmd)

          ! remove basename from executable name
          i = INDEX(full_cmd, "/", .TRUE.)
          if (i .EQ. 0) then
              cmd(:) = full_cmd(:)
          else
              cmd(:) = full_cmd(i+1:)
          endif

          argc = IARGC()
          if (argc .GT. 1) then
              print*,'Usage: ',trim(cmd),' [filename]'
              get_args = 0
              return
          endif
          if (argc .EQ. 1) call getarg(1, filename)
      end function get_args

      ! This function prints the pass/fail message on screen
      subroutine pass_fail(nerrs, msg)
          implicit none
          integer nerrs
          character(len=*) msg

          ! local variables
          CHARACTER ESC
          PARAMETER (ESC=char(27))

#ifdef PNETCDF_DEBUG
          CHARACTER (LEN=20) PASS_STR, FAIL_STR
          PARAMETER (PASS_STR='------ '//ESC//'[32mpass'//ESC//'[0m')
          PARAMETER (FAIL_STR='------ '//ESC//'[31mfail'//ESC//'[0m')
#else
          CHARACTER (LEN=11) PASS_STR, FAIL_STR
          PARAMETER (PASS_STR='------ pass')
          PARAMETER (FAIL_STR='------ fail')
#endif

          if (nerrs .EQ. 0) then
              write(*,"(A67,A)") msg, PASS_STR
          else
              write(*,"(A67,A)") msg, FAIL_STR
          endif
      end subroutine pass_fail

      subroutine get_env(hint_str, value)
          character(len=*) hint_str, value
#ifdef HAS_GET_ENVIRONMENT_VARIABLE
          call Get_Environment_Variable(hint_str, Value=value)
#else
          call getenv(hint_str, value)
#endif
      end subroutine get_env

