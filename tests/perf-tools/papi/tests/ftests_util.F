#include "fpapi_test.h"

      integer function get_quiet()
      implicit integer (p)
      character*25 chbuf
      integer retval
      integer quiet
      common quiet

C     This routine tests for a command line argument
C     that matches 'TESTS_QUIET'
C     If found, it returns 1 to set output to quiet
C     The routine was placed her to hide the ugly
C     Windows #if stuff from normal view
C     And also to make the test code read cleaner
      call getarg(1,chbuf)
      get_quiet = 0
      if (LGE(chbuf, 'TESTS_QUIET')) then
        get_quiet=1
      else
         call PAPIf_set_debug(PAPI_VERB_ECONT, retval)
         if ( retval.NE.PAPI_OK) then
            call ftest_fail(__FILE__, __LINE__,
     .       'PAPIf_set_debug', retval)
         end if
      end if
      quiet=get_quiet
      end


      integer function last_char(string)
      implicit integer (p)
      character*(*) string

      do last_char=len(string),1,-1
        if(string(last_char:last_char).NE.' ') return
      end do
      last_char=0
      end

      subroutine ftests_warning(line,msg)
      implicit integer (p)
      character*(*) msg
      integer line

      write(*,*) '****  WARNING message  ****'
      call ftests_perror(line,msg)
      end

      subroutine ftests_fatal_error(line,msg)
      implicit integer (p)
      character*(*) msg
      integer line

      call ftests_perror(line,msg)
      call pause()
      stop
      end

      subroutine ftests_perror(line,msg)
      implicit integer (p)
      character*(*) msg
      integer line

      write(*,*) '****  Test error occurred  ****'
      write(*,100) line,msg
 100  format(t3,'Line # ',i5,':: ',a)

      end
      
      subroutine ftests_pass(test_str)
      implicit integer (p)
      character*(*) test_str

      write(*,100) test_str,' PASSED'
      call PAPIF_shutdown()
      call pause()
      stop
 100  format(a,t41,a)
      end

      subroutine ftests_skip(test_str,papiretval)
      implicit integer (p)
      character*(*) test_str
      character*(PAPI_MAX_STR_LEN) papi_errstr
      integer papiretval
      integer last_char
      external last_char
      integer quiet
      common quiet

      write(*,100) test_str,' SKIPPED'
      if (quiet .eq. 0) then
        if(papiretval.lt.0)then
          call PAPIF_perror( )
          write(*,*) 'PAPI error: ',
     *       papi_errstr(1:last_char(papi_errstr))
        end if
      end if
      call pause()
      stop
 100  format(a,t41,a)
      end

      subroutine ftest_fail(file, line, callstr, retval)
      implicit integer (p)
      character*(*) file
      integer line
      character*(*) callstr
      integer retval,ilen
      character*(PAPI_MAX_STR_LEN) papi_errstr
      integer last_char
      external last_char

      if ( retval.eq.PAPI_ECMP .OR. retval.eq.PAPI_ENOEVNT
     &    .OR. retval.eq.PAPI_ECNFLCT .OR. retval.eq.PAPI_EPERM ) then
        call ftest_skip(file, line, callstr, retval)
      end if
      if ( retval.eq.PAPI_ENOEVNT ) then
        call ftest_skip(file, line, callstr, retval)
      end if

      if ( retval.ne.0 )then
        write(*,100) file,' FAILED'
      else
        write(*,100) file,' SKIPPED'
      end if
      write(*,*) 'Line #', line

      if (retval.eq.PAPI_ESYS) then
        write(*,*) "System error in ", callstr
      else if (retval.gt.0) then
        write(*,*) "Error calculating: ", callstr
      else if(retval.eq.0)then
        write(*,*) 'SGI requires root permissions for this test'
      else
        call PAPIF_perror( )
        ilen=last_char(callstr)
        write(*,'(T2,3a)') 'PAPI error in ', callstr(1:ilen), 
     *       ': '// papi_errstr(1:last_char(papi_errstr))
      end if
      call pause()
      stop
 100  format(a,t41,a)
      end

      subroutine ftest_skip(file, line, callstr, retval)
      implicit integer (p)
      character*(*) file
      integer line
      character*(*) callstr
      integer retval,ilen
      integer quiet
      common quiet

      character*(PAPI_MAX_STR_LEN) papi_errstr
      integer last_char
      external last_char

      write(*,100) file, ' SKIPPED'
      if (quiet .eq. 0) then
        write(*,*) 'Line #', line
        if(retval.eq.PAPI_ESYS) then
          write(*,*) "System error in ", callstr
        else if (retval.gt.0) then
          write(*,*) "Error calculating: ", callstr
        else
          call PAPIF_perror( )
          ilen=last_char(callstr)
          write(*,'(T2,3a)') 'Error in ', callstr(1:ilen),
     *         ': ' // papi_errstr(1:last_char(papi_errstr))
        end if
      end if
      call pause()
      stop
 100  format(a,t41,a)
      end

      subroutine stringify_domain(domain, str)
      implicit integer (p)      
      integer domain
      character*(PAPI_MAX_STR_LEN) str
      integer idx
      
      if (domain .EQ. PAPI_DOM_ALL) then
        str = "PAPI_DOM_ALL"
      else
        idx = 1
C       Is there a better way to write this?
        if (IAND(domain, PAPI_DOM_USER) .NE. 0) then
          str(idx:idx+13) = "+PAPI_DOM_USER"
          idx = idx + 14
        end if
        if (IAND(domain, PAPI_DOM_KERNEL) .NE. 0) then
          str(idx:idx+15) = "+PAPI_DOM_KERNEL"
          idx = idx + 16
        end if
        if (IAND(domain, PAPI_DOM_SUPERVISOR) .NE. 0) then
          str(idx:idx+19) = "+PAPI_DOM_SUPERVISOR"
          idx = idx + 20
        end if
        if (IAND(domain, PAPI_DOM_OTHER) .NE. 0) then
          str(idx:idx+14) = "+PAPI_DOM_OTHER"
          idx = idx + 15
        end if
        if (idx .NE. 1) then
C         Eliminate the first + character
          str = str(2:LEN(str))
C         Blank-fill the rest of the string so last_char works correctly
          str(idx:) = ' '
        else
          print *, 'error in stringify_domain'
          call pause()
          stop
        end if
      end if
      end

      subroutine stringify_granularity(granularity, str)
      implicit integer (p)      
      integer granularity
      character*(PAPI_MAX_STR_LEN) str

      if (granularity .EQ. PAPI_GRN_THR) then
        str = "PAPI_GRN_THR"
      else if (granularity .EQ. PAPI_GRN_PROC) then
        str = "PAPI_GRN_PROC"
      else if (granularity .EQ. PAPI_GRN_PROCG) then
        str = "PAPI_GRN_PROCG"
      else if (granularity .EQ. PAPI_GRN_SYS_CPU) then
        str = "PAPI_GRN_SYS_CPU"
      else if (granularity .EQ. PAPI_GRN_SYS) then
        str = "PAPI_GRN_SYS"
      else
        print *, 'error in stringify_granularity'
        call pause()
        stop
      end if
      end

C     This routine provides a bottleneck
C     at the exit point of a program
C     For Windows, it links to a simple C routine
C     that prompts the user for a keypress
C     For every other platform it is a nop.
      subroutine pause()
      implicit integer (p)      
      integer quiet
      common quiet

      end

C     Print the content of an event set
      subroutine PrintEventSet(ES)
      IMPLICIT integer (p)
      integer ES
      integer MAXEVENT
      parameter(MAXEVENT=64)
      integer n,i,codes(MAXEVENT),retval
      character*(PAPI_MAX_STR_LEN) name

      n=MAXEVENT
      call PAPIf_list_events(ES,codes,n,retval)
      if(n.gt.MAXEVENT) write(*,100) n,MAXEVENT
  100 format(T1,'There are',i4,' events in the set. Can only print',
     &       i4,'.')

      do i=1,min(n,MAXEVENT)
        name = ' '
        call PAPIf_event_code_to_name(codes(i),name,retval)
        if(retval .EQ. PAPI_OK)then
          write(*,200) i,name
        else
          write(*,210) i,codes(i),'**Error looking up the event name**'
        end if
      end do
  200 format(T1,i4,': ',a40)
  210 format(T1,i4,' (',i12.11,'): ',a40)

      write(*,*)
      end

      subroutine init_multiplex()
      IMPLICIT integer (p)
      integer retval
      CHARACTER*(PAPI_MAX_STR_LEN)  vstring, mstring
      INTEGER ncpu,nnodes,totalcpus,vendor,model
      REAL revision, mhz
      integer nchr,i

C     Get PAPI h/w info
      call PAPIf_get_hardware_info( ncpu, nnodes, totalcpus, vendor,
     .   vstring, model, mstring, revision, mhz )
      do i=len(mstring),1,-1
        if(mstring(i:i).NE.' ') goto 10
      end do
 10   if(i.LT.1)then
        nchr=1
      else
        nchr=i
      end if

      if ( mstring(1:nchr).EQ."POWER6") then
C     Setting domain to user+kernel+supervisor is really only necessary when
C     using PAPI multiplexing on POWER6/perfctr.  But since the Fortran API
C     does not include access to component info, we'll just set the domain
C     in this manner for all components on POWER6.
      call PAPIf_set_domain(PAPI_DOM_ALL, retval)
      if ( retval.NE.PAPI_OK) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPIf_set_domain', retval)
      end if
      end if

      call PAPIf_multiplex_init(retval)
      if ( retval.NE.PAPI_OK) then
        call ftest_fail(__FILE__, __LINE__,
     &      'papif_multiplex_init', retval)
      end if

      end

