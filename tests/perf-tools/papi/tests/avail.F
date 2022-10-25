C This file performs the following tests:
C Hardware info
#include "fpapi_test.h"

      program avail
      IMPLICIT integer (p)

      INTEGER ncpu,nnodes,totalcpus,vendor,model, check, handle, n
      CHARACTER*(PAPI_MAX_STR_LEN) vstring, mstring
      REAL revision, mhz
      integer last_char
      external last_char
      integer i, avail_flag, flags,k,l
      CHARACTER*(PAPI_MAX_STR_LEN) event_name, event_descr, 
     *event_label, event_note
      CHARACTER*(10) avail_str, flags_str
      integer tests_quiet, get_quiet
      external get_quiet

      tests_quiet = get_quiet()

      handle=0 
      check = PAPI_VER_CURRENT
      call PAPIf_library_init(check)
      if ( check.NE.PAPI_VER_CURRENT) then
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPI_library_init', check)
      end if

      call PAPIf_get_hardware_info( ncpu,nnodes,totalcpus,vendor,
     .     vstring, model, mstring, revision, mhz )

      if (tests_quiet .EQ. 0) then
      print *, 'Hardware information and available events'
      print *, '--------------------------------------'//
     .'---------------------------------------'
      n=last_char(vstring)
      print *, 'Vendor string and code   : ',vstring(1:n),
     &' (',vendor,')'
      n=last_char(mstring)
      print *, 'Model string and code    : ',mstring(1:n),' (',model,')'
      print *, 'CPU revision             : ',revision 
      print *, 'CPU Megahertz            : ',mhz
      print *, 'CPUs in an SMP node      : ',ncpu
      print *, 'Nodes in the system      : ',nnodes
      print *, 'Total CPUs in the system : ',totalcpus
      print *, '--------------------------------------'//
     .'---------------------------------------'

      write (*,200) 'Name', 'Code', 'Avail', 'Deriv', 
     *'Description', '(note)'
 200  format(A8, A12, A9, A6, A25, A30)
      end if
      event_name=' '
      do i=0, PAPI_MAX_PRESET_EVENTS-1
C PAPI_L1_DCM is the first event in the list
         call papif_get_event_info(PAPI_L1_DCM+i, event_name, 
     *   event_descr, event_label, avail_flag, event_note, flags, check)
         if (avail_flag.EQ.1) then
            avail_str = 'Yes'
         else 
            avail_str = 'No'
         end if

         if (flags.EQ.1) then
            flags_str = 'Yes'
         else 
            flags_str = 'No'
         end if

         if (check.EQ.PAPI_OK .and. tests_quiet .EQ. 0) then
            l=1
            do k=len(event_note),1,-1
              if(l.EQ.1.AND.event_note(k:k).NE.' ') l=k
            end do
C PAPI_L1_DCM is the first event in the list
            write (6, 100) event_name, PAPI_L1_DCM+i, avail_str, 
     *       flags_str, event_descr, event_note(1:l)
 100  format(A12, '0x', z8, 2x, A5, 1x, A5, A45, 1x,'(', A, ')') 
         end if
      end do
      if (tests_quiet .EQ. 0) then 
      print *, '--------------------------------------'//
     .'---------------------------------------'
      end if

      call ftests_pass(__FILE__)      
      end
