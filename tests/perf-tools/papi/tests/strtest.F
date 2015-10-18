C  Strtest - Perform some basic tests of the functionality of the
C  string passing to and from the PAPI Fortran interface. 
C
C    Test 1: Look up an event name from an event code. Use this name
C            to try and locate the event code using the name received.
C            Long, short and too short strings are used in the tests
C
C    Test 2: Look up a PAPI error string. Use long, short and too 
C            short strings to store the result.
C
C    Test 3: Look up and display event descriptions 
C            using PAPIf_get_event_info.
C
C    Comments:
C    When using the Fortran interface it may not always be possible to
C    use the PAPI predefined constants as actual arguments. Due to the
C    values in these compilers might occasionally cast these into the
C    wrong type. In the code below the line code=MSGCODE is used to 
C    make sure that the event code get the right type.
C
#include "fpapi_test.h"
C Set MSGLEN to the number of characters in the named event in MSGCODE
#define MSGLEN 11
#define MSGCODE PAPI_L1_DCM 
#define ERRCODE PAPI_EINVAL
       
      program strtest
      implicit integer (p)
      
      CHARACTER*(PAPI_MAX_STR_LEN) papistr
      CHARACTER*(PAPI_MAX_STR_LEN*2) papidblstr

      CHARACTER*(PAPI_MAX_STR_LEN) ckstr
      CHARACTER*(MSGLEN) invstr1
      CHARACTER*(MSGLEN+1) invstr2
      CHARACTER*(MSGLEN+2) invstr3
      CHARACTER*(MSGLEN-1) invstr4
      CHARACTER*(MSGLEN-2) invstr5

      integer check,lastchar
      integer code,papicode
      integer getstrlen
      external getstrlen
      integer tests_quiet, get_quiet
      external get_quiet

      tests_quiet = get_quiet()

      check=PAPI_VER_CURRENT
      call PAPIF_library_init(check)
      if ( check.NE.PAPI_VER_CURRENT) then
           call PAPIF_perror( 'PAPI_library_init' )
        call ftest_fail(__FILE__, __LINE__,
     . 'PAPI_library_init', check)
      end if

      code=MSGCODE

      if (tests_quiet .EQ. 0) then
         print *,'---------------------------------------------------'
         print *,'   Testing PAPIF_name_to_code/PAPIF_code_to_name   '
         print *,'---------------------------------------------------'
         print *,' These tests look up an event name and event code'
         print *,' On no occasion should a NULL character be found(+)'
         print *,' When strings are too short, the lookup should fail'
         print *
         print *,' Tests use the event code ',code
         print *
       end if

      lastchar=PAPI_MAX_STR_LEN
      call checkstr(code,ckstr,check,lastchar,tests_quiet)
      lastchar=getstrlen(ckstr)
 
      call checkstr(code,invstr1,check,lastchar,tests_quiet)

      call checkstr(code,invstr2,check,lastchar,tests_quiet)

      call checkstr(code,invstr3,check,lastchar,tests_quiet)

      call checkstr(code,invstr4,check,lastchar,tests_quiet)

      call checkstr(code,invstr5,check,lastchar,tests_quiet)

      if (tests_quiet .EQ. 0) then
         print *,'---------------------------------------------------'
         print *,'          Testing PAPIF_descr_event                '
         print *,'---------------------------------------------------'
         print *,' These tests should return a PAPI description for'
         print *,' various event names and argument shapes.'
         print *,' On no occasion should a NULL character be found(+)'
         print *

         print 200,'Test 1'
      end if

      papistr="  "
      papicode=PAPI_L1_DCM
      call test_papif_descr(papistr,papicode,papidblstr,
     .  check,tests_quiet)
      call checkcode(papicode,PAPI_L1_DCM,tests_quiet)

      if (tests_quiet .EQ. 0) then
         print *
         print 200,'Test 2'
      end if

      papistr="  "
      papicode=PAPI_L2_DCM
      call test_papif_descr(papistr,papicode,papidblstr,
     .  check,tests_quiet)
      call checkname(papistr,"PAPI_L2_DCM",tests_quiet)

      if (tests_quiet .EQ. 0) then
         print *
         print 200,'Test 3'
      end if

      invstr1="  "
      papicode=PAPI_L1_ICM
      call test_papif_descr(invstr1,papicode,papidblstr,
     .  check,tests_quiet)
      call checkcode(papicode,PAPI_L1_ICM,tests_quiet)

      if (tests_quiet .EQ. 0) then
          print *
          print 200,'Test 4'
      end if

      invstr1="  "
      papicode=PAPI_L2_ICM
      call test_papif_descr(invstr1,papicode,papidblstr,
     .  check,tests_quiet)
      call checkname(invstr1,"PAPI_L2_ICM",tests_quiet)

      if (tests_quiet .EQ. 0) then
         print *
         print 200,'Test 5  (This should get a truncated description)'
      end if

      invstr2="  "
      papicode=PAPI_L3_DCM
      call test_papif_descr(invstr2,papicode,invstr1,
     .  check,tests_quiet)
      call checkcode(papicode,PAPI_L3_DCM,tests_quiet)

      if (tests_quiet .EQ. 0) then
         print *
         print 200,'Test 6  (This should get a truncated description)'
      end if

      invstr2="  "
      papicode=PAPI_L3_ICM
      call test_papif_descr(invstr2,papicode,invstr1,
     .  check,tests_quiet)
      call checkname(invstr2,"PAPI_L3_ICM",tests_quiet)

      if (tests_quiet .EQ. 0) then
         print *
         print 200,'Test 7  (This should get a truncated name)'
      end if

      invstr4="  "
      papicode=PAPI_L1_DCM
      call test_papif_descr(invstr4,papicode,papistr,
     .  check,tests_quiet)
      if (tests_quiet .EQ. 0) then
         call checkname(invstr4,"PAPI_L1_DCM",tests_quiet)
      end if

 200  format(t1,a)

      if (tests_quiet .EQ. 0) then
         print *,'---------------------------------------------------'
         print *,'(+) Fortran implementations that do not provide the'
         print *,'    string argument length might show NULL '//
     .           'characters.'
         print *,'    This may or may not be OK depending on the '//
     .           'Fortran'
         print *,'    compiler. See papi_fwrappers.c and your Fortran'
         print *,'    compiler reference manual.'
      end if

      call ftests_pass(__FILE__)      
      end

      subroutine checkstr(incode,string,check,lastchar,quiet)
      implicit integer (P)
      integer incode
      integer check,lastchar, quiet
      character*(*) string
      integer code
      integer getstrlen
      external getstrlen

 100  format(t1,a,i4)

      if (quiet .EQ. 0) then
         print 100,"Testing string length ",len(string)
         if(len(string).lt.lastchar)then
            print *,'This call should return an error code.'
         end if
      end if

      code=incode
      call PAPIF_event_code_to_name(code,string,check)
      if(check.ne.PAPI_OK)then
        if (len(string).ge.lastchar)then
          call ftest_fail(__FILE__, __LINE__,
     . 'PAPIF_event_code_to_name', check)
        else
        if (quiet .EQ. 0) then
           call PAPIF_perror( 'PAPIF_event_code_to_name' )
           print *,'*ERROR* '
           print *,'******* '//'Error in checkstr using '//
     $ 'PAPIF_event_code_to_name'
        end if
       end if
      end if

 200  format(t1,a,'"',a,'"')
      if (quiet .EQ. 0) then
         print 200,'The event name is: ',string(1:getstrlen(string))
      end if

      call PAPIF_event_name_to_code(string,code,check)
      if(check.ne.PAPI_OK)then
      if (len(string).ge.lastchar)then
          call ftest_fail(__FILE__, __LINE__,
     . 'PAPIF_event_name_to_code', check)
      else
      if (quiet .EQ. 0) then
         call PAPIF_perror( 'PAPIF_event_name_to_code' )
         print *,'*ERROR* '
         print *,'******* '//'Error in checkstr using '//
     $   'PAPIF_event_name_to_code'
      end if
      end if
      end if
      
      call findnull(string,quiet)

      if (quiet .EQ. 0) then
         print *
      end if

      return
      end

      subroutine test_papif_descr(name,code,string,check,quiet)
      implicit integer (P)
      integer code,count,flags
      integer check,quiet
      character*(*) name,string

      character*(PAPI_MAX_STR_LEN) label,note
      integer getstrlen
      external getstrlen

C      This API was deprecated with PAPI 3
C      call PAPIF_describe_event(name,code,string,check)
      call PAPIF_get_event_info(code,name,string,label,count,
     $     note,flags,check)
 100  format(t1,a,'"',a,'"')
      if (quiet .EQ. 0) then
        print 100,'The event description is: ',
     $    string(1:getstrlen(string))
      end if

      if(check.ne.PAPI_OK)then
      if (quiet .EQ. 0) then
          call PAPIF_perror( 'PAPI_get_event_info' )
          print *,'*ERROR* '
          print *,'******* '//'Error in test_papif_descr using '//
     $      'PAPIF_get_event_info'
      else
          call ftest_fail(__FILE__, __LINE__,
     . 'PAPIF_get_event_info', check)
        end if
      end if

      call findnull(string,quiet)
      call findnull(name,quiet)

      return
      end

      integer function getstrlen(string)
      implicit integer (P)
      character*(*) string
      integer i

      do i=len(string),1,-1
        if(string(i:i).ne.' ') then 
          goto 20
        end if
      end do
      getstrlen=0
      return

 20   continue
      getstrlen=i
      return
      end

      subroutine findnull(string,quiet)
      implicit integer (P)
      integer quiet,i
      character*(*) string

      i=index(string,char(0))
      if(i.gt.0)then
        if(quiet.EQ.0)then
           print *,'NULL character found in string!!!'
        else
            call ftest_fail(__FILE__, __LINE__,
     . 'NULL character found in string!!!', 0)
        end if
      end if

      return
      end


      subroutine checkcode(code,check,quiet)
      implicit integer (P)
      integer code
      integer check,quiet

      if(code.ne.check)then
      if(quiet.EQ.0)then
         print 100,'Code look up failed?'
      else
          call ftest_fail(__FILE__, __LINE__,
     .   'Code look up failed?', 0)
        end if
      end if
 100  format(t2,a)

      return
      end

      subroutine checkname(name,check,quiet)
      implicit integer (P)
      character*(*) name
      character*(*) check
      integer i,quiet
      integer getstrlen

      i=getstrlen(name)
      if(name(1:i).ne.check)then
        if (quiet .eq. 0) then
           print 100,'PAPI name incorrect?'
           print 110,'Got:      ',name(1:i)
           print 110,'Expected: ',check
        else
          call ftest_fail(__FILE__, __LINE__,
     .  'PAPI name incorrect?', 0)
        end if
      end if

 100  format(t2,a)
 110  format(a12,'"',a,'"')

      return
      end
