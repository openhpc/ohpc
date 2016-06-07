#include "config.f"

module MQEoptions

  type :: MQEoptions_t
     character*256 :: infile = ' '
     integer      :: npool
     integer      :: ntg  
     integer      :: ndiag
  end type MQEoptions_t

  type( MQEoptions_t ) :: MQEo

end module MQEoptions

subroutine MQEoptions_read()

  use MQEoptions, only: MQEo
  integer, DECLARE_IARGC :: iargc
  
  integer :: iarg, narg
  character*80 :: arg_i

  !set default options
  MQEo%infile = "pw.in"
  MQEo%npool  = 1
  MQEo%ntg    = 1
  MQEo%ndiag  = 0

  iarg = 1
  narg = iargc()
  do while( iarg .le. narg )

     call getarg( iarg, arg_i )
     
     select case( arg_i )

     case("-in")
        iarg = iarg + 1
        call getarg( iarg, arg_i )
        read(arg_i,*) MQEo%infile

     case("-npool")
        iarg = iarg + 1
        call getarg( iarg, arg_i )
        read(arg_i,*) MQEo%npool

     case("-ntg")
        iarg = iarg + 1
        call getarg( iarg, arg_i )
        read(arg_i,*) MQEo%ntg

     case("-ndiag")
        iarg = iarg + 1
        call getarg( iarg, arg_i )
        read(arg_i,*) MQEo%ndiag 

     case default
        write(*,*)"Error: Unrecognized option: ", trim(arg_i)
        stop

     end select

     iarg = iarg + 1
  end do
  
end subroutine MQEoptions_read
