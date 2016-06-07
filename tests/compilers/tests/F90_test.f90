program main
  implicit none
  
  integer :: y

  y = mystery_machine(0)

  if(y .ne. 42) then
     write(*,*) 'Error with F90 runtime'
     call exit(1)
  endif

contains

integer function mystery_machine(x)
  implicit none
  integer, intent(in) :: x

  mystery_machine = x + 42
end function mystery_machine

end program main
