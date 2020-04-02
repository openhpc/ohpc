! Coarray "Hello, world!" test program
!
! Copyright (c) 2012-2014, Sourcery, Inc.
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!     * Redistributions of source code must retain the above copyright
!       notice, this list of conditions and the following disclaimer.
!     * Redistributions in binary form must reproduce the above copyright
!       notice, this list of conditions and the following disclaimer in the
!       documentation and/or other materials provided with the distribution.
!     * Neither the name of the Sourcery, Inc., nor the
!       names of its contributors may be used to endorse or promote products
!       derived from this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
! ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
! WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL SOURCERY, INC., BE LIABLE FOR ANY
! DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
! (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
! ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
! SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

! Robodoc header:
!****e* tests/coarrayHelloWorld/hello_multiverse.F90
! NAME
!   hello_multiverse
! SYNOPSIS
!   Demonstrate coarray communication via a scalar character coarray.
! INPUTS
!   None.
! OUTPUTS
!   Test pass or failure.
!******

program hello_multiverse
  implicit none
  integer, parameter :: MAX_STRING=100
  character(len=MAX_STRING) :: greeting[*] ! Scalar coarray
  integer image
  write(greeting,"(2(a,i2))") "Greetings from image ",this_image()," of ",num_images()
  sync all ! Barrier
  if (this_image()==1) then
    do concurrent (image=1:num_images())
      print *,greeting[image]
    end do
    block
      integer, parameter :: expected_location=23,max_single_digit=9
      do image=2,min(num_images(),max_single_digit)
        ! Verify that the greetings of images 1-9 have their image number at the expected location:
        if (scan(greeting[image],set="123456789")/=expected_location) error stop "Test failed."
      end do
    end block
    print *,"Test passed."
  end if
end program
