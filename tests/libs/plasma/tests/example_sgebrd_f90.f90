!
!     Copyright © 2011 The Numerical Algorithms Group Ltd. All rights reserved.
!   
!     Redistribution and use in source and binary forms, with or without
!     modification, are permitted provided that the following conditions are
!     met:
!     - Redistributions of source code must retain the above copyright notice,
!       this list of conditions, and the following disclaimer.
!     - Redistributions in binary form must reproduce the above copyright
!       notice, this list of conditions and the following disclaimer listed in
!       this license in the documentation and/or other materials provided with
!       the distribution.
!     - Neither the name of the copyright holders nor the names of its
!       contributors may be used to endorse or promote products derived from
!       this software without specific prior written permission.
!     
!     This software is provided by the copyright holders and contributors "as
!     is" and any express or implied warranties, including, but not limited
!     to, the implied warranties of merchantability and fitness for a
!     particular purpose are disclaimed. in no event shall the copyright owner
!     or contributors be liable for any direct, indirect, incidental, special,
!     exemplary, or consequential damages (including, but not limited to,
!     procurement of substitute goods or services; loss of use, data, or
!     profits; or business interruption) however caused and on any theory of
!     liability, whether in contract, strict liability, or tort (including
!     negligence or otherwise) arising in any way out of the use of this
!     software, even if advised of the possibility of such damage.
!
! @file example_sgebrd_f90.f90
!
!  PLASMA Fortran 90 example using Fortran 2003 ISO C bindings
!  PLASMA is a software package provided by Univ. of Tennessee,
!  Univ. of California Berkeley and Univ. of Colorado Denver
!
! @version 2.8.0
! @author Numerical Algorithms Group
! @date 2012-08-14
!
! ./example_sgebrd_f90 < gebrd_example.d
!

    program example_sgebrd_f90
       use plasma
       use iso_c_binding
       implicit none
       integer, parameter              :: wp = kind(0.0)
       integer, parameter              :: nin = 5, nout = 6
       integer                         :: info, lda, m, n
       real(kind=wp), allocatable      :: a(:,:), d(:), e(:)
       real(kind=wp)                   :: q(1), p(1)
       character                       :: sched
       integer                         :: npcores, nb, ib
       logical                         :: atun
       intrinsic                       :: min
       intrinsic                       :: random_number
       type(c_ptr)                     :: desc_t

       ! Read and initialise problem
       write(*,*) 'Enter the dimensions M N:'
       read (nin,*) m, n
       write(*,*) 'Do you want autotuning ? [.false./.true.]:'
       read (nin,*) atun
       write(*,*) 'Enter NB IB:'
       read (nin,*) nb, ib
       write(*,*) 'Enter the number of cores:'
       read (nin,*) npcores
       write(*,*) 'Static/dynamic scheduler? [s/d]:'
       read (nin,*) sched
       lda = m
       allocate (a(lda,n),d(min(m,n)),e(min(m,n)-1))
       call random_number(a)

       ! Initialize PLASMA
       call plasma_init(npcores,info)

       ! Choose whether to perform autotuning
       if (.not. atun) then
          call plasma_disable(plasma_autotuning,info)
          call plasma_set(plasma_tile_size,nb,info)
          call plasma_set(plasma_inner_block_size,ib,info)
       else
          call plasma_enable(plasma_autotuning,info)
       end if

       ! Choose whether to use dynamic scheduling
       if (sched=='d') then
          call plasma_set(plasma_scheduling_mode,plasma_static_scheduling,info)
       end if

       ! Create workspace pointed to by desc_t, which is type(c_ptr)
       call plasma_alloc_workspace_sgebrd(m,n,desc_t,info)

       write(nout,*) "Sanity check in: ",a(1:min(5,m),1)

       ! Call the routine - pass in original column layout array A
       call plasma_sgebrd(PlasmaNoVec, PlasmaNoVec, m, n, a, lda, d, e, &
            desc_t, q, 1, p, 1, info)
       if (info/=0) write (nout,*) 'plasma call failed'

       write(nout,*) "Sanity check out: ",d(1:min(5,m))

       ! Close down
       call plasma_finalize(info)

    end program example_sgebrd_f90
