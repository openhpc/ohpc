!
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!-----------------------------------------------------------------------
subroutine usnldiag (h_diag, s_diag)
  !-----------------------------------------------------------------------
  !
  !    add nonlocal pseudopotential term to diagonal part of Hamiltonian
  !    compute the diagonal part of the S matrix
  !
  USE kinds, ONLY: DP
  USE ions_base,  ONLY : nat, ityp, ntyp => nsp
  USE wvfct, ONLY: npw, npwx
  USE lsda_mod, ONLY: current_spin 
  USE uspp,  ONLY: deeq, vkb, qq, qq_so, deeq_nc
  USE uspp_param, ONLY: upf, nh, newpseudo
  USE spin_orb, ONLY: lspinorb
  !
  implicit none
  
  integer, parameter :: npol=1 !substitute for noncollin_module

  !    here the dummy variables
  !
  real(DP) :: h_diag (npwx,npol), s_diag (npwx,npol)
  ! input/output: the diagonal part of the hamiltonian
  ! output: the diagonal part of the S matrix
  !
  !   and here the local variables
  !
  integer :: ikb, jkb, ih, jh, na, nt, ig, ijkb0, ipol
  ! counters
  complex(DP) :: ps1(2), ps2(2), ar
  !
  ! initialise s_diag
  !
  s_diag = 1.d0
  !
  !    multiply on projectors
  !
  ijkb0 = 0
  do nt = 1, ntyp
     do na = 1, nat
        if (ityp (na) == nt) then
           do ih = 1, nh (nt)
              ikb = ijkb0 + ih
              if (lspinorb) then
                 ps1(1) = deeq_nc (ih, ih, na, 1)
                 ps1(2) = deeq_nc (ih, ih, na, 4)
                 ps2(1) = qq_so(ih, ih, 1, nt)
                 ps2(2) = qq_so(ih, ih, 4, nt)
              else
                 ps1(1) = deeq (ih, ih, na, current_spin)
                 ps2(1) = qq (ih, ih, nt)
              end if
              do ipol =1, npol
                 do ig = 1, npw
                    ar = vkb (ig, ikb)*CONJG(vkb (ig, ikb))
                    h_diag (ig,ipol) = h_diag (ig,ipol) + ps1(ipol) * ar
                    s_diag (ig,ipol) = s_diag (ig,ipol) + ps2(ipol) * ar
                 enddo
              enddo
              if ( newpseudo (nt) ) then
                 do jh = 1, nh (nt)
                    if (jh.ne.ih) then
                       jkb = ijkb0 + jh
                       if (lspinorb) then
                          ps1(1) = deeq_nc (ih, jh, na, 1)
                          ps1(2) = deeq_nc (ih, jh, na, 4)
                          ps2(1) = qq_so(ih, jh, 1, nt)
                          ps2(2) = qq_so(ih, jh, 4, nt)
                       else
                          ps1(1) = deeq (ih, jh, na, current_spin)
                          ps2(1) = qq (ih, jh, nt)
                       end if
                       do ipol = 1, npol
                          do ig = 1, npw
                             ar = vkb (ig, ikb) *CONJG( vkb (ig, jkb))
                             h_diag (ig,ipol) = h_diag (ig,ipol) + &
                                  ps1(ipol) * ar
                             s_diag (ig,ipol) = s_diag (ig,ipol) + &
                                  ps2(ipol) * ar
                          enddo
                       enddo
                    endif
                 enddo
              endif
           enddo
           ijkb0 = ijkb0 + nh (nt)
        endif
     enddo
  enddo

  return
end subroutine usnldiag
