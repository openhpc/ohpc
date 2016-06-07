!
! Copyright (C) 2004-2011 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!-------------------------------------------------------------------
module funct
!-------------------------------------------------------------------
! This module contains data defining the DFT functional in use 
! and a number of functions and subroutines to manage them.
! Data are PRIVATE and are accessed and set only by function calls.
! Basic drivers to compute XC quantities are also included.
!  
!  setting routines:   set_dft_from_name (previously which_dft)
!                      set_dft_from_indices
!                      enforce_input_dft
!                      start_exx
!                      stop_exx
!                      set_finite_size_volume
!  retrieve functions: get_dft_name
!                      get_iexch
!                      get_icorr
!                      get_igcx
!                      get_igcc
!                      get_exx_fraction
!                      dft_name
!                      write_dft_name
!  logical functions:  dft_is_gradient
!                      dft_is_meta
!                      dft_is_hybrid
!                      dft_is_nonlocc
!                      exx_is_active
!                      dft_has_finite_size_correction
!
!  XC computation drivers: xc, xc_spin, gcxc, gcx_spin, gcc_spin, gcc_spin_more
!  derivatives of XC computation drivers: dmxc, dmxc_spin, dmxc_nc, dgcxc,
!                                         dgcxc_spin
!
  USE io_global, ONLY: stdout
  USE kinds,     ONLY: DP
  IMPLICIT NONE
  PRIVATE
  SAVE
  ! subroutines/functions managing dft name and indices
  PUBLIC  :: set_dft_from_indices, set_dft_from_name
  PUBLIC  :: enforce_input_dft, write_dft_name, dft_name
  PUBLIC  :: get_dft_name, get_iexch, get_icorr, get_igcx, get_igcc, get_inlc
  PUBLIC  :: dft_is_gradient

  ! additional subroutines/functions for hybrid functionals
  ! additional subroutines/functions for finite size corrections
  ! driver subroutines computing XC
  PUBLIC  :: xc, xc_spin, gcxc, gcx_spin, gcc_spin 
  PUBLIC  :: dmxc, dmxc_spin 
  PUBLIC  :: dgcxc, dgcxc_spin
  !
  ! PRIVATE variables defining the DFT functional
  !
  PRIVATE :: dft, dft_shortname, iexch, icorr, igcx, igcc, inlc
  PRIVATE :: discard_input_dft
  PRIVATE :: isgradient, ismeta, ishybrid
  PRIVATE :: exx_fraction, exx_started
  PRIVATE :: has_finite_size_correction, &
             finite_size_cell_volume,  finite_size_cell_volume_set 
  !
  character (len=25) :: dft = 'not set'
  character (len=6)  :: dft_shortname = ' '
  !
  ! dft is the exchange-correlation functional, described by
  ! one of the following keywords ("dft_shortname"):
  !              "pz"    = "sla+pz"            = Perdew-Zunger LDA
  !              "pbe"   = "sla+pw+pbx+pbc"    = PBE
  ! References:
  !              pz      J.P.Perdew and A.Zunger, PRB 23, 5048 (1981) 
  !              pbe     J.P.Perdew, K.Burke, M.Ernzerhof, PRL 77, 3865 (1996)
  !
  integer, parameter:: notset = -1
  !
  integer :: iexch = notset
  integer :: icorr = notset
  integer :: igcx  = notset
  integer :: igcc  = notset
  integer :: inlc  = notset
  real(DP):: exx_fraction = 0.0_DP
  real(DP):: screening_parameter = 0.0_DP
  logical :: isgradient  = .false.
  logical :: ismeta      = .false.
  logical :: ishybrid    = .false.
  logical :: exx_started = .false.
  logical :: has_finite_size_correction = .false.
  logical :: finite_size_cell_volume_set = .false.
  real(DP):: finite_size_cell_volume = notset
  
  logical :: isnonlocc       = .false.

  logical :: discard_input_dft = .false.
  !
  ! internal indices for exchange-correlation
  !    iexch: type of exchange
  !    icorr: type of correlation
  !    igcx:  type of gradient correction on exchange
  !    igcc:  type of gradient correction on correlation
  !    inlc:  type of non local correction on correlation
  !
  !    ismeta: .TRUE. if gradient correction is of meta-gga type
  !    ishybrid: .TRUE. if the xc functional is an HF+DFT hybrid like
  !              PBE0, B3LYP, HSE or HF itself
  !
  ! see comments above and routine "set_dft_from_name" below 
  !
  ! data
  integer :: nxc, ncc, ngcx, ngcc, ncnl

  parameter (nxc = 8, ncc =11, ngcx =16, ngcc = 10, ncnl=2)

  character (len=4) :: exc, corr
  character (len=4) :: gradx, gradc, nonlocc
  dimension exc (0:nxc), corr (0:ncc), gradx (0:ngcx), gradc (0: ngcc), nonlocc (0: ncnl)

  data exc / 'NOX', 'SLA', 'SL1', 'RXC', 'OEP', 'HF', 'PB0X', 'B3LP', 'KZK' /
  data corr / 'NOC', 'PZ', 'VWN', 'LYP', 'PW', 'WIG', 'HL', 'OBZ', &
              'OBW', 'GL' , 'B3LP', 'KZK' /
  data gradx / 'NOGX', 'B88', 'GGX', 'PBX',  'RPB', 'HCTH', 'OPTX',&
               'META', 'PB0X', 'B3LP','PSX', 'WCX', 'HSE', 'RW86', 'PBE', &
               'TPSS', 'C09X' / 

  data gradc / 'NOGC', 'P86', 'GGC', 'BLYP', 'PBC', 'HCTH', 'META',&
                'B3LP', 'PSC', 'PBE', 'TPSS' / 

  data nonlocc / '    ', 'VDW1', 'VDW2' / 

CONTAINS
  !-----------------------------------------------------------------------
  subroutine set_dft_from_name( dft_ )
    !-----------------------------------------------------------------------
    !
    ! translates a string containing the exchange-correlation name
    ! into internal indices iexch, icorr, igcx, igcc
    !
    implicit none
    ! input
    character(len=*)               :: dft_
    ! local
    integer :: len, l, i
    character (len=50):: dftout
    logical :: dft_defined = .false.
    logical, external :: matches
    character (len=1), external :: capital
    integer ::  save_iexch, save_icorr, save_igcx, save_igcc, save_inlc
    
    !
    !
    ! Exit if discard_input_dft
    !
    if ( discard_input_dft ) return
    !
    ! save current status of XC indices
    !
    save_iexch = iexch
    save_icorr = icorr
    save_igcx  = igcx
    save_igcc  = igcc
    save_inlc  = inlc
    !
    ! convert to uppercase
    !
    len = len_trim(dft_)
    dftout = ' '
    do l = 1, len
       dftout (l:l) = capital (dft_(l:l) )
    enddo
    !
    ! ----------------------------------------------
    ! FIRST WE CHECK ALL THE SPECIAL NAMES
    ! Note: comparison is now done via exact matching
    !       not using function "matches"
    ! ----------------------------------------------
    !
    if ('PBE' .EQ. TRIM(dftout) ) then
    ! special case : PBE
       call set_dft_value (iexch,1) !Default    
       call set_dft_value (icorr,4)
       call set_dft_value (igcx, 3)
       call set_dft_value (igcc, 4)
       call set_dft_value (inlc,0) !Default       
       dft_defined = .true.
       

    ! special cases : PZ  (LDA is equivalent to PZ)
    else IF (('PZ' .EQ. TRIM(dftout) ).OR.('LDA' .EQ. TRIM(dftout) )) THEN
       call set_dft_value (iexch,1) 
       call set_dft_value (icorr, 1) 
       CALL set_dft_value( igcx,  0)
       call set_dft_value (igcc, 0)      
       call set_dft_value (inlc,0)    
       dft_defined = .true.
           
    else
       
       call errore('set_dft_from_name','only LDA and PZ functionals are supported in mini_DFT',1)

    END IF

    !
    ! Fill variables and exit
    !
    dft = dftout

    dftout = exc (iexch) //'-'//corr (icorr) //'-'//gradx (igcx) //'-' &
         &//gradc (igcc) //'-'// nonlocc(inlc)


    call set_auxiliary_flags
    !
    ! check dft has not been previously set differently 
    !
    if (save_iexch .ne. notset .and. save_iexch .ne. iexch) then
       write (stdout,*) iexch, save_iexch
       call errore('set_dft_from_name',' conflicting values for iexch',1)
    end if
    if (save_icorr .ne. notset .and. save_icorr .ne. icorr) then
       write (stdout,*) icorr, save_icorr
       call errore('set_dft_from_name',' conflicting values for icorr',1)
    end if
    if (save_igcx .ne. notset .and. save_igcx .ne. igcx) then
       write (stdout,*) igcx, save_igcx
       call errore('set_dft_from_name',' conflicting values for igcx',1)
    end if
    if (save_igcc .ne. notset .and. save_igcc .ne. igcc) then
       write (stdout,*) igcc, save_igcc
       call errore('set_dft_from_name',' conflicting values for igcc',1)
    end if
    if (save_inlc .ne. notset .and. save_inlc .ne. inlc) then
       write (stdout,*) inlc, save_inlc
       call errore('set_dft_from_name',' conflicting values for inlc',1)
    end if

    return
  end subroutine set_dft_from_name
  !
  !-----------------------------------------------------------------------
  subroutine set_auxiliary_flags
    !-----------------------------------------------------------------------
    ! set logical flags describing the complexity of the xc functional
    ! define the fraction of exact exchange used by hybrid fuctionals
    !
    logical, external :: matches

    !! Reversed as before VDW
    isgradient =  ( (igcx > 0) .or. ( igcc > 0) )

    isnonlocc = (inlc > 0)

    ismeta     =  (igcx == 7)

    ishybrid = ( exx_fraction /= 0.0_DP )

    has_finite_size_correction = ( iexch==8 .or. icorr==11)

    return
  end subroutine set_auxiliary_flags
  !
  !-----------------------------------------------------------------------
  subroutine set_dft_value (m, i)
    !-----------------------------------------------------------------------
    !
    implicit none
    integer :: m, i
    ! local

    if ( m /= notset .and. m /= i) then
         write(*, '(A,2I4)') "parameters", m, i
         call errore ('set_dft_value', 'two conflicting matching values', 1)
    end if
    m = i
    return

  end subroutine set_dft_value

  !-----------------------------------------------------------------------
  subroutine enforce_input_dft (dft_, nomsg)
    !
    ! translates a string containing the exchange-correlation name
    ! into internal indices and force any subsequent call to set_dft_from_name
    ! to return without changing them
    !
    implicit none
    character(len=*), intent(in) :: dft_
    logical, intent(in), optional :: nomsg

     call set_dft_from_name (dft_)
     if (dft == 'not set') call errore('enforce_input_dft','cannot fix unset dft',1)
     discard_input_dft = .true.

     if ( present (nomsg) ) return

     write (stdout,'(/,5x,a)') "IMPORTANT: XC functional enforced from input :"
     call write_dft_name
     write (stdout,'(5x,a)') "Any further DFT definition will be discarded"
     write (stdout,'(5x,a/)') "Please, verify this is what you really want"

     return
  end subroutine enforce_input_dft
 
  !-----------------------------------------------------------------------
  function get_iexch ()
     integer get_iexch
     get_iexch = iexch
     return
  end function get_iexch
  !-----------------------------------------------------------------------
  function get_icorr ()
     integer get_icorr
     get_icorr = icorr
     return
  end function get_icorr
  !-----------------------------------------------------------------------
  function get_igcx ()
     integer get_igcx
     get_igcx = igcx
     return
  end function get_igcx
  !-----------------------------------------------------------------------
  function get_igcc ()
     integer get_igcc
     get_igcc = igcc
     return
  end function get_igcc
  !-----------------------------------------------------------------------
  function get_inlc ()
     integer get_inlc
     get_inlc = inlc
     return
  end function get_inlc
  function get_dft_name ()
     character (len=25) :: get_dft_name
     get_dft_name = dft
     return
  end function get_dft_name
  !-----------------------------------------------------------------------
  function dft_is_gradient ()
     logical :: dft_is_gradient
     dft_is_gradient = isgradient
     return
  end function dft_is_gradient
  
  !-----------------------------------------------------------------------
  subroutine set_dft_from_indices(iexch_,icorr_,igcx_,igcc_, inlc_)
     integer :: iexch_, icorr_, igcx_, igcc_, inlc_
     if ( discard_input_dft ) return
     if (iexch == notset) iexch = iexch_
     if (iexch /= iexch_) then
        write (stdout,*) iexch, iexch_
        call errore('set_dft',' conflicting values for iexch',1)
     end if
     if (icorr == notset) icorr = icorr_
     if (icorr /= icorr_) then
        write (stdout,*) icorr, icorr_
        call errore('set_dft',' conflicting values for icorr',1)
     end if
     if (igcx  == notset) igcx = igcx_
     if (igcx /= igcx_) then
        write (stdout,*) igcx, igcx_
        call errore('set_dft',' conflicting values for igcx',1)
     end if
     if (igcc  == notset) igcc = igcc_
     if (igcc /= igcc_) then
        write (stdout,*) igcc, igcc_
        call errore('set_dft',' conflicting values for igcc',1)
     end if
     if (inlc  == notset) inlc = inlc_
     if (inlc /= inlc_) then
        write (stdout,*) inlc, inlc_
        call errore('set_dft',' conflicting values for inlc',1)
     end if
     dft = exc (iexch) //'-'//corr (icorr) //'-'//gradx (igcx) //'-' &
           &//gradc (igcc)//'-'//nonlocc (inlc)
     ! WRITE( stdout,'(a)') dft
     call set_auxiliary_flags
     return
  end subroutine set_dft_from_indices
  !---------------------------------------------------------------------
  subroutine dft_name(iexch_, icorr_, igcx_, igcc_, inlc_, longname_, shortname_)
  !---------------------------------------------------------------------
  ! convert the four indices iexch, icorr, igcx, igcc
  ! into user-readable strings
  !
  implicit none
  integer iexch_, icorr_, igcx_, igcc_, inlc_
  character (len=6) :: shortname_
  character (len=25):: longname_
  !
  if (iexch_==1.and.igcx_==0.and.igcc_==0) then
     shortname_ = corr(icorr_)
  else if (iexch_==1.and.icorr_==4.and.igcx_==3.and.igcc_==4) then
     shortname_ = 'PBE'
  else
     shortname_ = ' '
  end if
  write(longname_,'(5a5)') exc(iexch_),corr(icorr_),gradx(igcx_),gradc(igcc_),nonlocc(inlc_)
  
  return
end subroutine dft_name

subroutine write_dft_name
!-----------------------------------------------------------------------
   WRITE( stdout, '(5X,"Exchange-correlation      = ",A, &
        &  " (",5I2,")")') TRIM( dft ), iexch, icorr, igcx, igcc, inlc
   return
end subroutine write_dft_name

!
!-----------------------------------------------------------------------
!-------  LDA DRIVERS --------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
subroutine xc (rho, ex, ec, vx, vc)
  !-----------------------------------------------------------------------
  !     lda exchange and correlation functionals - Hartree a.u.
  !
  !     exchange   :  Slater, relativistic Slater
  !     correlation:  Ceperley-Alder (Perdew-Zunger parameters)
  !
  !     input : rho=rho(r)
  !     definitions: E_x = \int E_x(rho) dr, E_x(rho) = rho\epsilon_c(rho)
  !                  same for correlation
  !     output: ex = \epsilon_x(rho) ( NOT E_x(rho) )
  !             vx = dE_x(rho)/drho  ( NOT d\epsilon_x(rho)/drho )
  !             ec, vc as above for correlation
  !
  implicit none

  real(DP) :: rho, ec, vc, ex, vx
  !
  real(DP), parameter :: small = 1.E-10_DP,  third = 1.0_DP / 3.0_DP, &
       pi34 = 0.6203504908994_DP  ! pi34=(3/4pi)^(1/3)
  real(DP) :: rs
  !
  if (rho <= small) then
     ec = 0.0_DP
     vc = 0.0_DP
     ex = 0.0_DP
     vx = 0.0_DP
     return
  else
     rs = pi34 / rho**third
     ! rs as in the theory of metals: rs=(3/(4pi rho))^(1/3)
  endif
  !..exchange
  if (iexch == 1) THEN             !  'sla'
     call slater (rs, ex, vx)
  else
     ex = 0.0_DP
     vx = 0.0_DP
  endif
  !..correlation
  if (icorr == 1) then
     call pz (rs, 1, ec, vc)
  elseif (icorr == 4) then
     call pw (rs, 1, ec, vc)
  else
     ec = 0.0_DP
     vc = 0.0_DP
  endif
  !
  return
end subroutine xc
!!!!!!!!!!!!!!SPIN
!-----------------------------------------------------------------------
subroutine xc_spin (rho, zeta, ex, ec, vxup, vxdw, vcup, vcdw)
  !-----------------------------------------------------------------------
  !     lsd exchange and correlation functionals - Hartree a.u.
  !
  !     exchange  :  Slater (alpha=2/3)
  !     correlation: Ceperley & Alder (Perdew-Zunger parameters)
  !                  Perdew & Wang
  !
  !     input : rho = rhoup(r)+rhodw(r)
  !             zeta=(rhoup(r)-rhodw(r))/rho
  !
  implicit none

  real(DP) :: rho, zeta, ex, ec, vxup, vxdw, vcup, vcdw
  !
  real(DP), parameter :: small= 1.E-10_DP, third = 1.0_DP/3.0_DP, &
       pi34= 0.6203504908994_DP ! pi34=(3/4pi)^(1/3)
  real(DP) :: rs
  !
  if (rho <= small) then
     ec = 0.0_DP
     vcup = 0.0_DP
     vcdw = 0.0_DP
     ex = 0.0_DP
     vxup = 0.0_DP
     vxdw = 0.0_DP
     return
  else
     rs = pi34 / rho**third
  endif
  !..exchange
  IF (iexch == 1) THEN      ! 'sla'
     call slater_spin (rho, zeta, ex, vxup, vxdw)
  ELSE
     ex = 0.0_DP
     vxup = 0.0_DP
     vxdw = 0.0_DP
  ENDIF
  !..correlation
if (icorr == 1) then
     call pz_spin (rs, zeta, ec, vcup, vcdw)
  elseif (icorr == 4) then
     call pw_spin (rs, zeta, ec, vcup, vcdw)
  else
     call errore ('lsda_functional (xc_spin)', 'not implemented', icorr)
  endif
  !
  return
end subroutine xc_spin
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!------- GRADIENT CORRECTIONS DRIVERS ----------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
subroutine gcxc (rho, grho, sx, sc, v1x, v2x, v1c, v2c)
  !-----------------------------------------------------------------------
  !     gradient corrections for exchange and correlation - Hartree a.u.
  !     See comments at the beginning of module for implemented cases
  !
  !     input:  rho, grho=|\nabla rho|^2
  !     definition:  E_x = \int E_x(rho,grho) dr
  !     output: sx = E_x(rho,grho)
  !             v1x= D(E_x)/D(rho)
  !             v2x= D(E_x)/D( D rho/D r_alpha ) / |\nabla rho|
  !             sc, v1c, v2c as above for correlation
  !
  implicit none

  real(DP) :: rho, grho, sx, sc, v1x, v2x, v1c, v2c
  real(DP) :: sxsr, v1xsr, v2xsr
  real(DP), parameter:: small = 1.E-10_DP

  ! exchange
  if (rho <= small) then
     sx = 0.0_DP
     v1x = 0.0_DP
     v2x = 0.0_DP
  elseif (igcx == 3) then
     call pbex (rho, grho, 1, sx, v1x, v2x)
  else
     sx = 0.0_DP
     v1x = 0.0_DP
     v2x = 0.0_DP
  endif
  ! correlation
  if (rho.le.small) then
     sc = 0.0_DP
     v1c = 0.0_DP
     v2c = 0.0_DP
  elseif (igcc == 4) then
     call pbec (rho, grho, 1, sc, v1c, v2c)
  else
     sc = 0.0_DP
     v1c = 0.0_DP
     v2c = 0.0_DP
  endif
  !
  return
end subroutine gcxc
!
!!!!!!!!!!!!!!SPIN
!-----------------------------------------------------------------------
subroutine gcx_spin (rhoup, rhodw, grhoup2, grhodw2, &
                     sx, v1xup, v1xdw, v2xup, v2xdw)
  !-----------------------------------------------------------------------
  !     gradient corrections for exchange - Hartree a.u.
  !
  implicit none
  !
  !     dummy arguments
  !
  real(DP) :: rhoup, rhodw, grhoup2, grhodw2, sx, v1xup, v1xdw, &
       v2xup, v2xdw
  ! up and down charge
  ! up and down gradient of the charge
  ! exchange and correlation energies
  ! derivatives of exchange wr. rho
  ! derivatives of exchange wr. grho
  !
  real(DP) :: sxsr, v1xupsr, v2xupsr, v1xdwsr, v2xdwsr
  real(DP), parameter :: small = 1.E-10_DP
  real(DP) :: rho, sxup, sxdw
  integer :: iflag
  !
  !
  ! exchange
  rho = rhoup + rhodw
  if (rho <= small .or. igcx == 0) then
     sx = 0.0_DP
     v1xup = 0.0_DP
     v2xup = 0.0_DP
     v1xdw = 0.0_DP
     v2xdw = 0.0_DP
  elseif (igcx == 3 .or. igcx == 4 .or. igcx == 8 .or. &
          igcx == 10 .or. igcx == 12) then
     ! igcx=3: PBE, igcx=4: revised PBE, igcx=8 PBE0, igcx=10: PBEsol
     ! igcx=12: HSE
     if (rhoup > small .and. sqrt (abs (grhoup2) ) > small) then
        call pbex (2.0_DP * rhoup, 4.0_DP * grhoup2, iflag, sxup, v1xup, v2xup)
     else
        sxup = 0.0_DP
        v1xup = 0.0_DP
        v2xup = 0.0_DP
     endif
     if (rhodw > small .and. sqrt (abs (grhodw2) ) > small) then
        call pbex (2.0_DP * rhodw, 4.0_DP * grhodw2, iflag, sxdw, v1xdw, v2xdw)
     else
        sxdw = 0.0_DP
        v1xdw = 0.0_DP
        v2xdw = 0.0_DP
     endif
     sx = 0.5_DP * (sxup + sxdw)
     v2xup = 2.0_DP * v2xup
     v2xdw = 2.0_DP * v2xdw
     if (igcx == 8 .and. exx_started ) then
       sx = (1.0_DP - exx_fraction) * sx
       v1xup = (1.0_DP - exx_fraction) * v1xup
       v1xdw = (1.0_DP - exx_fraction) * v1xdw
       v2xup = (1.0_DP - exx_fraction) * v2xup
       v2xdw = (1.0_DP - exx_fraction) * v2xdw
     end if
  else
     call errore ('gcx_spin', 'not implemented', igcx)
  endif
  !
  return
end subroutine gcx_spin
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
subroutine gcc_spin (rho, zeta, grho, sc, v1cup, v1cdw, v2c)
  !-----------------------------------------------------------------------
  !     gradient corrections for correlations - Hartree a.u.
  !     Implemented:  Perdew86, GGA (PW91), PBE
  !
  implicit none
  !
  !     dummy arguments
  !
  real(DP) :: rho, zeta, grho, sc, v1cup, v1cdw, v2c
  ! the total charge
  ! the magnetization
  ! the gradient of the charge squared
  ! exchange and correlation energies
  ! derivatives of correlation wr. rho
  ! derivatives of correlation wr. grho

  real(DP), parameter :: small = 1.E-10_DP, epsr=1.E-6_DP
  !
  !
  if ( abs(zeta) > 1.0_DP ) then
     sc = 0.0_DP
     v1cup = 0.0_DP
     v1cdw = 0.0_DP
     v2c = 0.0_DP
     return
  else
     !
     ! ... ( - 1.0 + epsr )  <  zeta  <  ( 1.0 - epsr )
     zeta = SIGN( MIN( ABS( zeta ), ( 1.0_DP - epsr ) ) , zeta )
  endif

  if (igcc == 0 .or. rho <= small .or. sqrt(abs(grho)) <= small) then
     sc = 0.0_DP
     v1cup = 0.0_DP
     v1cdw = 0.0_DP
     v2c = 0.0_DP
  elseif (igcc == 4) then
     call pbec_spin (rho, zeta, grho, 1, sc, v1cup, v1cdw, v2c)
  else
     call errore ('lsda_functionals (gcc_spin)', 'not implemented', igcc)
  endif
  !
  return
end subroutine gcc_spin

!-----------------------------------------------------------------------
!------- DRIVERS FOR DERIVATIVES OF XC POTENTIAL -----------------------
!-----------------------------------------------------------------------
!
      !-----------------------------------------------------------------------
      function dmxc (rho)
        !-----------------------------------------------------------------------
        !
        !  derivative of the xc potential with respect to the local density
        !
        !
        implicit none
        !
        real(DP), intent(in) :: rho
        ! input: the charge density ( positive )
        real(DP) :: dmxc
        ! output: the derivative of the xc potential
        !
        ! local variables
        !
        real(DP) :: dr, vxp, vcp, vxm, vcm, vx, ex, ec, rs
        real(DP), external :: dpz
        integer :: iflg
        !
        real(DP), parameter :: small = 1.E-30_DP, e2 = 2.0_DP, &
             pi34 = 0.75_DP / 3.141592653589793_DP, third = 1.0_DP /3.0_DP
        !
        dmxc = 0.0_DP
        if (rho < small) then
           return
        endif
        !
        !    first case: analytical derivatives available
        !
        if (get_iexch() == 1 .and. get_icorr() == 1) then
           rs = (pi34 / rho) **third
           !..exchange
           call slater (rs, ex, vx)
           dmxc = vx / (3.0_DP * rho)
           !..correlation
           iflg = 2
           if (rs < 1.0_DP) iflg = 1
           dmxc = dmxc + dpz (rs, iflg)
        else
           !
           !     second case: numerical derivatives
           !
           dr = min (1.E-6_DP, 1.E-4_DP * rho)
           call xc (rho + dr, ex, ec, vxp, vcp)
           call xc (rho - dr, ex, ec, vxm, vcm)
           dmxc = (vxp + vcp - vxm - vcm) / (2.0_DP * dr)
        endif
        !
        ! bring to rydberg units
        !
        dmxc = e2 * dmxc
        return
        !
      end function dmxc
      !
      !-----------------------------------------------------------------------
      subroutine dmxc_spin (rhoup, rhodw, dmuxc_uu, dmuxc_ud, dmuxc_du, &
           dmuxc_dd)
      !-----------------------------------------------------------------------
        !  derivative of the xc potential with respect to the local density
        !  spin-polarized case
        !
        implicit none
        !
        real(DP), intent(in) :: rhoup, rhodw
        ! input: spin-up and spin-down charge density
        real(DP), intent(out) :: dmuxc_uu, dmuxc_ud, dmuxc_du, dmuxc_dd
        ! output: up-up, up-down, down-up, down-down derivatives of the
        ! XC functional
        !
        ! local variables
        !
        real(DP) :: rhotot, rs, zeta, fz, fz1, fz2, ex, vx, ecu, ecp, vcu, &
             vcp, dmcu, dmcp, aa, bb, cc, dr, dz, ec, vxupm, vxdwm, vcupm, &
             vcdwm, rho, vxupp, vxdwp, vcupp, vcdwp, zeta_eff
        real(DP), external :: dpz, dpz_polarized
        integer :: iflg
        !
        real(DP), parameter :: small = 1.E-30_DP, e2 = 2.0_DP, &
             pi34 = 0.75_DP / 3.141592653589793_DP, third = 1.0_DP/3.0_DP, &
             p43 = 4.0_DP / 3.0_DP, p49 = 4.0_DP / 9.0_DP, m23 = -2.0_DP / 3.0_DP
        !
        dmuxc_uu = 0.0_DP
        dmuxc_du = 0.0_DP
        dmuxc_ud = 0.0_DP
        dmuxc_dd = 0.0_DP
        !
        rhotot = rhoup + rhodw
        if (rhotot <= small) return
        zeta = (rhoup - rhodw) / rhotot
        
        if (abs (zeta) > 1.0_DP) return
        if (get_iexch() == 1 .and. get_icorr() == 1) then
           !
           !    first case: analytical derivative available
           !
           !..exchange
           rs = (pi34 / (2.0_DP * rhoup) ) **third
           call slater (rs, ex, vx)
           dmuxc_uu = vx / (3.0_DP * rhoup)
           rs = (pi34 / (2.0_DP * rhodw) ) **third
           call slater (rs, ex, vx)
           dmuxc_dd = vx / (3.0_DP * rhodw)
           !..correlation
           rs = (pi34 / rhotot) **third
           iflg = 2
           if (rs < 1.0_DP) iflg = 1
           dmcu = dpz (rs, iflg)
           dmcp = dpz_polarized (rs, iflg)
           call pz (rs, 1, ecu, vcu)
           call pz_polarized (rs, ecp, vcp)
           fz = ( (1.0_DP + zeta) **p43 + (1.0_DP - zeta) **p43 - 2.0_DP) &
                / (2.0_DP**p43 - 2.0_DP)
           fz1 = p43 * ( (1.0_DP + zeta) **third- (1.0_DP - zeta) **third) &
                / (2.0_DP**p43 - 2.0_DP)
           fz2 = p49 * ( (1.0_DP + zeta) **m23 + (1.0_DP - zeta) **m23) &
                / (2.0_DP**p43 - 2.0_DP)
           aa = dmcu + fz * (dmcp - dmcu)
           bb = 2.0_DP * fz1 * (vcp - vcu - (ecp - ecu) ) / rhotot
           cc = fz2 * (ecp - ecu) / rhotot
           dmuxc_uu = dmuxc_uu + aa + (1.0_DP - zeta) * bb + (1.0_DP - zeta)**2 * cc
           dmuxc_du = dmuxc_du + aa + ( - zeta) * bb + (zeta**2 - 1.0_DP) * cc
           dmuxc_ud = dmuxc_du
           dmuxc_dd = dmuxc_dd+aa - (1.0_DP + zeta) * bb + (1.0_DP + zeta)**2 * cc
           
        else
           
           rho = rhoup + rhodw
           dr = min (1.E-6_DP, 1.E-4_DP * rho)
           call xc_spin (rho - dr, zeta, ex, ec, vxupm, vxdwm, vcupm, vcdwm)
           call xc_spin (rho + dr, zeta, ex, ec, vxupp, vxdwp, vcupp, vcdwp)
           dmuxc_uu = (vxupp + vcupp - vxupm - vcupm) / (2.0_DP * dr)
           dmuxc_ud = dmuxc_uu
           dmuxc_dd = (vxdwp + vcdwp - vxdwm - vcdwm) / (2.0_DP * dr)
           dmuxc_du = dmuxc_dd
           ! dz = min (1.d-6, 1.d-4 * abs (zeta) )
           dz = 1.E-6_DP
!
!          If zeta is too close to +-1, the derivative is computed at a slightly
!          smaller zeta
!
           zeta_eff = SIGN( MIN( ABS( zeta ), ( 1.0_DP - 2.0_DP*dz ) ) , zeta )

           call xc_spin (rho, zeta_eff - dz, ex, ec, vxupm, vxdwm, vcupm, vcdwm)
           call xc_spin (rho, zeta_eff + dz, ex, ec, vxupp, vxdwp, vcupp, vcdwp)
           dmuxc_uu = dmuxc_uu + (vxupp + vcupp - vxupm - vcupm) * &
                (1.0_DP - zeta) / rho / (2.0_DP * dz)
           dmuxc_ud = dmuxc_ud- (vxupp + vcupp - vxupm - vcupm) * &
                (1.0_DP + zeta) / rho / (2.0_DP * dz)
           dmuxc_du = dmuxc_du + (vxdwp + vcdwp - vxdwm - vcdwm) * &
                (1.0_DP - zeta) / rho / (2.0_DP * dz)
           dmuxc_dd = dmuxc_dd- (vxdwp + vcdwp - vxdwm - vcdwm) * &
                (1.0_DP + zeta) / rho / (2.0_DP * dz)
        endif
        !
        ! bring to rydberg units
        !
        dmuxc_uu = e2 * dmuxc_uu
        dmuxc_du = e2 * dmuxc_du
        dmuxc_ud = e2 * dmuxc_ud
        dmuxc_dd = e2 * dmuxc_dd
        !
        return
        
      end subroutine dmxc_spin

      !
      !-----------------------------------------------------------------------
      subroutine dgcxc (r, s2, vrrx, vsrx, vssx, vrrc, vsrc, vssc)
        !-----------------------------------------------------------------------
        USE kinds, only : DP
        implicit none
        real(DP) :: r, s2, vrrx, vsrx, vssx, vrrc, vsrc, vssc
        real(DP) :: dr, s, ds
        
        real(DP) :: sx, sc, v1xp, v2xp, v1cp, v2cp, v1xm, v2xm, v1cm, &
             v2cm
        s = sqrt (s2)
        dr = min (1.d-4, 1.d-2 * r)
        
        ds = min (1.d-4, 1.d-2 * s)
        call gcxc (r + dr, s2, sx, sc, v1xp, v2xp, v1cp, v2cp)
        
        call gcxc (r - dr, s2, sx, sc, v1xm, v2xm, v1cm, v2cm)
        vrrx = 0.5d0 * (v1xp - v1xm) / dr
        
        vrrc = 0.5d0 * (v1cp - v1cm) / dr
        vsrx = 0.25d0 * (v2xp - v2xm) / dr
        
        vsrc = 0.25d0 * (v2cp - v2cm) / dr
        call gcxc (r, (s + ds) **2, sx, sc, v1xp, v2xp, v1cp, v2cp)
        
        call gcxc (r, (s - ds) **2, sx, sc, v1xm, v2xm, v1cm, v2cm)
        vsrx = vsrx + 0.25d0 * (v1xp - v1xm) / ds / s
        
        vsrc = vsrc + 0.25d0 * (v1cp - v1cm) / ds / s
        vssx = 0.5d0 * (v2xp - v2xm) / ds / s
        
        vssc = 0.5d0 * (v2cp - v2cm) / ds / s
        return
      end subroutine dgcxc
      !
      !-----------------------------------------------------------------------
      subroutine dgcxc_spin (rup, rdw, gup, gdw, vrrxup, vrrxdw, vrsxup, &
           vrsxdw, vssxup, vssxdw, vrrcup, vrrcdw, vrscup, vrscdw, vssc, &
           vrzcup, vrzcdw)
        !-----------------------------------------------------------------------
        !
        !    This routine computes the derivative of the exchange and correlatio
        !    potentials with respect to the density, the gradient and zeta
        !
        USE kinds, only : DP
        implicit none
        real(DP), intent(in) :: rup, rdw, gup (3), gdw (3)
        ! input: the charges and the gradient
        real(DP), intent(out):: vrrxup, vrrxdw, vrsxup, vrsxdw, vssxup, &
             vssxdw, vrrcup, vrrcdw, vrscup, vrscdw, vssc, vrzcup, vrzcdw
        ! output: derivatives of the exchange and of the correlation
        !
        !    local variables
        !
        real(DP) :: r, zeta, sup2, sdw2, s2, s, sup, sdw, dr, dzeta, ds, &
             drup, drdw, dsup, dsdw, sx, sc, v1xupp, v1xdwp, v2xupp, v2xdwp, &
             v1xupm, v1xdwm, v2xupm, v2xdwm, v1cupp, v1cdwp, v2cp, v1cupm, &
             v1cdwm, v2cm
        ! charge densities and square gradients
        ! delta charge densities and gra
        ! delta gradients
        ! energies
        ! exchange potentials
        ! exchange potentials
        ! coorelation potentials
        ! coorelation potentials
        real(DP), parameter :: eps = 1.d-6
        !
        r = rup + rdw
        if (r.gt.eps) then
           zeta = (rup - rdw) / r
        else
           zeta = 2.d0
        endif
        sup2 = gup (1) **2 + gup (2) **2 + gup (3) **2
        sdw2 = gdw (1) **2 + gdw (2) **2 + gdw (3) **2

        s2 = (gup (1) + gdw (1) ) **2 + (gup (2) + gdw (2) ) **2 + &
             (gup (3) + gdw (3) ) **2
        sup = sqrt (sup2)
        sdw = sqrt (sdw2)
        s = sqrt (s2)
        !
        !     up part of exchange
        !
        
        if (rup.gt.eps.and.sup.gt.eps) then
           drup = min (1.d-4, 1.d-2 * rup)
           dsup = min (1.d-4, 1.d-2 * sdw)
           !
           !    derivatives of exchange: up part
           !
           call gcx_spin (rup + drup, rdw, sup2, sdw2, sx, v1xupp, v1xdwp, &
                v2xupp, v2xdwp)
           
           call gcx_spin (rup - drup, rdw, sup2, sdw2, sx, v1xupm, v1xdwm, &
                v2xupm, v2xdwm)
           vrrxup = 0.5d0 * (v1xupp - v1xupm) / drup
           vrsxup = 0.25d0 * (v2xupp - v2xupm) / drup

           call gcx_spin (rup, rdw, (sup + dsup) **2, sdw2, sx, v1xupp, &
                v1xdwp, v2xupp, v2xdwp)
 
           call gcx_spin (rup, rdw, (sup - dsup) **2, sdw2, sx, v1xupm, &
                v1xdwm, v2xupm, v2xdwm)
           vrsxup = vrsxup + 0.25d0 * (v1xupp - v1xupm) / dsup / sup
           vssxup = 0.5d0 * (v2xupp - v2xupm) / dsup / sup
        else
           vrrxup = 0.d0
           vrsxup = 0.d0
           vssxup = 0.d0
        endif
        
        if (rdw.gt.eps.and.sdw.gt.eps) then
           drdw = min (1.d-4, 1.d-2 * rdw)
           dsdw = min (1.d-4, 1.d-2 * sdw)
           !
           !    derivatives of exchange: down part
           !
           call gcx_spin (rup, rdw + drdw, sup2, sdw2, sx, v1xupp, v1xdwp, &
                v2xupp, v2xdwp)

           call gcx_spin (rup, rdw - drdw, sup2, sdw2, sx, v1xupm, v1xdwm, &
                v2xupm, v2xdwm)
           vrrxdw = 0.5d0 * (v1xdwp - v1xdwm) / drdw

           vrsxdw = 0.25d0 * (v2xdwp - v2xdwm) / drdw
           call gcx_spin (rup, rdw, sup2, (sdw + dsdw) **2, sx, v1xupp, &
                v1xdwp, v2xupp, v2xdwp)

           call gcx_spin (rup, rdw, sup2, (sdw - dsdw) **2, sx, v1xupm, &
                v1xdwm, v2xupm, v2xdwm)
           vrsxdw = vrsxdw + 0.25d0 * (v1xdwp - v1xdwm) / dsdw / sdw
           vssxdw = 0.5d0 * (v2xdwp - v2xdwm) / dsdw / sdw
        else
           vrrxdw = 0.d0
           vrsxdw = 0.d0
           vssxdw = 0.d0
        endif
        !
        !     derivatives of correlation
        !
        
        if (r.gt.eps.and.abs (zeta) .le.1.d0.and.s.gt.eps) then
           
           dr = min (1.d-4, 1.d-2 * r)
           call gcc_spin (r + dr, zeta, s2, sc, v1cupp, v1cdwp, v2cp)
           
           call gcc_spin (r - dr, zeta, s2, sc, v1cupm, v1cdwm, v2cm)
           vrrcup = 0.5d0 * (v1cupp - v1cupm) / dr
           
           vrrcdw = 0.5d0 * (v1cdwp - v1cdwm) / dr
           
           ds = min (1.d-4, 1.d-2 * s)
           call gcc_spin (r, zeta, (s + ds) **2, sc, v1cupp, v1cdwp, v2cp)

           call gcc_spin (r, zeta, (s - ds) **2, sc, v1cupm, v1cdwm, v2cm)
           vrscup = 0.5d0 * (v1cupp - v1cupm) / ds / s
           vrscdw = 0.5d0 * (v1cdwp - v1cdwm) / ds / s

           vssc = 0.5d0 * (v2cp - v2cm) / ds / s
!           dzeta = min (1.d-4, 1.d-2 * abs (zeta) )

           dzeta = 1.d-6
!
!   If zeta is too close to +-1 the derivative is evaluated at a slightly 
!   smaller  value
!
           zeta = SIGN( MIN( ABS( zeta ), ( 1.0_DP - 2.0_DP*dzeta ) ) , zeta )

           call gcc_spin (r, zeta + dzeta, s2, sc, v1cupp, v1cdwp, v2cp)
           
           call gcc_spin (r, zeta - dzeta, s2, sc, v1cupm, v1cdwm, v2cm)
           vrzcup = 0.5d0 * (v1cupp - v1cupm) / dzeta
           vrzcdw = 0.5d0 * (v1cdwp - v1cdwm) / dzeta
        else
           vrrcup = 0.d0
           vrrcdw = 0.d0
           vrscup = 0.d0
           vrscdw = 0.d0
           vssc = 0.d0
           vrzcup = 0.d0
           vrzcdw = 0.d0

        endif
        return
      end subroutine dgcxc_spin



end module funct
