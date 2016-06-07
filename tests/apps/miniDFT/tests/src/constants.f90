!
! Copyright (C) 2002-2006 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
MODULE constants
  !----------------------------------------------------------------------------
  !
  USE kinds, ONLY : DP
  !
  ! ... The constants needed everywhere
  !
  IMPLICIT NONE
  !
  SAVE
  !
  ! ... Mathematical constants
  ! 
  REAL(DP), PARAMETER :: pi     = 3.14159265358979323846_DP 
  REAL(DP), PARAMETER :: tpi    = 2.0_DP * pi
  REAL(DP), PARAMETER :: fpi    = 4.0_DP * pi
  REAL(DP), PARAMETER :: sqrtpi = 1.77245385090551602729_DP 
  REAL(DP), PARAMETER :: sqrtpm1= 1.0_DP / sqrtpi
  REAL(DP), PARAMETER :: sqrt2  = 1.41421356237309504880_DP
  !
  ! ... Physical constants, SI (NIST CODATA 2006), Web Version 5.1
  !     http://physics.nist.gov/constants
  REAL(DP), PARAMETER :: H_PLANCK_SI      = 6.62606896E-34_DP   ! J s
  REAL(DP), PARAMETER :: K_BOLTZMANN_SI   = 1.3806504E-23_DP    ! J K^-1 
  REAL(DP), PARAMETER :: ELECTRON_SI      = 1.602176487E-19_DP  ! C
  REAL(DP), PARAMETER :: ELECTRONVOLT_SI  = 1.602176487E-19_DP  ! J  
  REAL(DP), PARAMETER :: ELECTRONMASS_SI  = 9.10938215E-31_DP   ! Kg
  REAL(DP), PARAMETER :: HARTREE_SI       = 4.35974394E-18_DP   ! J
  REAL(DP), PARAMETER :: RYDBERG_SI       = HARTREE_SI/2.0_DP   ! J
  REAL(DP), PARAMETER :: BOHR_RADIUS_SI   = 0.52917720859E-10_DP ! m
  REAL(DP), PARAMETER :: AMU_SI           = 1.660538782E-27_DP  ! Kg
  REAL(DP), PARAMETER :: C_SI             = 2.99792458E+8_DP    ! m sec^-1
  !
  ! ... Physical constants, atomic units:
  ! ... AU for "Hartree" atomic units (e = m = hbar = 1)
  ! ... RY for "Rydberg" atomic units (e^2=2, m=1/2, hbar=1)
  !
  REAL(DP), PARAMETER :: K_BOLTZMANN_AU   = K_BOLTZMANN_SI / HARTREE_SI
  REAL(DP), PARAMETER :: K_BOLTZMANN_RY   = K_BOLTZMANN_SI / RYDBERG_SI
  !
  ! ... Unit conversion factors: energy and masses
  !
  REAL(DP), PARAMETER :: AUTOEV           = HARTREE_SI / ELECTRONVOLT_SI
  REAL(DP), PARAMETER :: RYTOEV           = AUTOEV / 2.0_DP
  REAL(DP), PARAMETER :: AMU_AU           = AMU_SI / ELECTRONMASS_SI
  REAL(DP), PARAMETER :: AMU_RY           = AMU_AU / 2.0_DP
  !
  ! ... Unit conversion factors: atomic unit of time, in s and ps
  !
  REAL(DP), PARAMETER :: AU_SEC           = H_PLANCK_SI/tpi/HARTREE_SI
  REAL(DP), PARAMETER :: AU_PS            = AU_SEC * 1.0E+12_DP
  !
  ! ... Unit conversion factors: pressure (1 Pa = 1 J/m^3, 1GPa = 10 Kbar )
  !
  REAL(DP), PARAMETER :: AU_GPA           = HARTREE_SI / BOHR_RADIUS_SI ** 3 &
                                            / 1.0E+9_DP 
  REAL(DP), PARAMETER :: RY_KBAR          = 10.0_DP * AU_GPA / 2.0_DP
  !
  ! ... Unit conversion factors: 1 debye = 10^-18 esu*cm 
  ! ...                                  = 3.3356409519*10^-30 C*m 
  ! ...                                  = 0.208194346 e*A
  ! ... ( 1 esu = (0.1/c) Am, c=299792458 m/s)
  !
  REAL(DP), PARAMETER :: DEBYE_SI         = 3.3356409519_DP * 1.0E-30_DP ! C*m 
  REAL(DP), PARAMETER :: AU_DEBYE         = ELECTRON_SI * BOHR_RADIUS_SI / &
                                            DEBYE_SI
  !
  REAL(DP), PARAMETER :: eV_to_kelvin = ELECTRONVOLT_SI / K_BOLTZMANN_SI
  REAL(DP), PARAMETER :: ry_to_kelvin = RYDBERG_SI / K_BOLTZMANN_SI
  !
  ! .. Unit conversion factors: Energy to wavelength
  !
  REAL(DP), PARAMETER :: EVTONM = 1E+9_DP * H_PLANCK_SI * C_SI / &
                                  &ELECTRONVOLT_SI
  REAL(DP), PARAMETER :: RYTONM = 1E+9_DP * H_PLANCK_SI * C_SI / RYDBERG_SI
  !
  !  Speed of light in atomic units
  !
  REAL(DP), PARAMETER :: C_AU             = C_SI / BOHR_RADIUS_SI * AU_SEC
  !
  ! ... zero up to a given accuracy
  !
  REAL(DP), PARAMETER :: eps4  = 1.0E-4_DP
  REAL(DP), PARAMETER :: eps6  = 1.0E-6_DP
  REAL(DP), PARAMETER :: eps8  = 1.0E-8_DP
  REAL(DP), PARAMETER :: eps12 = 1.0E-12_DP
  REAL(DP), PARAMETER :: eps14 = 1.0E-14_DP
  REAL(DP), PARAMETER :: eps16 = 1.0E-16_DP
  REAL(DP), PARAMETER :: eps24 = 1.0E-24_DP
  REAL(DP), PARAMETER :: eps32 = 1.0E-32_DP
  !
  REAL(DP), PARAMETER :: gsmall = 1.0E-12_DP
  !
  REAL(DP), PARAMETER :: e2 = 2.0_DP      ! the square of the electron charge
  REAL(DP), PARAMETER :: degspin = 2.0_DP ! the number of spins per level
  !
  !!!!!! COMPATIBIILITY
  !
  REAL(DP), PARAMETER :: amconv = AMU_RY
  REAL(DP), PARAMETER :: bohr_radius_cm = bohr_radius_si * 100.0_DP
  REAL(DP), PARAMETER :: BOHR_RADIUS_ANGS = bohr_radius_cm * 1.0E8_DP
  REAL(DP), PARAMETER :: ANGSTROM_AU = 1.0_DP/BOHR_RADIUS_ANGS
  REAL(DP), PARAMETER :: DIP_DEBYE = AU_DEBYE
  REAL(DP), PARAMETER :: AU_TERAHERTZ  = AU_PS
  REAL(DP), PARAMETER :: AU_TO_OHMCMM1 = 46000.0_DP ! (ohm cm)^-1
  REAL(DP), PARAMETER :: RY_TO_THZ = 1.0_DP / AU_TERAHERTZ / FPI
  REAL(DP), PARAMETER :: RY_TO_CMM1 = 1.E+10_DP * RY_TO_THZ / C_SI
  !

END MODULE constants

! perl script to create a program to list the available constants:
! extract with: grep '^!XX!' constants.f90 | sed 's,!XX!,,' > mkconstlist.pl
! then run: perl mkconstlist.pl constants.f90 > testme.f90
! and compile and run: testme.f90
!XX!#!/usr/bin/perl -w
!XX!
!XX!use strict;
!XX!
!XX!print <<EOF
!XX!! list all available constants and derived values
!XX!
!XX!PROGRAM list_constants
!XX!
!XX!  USE kinds, ONLY : DP
!XX!  USE constants
!XX!
!XX!EOF
!XX!;
!XX!
!XX!while(<>) { 
!XX!  if ( /REAL\s*\(DP\)\s*,\s*PARAMETER\s*::\s*([a-zA-Z_0-9]+)\s*=.*$/ )  { 
!XX!    print "  WRITE (*,'(A18,G24.17)') '$1:',$1\n"; 
!XX!  } 
!XX!}
!XX!
!XX!print <<EOF
!XX!
!XX!END PROGRAM list_constants
!XX!EOF
!XX!;
!XX!
