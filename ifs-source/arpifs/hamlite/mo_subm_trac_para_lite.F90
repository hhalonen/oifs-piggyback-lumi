! 
! based on mo_submodel_tracer_interface.f90 
! in https://git.iac.ethz.ch/hammoz/icon-hammoz.git (hammoz/gpu/master)
!
! Philipp Weiss, University of Oxford  
!
! ICON
!
! ---------------------------------------------------------------
! Copyright (C) 2004-2024, DWD, MPI-M, DKRZ, KIT, ETH, MeteoSwiss
! Contact information: icon-model.org
!
! See AUTHORS.TXT for a list of authors
! See LICENSES/ for license information
! SPDX-License-Identifier: BSD-3-Clause
! ---------------------------------------------------------------

MODULE mo_subm_trac_para_lite

  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: ln, ls, ll, lf  
  PUBLIC :: nmaxtrac
  PUBLIC :: OK, NAME_USED, TABLE_FULL
  PUBLIC :: OFF, ON 
  PUBLIC :: GAS, AEROSOL, GAS_OR_AEROSOL, AEROSOLMASS, AEROSOLNUMBER, UNDEFINED 

  !--------------------
  ! module parameters
  !--------------------

  ! individual settings for each tracer
  INTEGER, PARAMETER :: ln =  24  ! length of name (char) components  
  INTEGER, PARAMETER :: ls =  32  ! length of short name
  INTEGER, PARAMETER :: ll = 256  ! length of longname and standardname 
  INTEGER, PARAMETER :: lf =   8  ! length of flag character string 
 
  ! maximum number of tracers  
  INTEGER, PARAMETER :: nmaxtrac = 10  

  !
  ! error return values
  !
  INTEGER, PARAMETER :: OK         = 0
  INTEGER, PARAMETER :: NAME_USED  = 2 
  INTEGER, PARAMETER :: TABLE_FULL = 4
  !
  ! general flags
  !
  INTEGER, PARAMETER :: OFF        = 0
  INTEGER, PARAMETER :: ON         = 1
  !
  ! phase indicator  (nphase)
  !
  INTEGER, PARAMETER :: GAS            = 1  ! gas
  INTEGER, PARAMETER :: AEROSOL        = 2  ! aerosol (for species definition)
  INTEGER, PARAMETER :: GAS_OR_AEROSOL = 3  ! gas or aerosol (for species definition)
  INTEGER, PARAMETER :: AEROSOLMASS    = 2  ! aerosol mass
  INTEGER, PARAMETER :: AEROSOLNUMBER  = -2 ! particle number concentration
  INTEGER, PARAMETER :: UNDEFINED      = 4  ! other tracers (e.g. CDNC)

END MODULE mo_subm_trac_para_lite
