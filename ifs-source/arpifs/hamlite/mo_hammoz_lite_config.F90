! 
! based on mo_hammoz_config.f90 
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

MODULE mo_hammoz_lite_config

  USE mo_io_units_lite,           ONLY: filename_max
  USE mo_impl_constants_lite,     ONLY: max_dom

  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC ::   nmaxclass
  PUBLIC ::   dynamics_grid_filename

  !--------------------
  ! module parameters
  !--------------------

  INTEGER, PARAMETER :: nmaxclass = 20  ! maximum number of aerosol modes or bins

  CHARACTER(LEN=filename_max) :: dynamics_grid_filename(max_dom)

END MODULE mo_hammoz_lite_config
