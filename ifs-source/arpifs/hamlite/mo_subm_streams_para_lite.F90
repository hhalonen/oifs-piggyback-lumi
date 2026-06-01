! 
! based on mo_submodel_streams_constants.f90
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

MODULE mo_subm_streams_para_lite

  USE mtime_lite,  ONLY: MAX_TIMEDELTA_STR_LEN
  
  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: nmaxstreamvars 
  PUBLIC :: default_output
  PUBLIC :: default_output_interval

  !--------------------
  ! module parameters
  !--------------------

  INTEGER, PARAMETER  :: nmaxstreamvars = 60  ! maximum number of different diagnostics in stream

  !--------------------
  ! module variables
  !--------------------

  LOGICAL, SAVE :: default_output = .TRUE.  ! logical to allow easy disabling of submodel output
  CHARACTER(LEN=MAX_TIMEDELTA_STR_LEN), SAVE :: default_output_interval = 'P1D'

END MODULE mo_subm_streams_para_lite
