! 
! based on mo_hammoz_timer.f90 
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

MODULE mo_hammoz_lite_timer
 
  USE mo_timer_lite, ONLY: new_timer

  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: init_hammoz_timers 

  !--------------------
  ! module variables
  !--------------------
   
  INTEGER, PUBLIC :: timer_lite_activation    
  INTEGER, PUBLIC :: timer_lite_wetdep
  INTEGER, PUBLIC :: timer_lite_drydep
  INTEGER, PUBLIC :: timer_lite_emissions
  INTEGER, PUBLIC :: timer_lite_sedimentation
  INTEGER, PUBLIC :: timer_lite_radiation_1
  INTEGER, PUBLIC :: timer_lite_radiation_2
  INTEGER, PUBLIC :: timer_lite_burden 

CONTAINS
  
  SUBROUTINE init_hammoz_timers
   
    timer_lite_activation     = new_timer('lite_activation') 
    timer_lite_wetdep         = new_timer('lite_wetdep')
    timer_lite_drydep         = new_timer('lite_drydep')
    timer_lite_emissions      = new_timer('lite_emissions')
    timer_lite_sedimentation  = new_timer('lite_sedimentation')
    timer_lite_radiation_1    = new_timer('lite_radiation_1')
    timer_lite_radiation_2    = new_timer('lite_radiation_2')
    timer_lite_burden         = new_timer('lite_burden') 

  END SUBROUTINE init_hammoz_timers

END MODULE mo_hammoz_lite_timer
