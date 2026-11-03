! 
! based on mo_ham_species.f90
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

MODULE mo_ham_lite_spec
  
  USE mo_kind_lite,              ONLY: dp 
  USE mo_subm_spec_lite,         ONLY: t_species 
  
  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: ham_lite_species 
  
  PUBLIC :: id_dms,     &  
            id_so2      

  PUBLIC :: id_so4,     &  
            id_bc,      &  
            id_oc,      &  
            id_ss,      &  
            id_du,      &  
            id_wat         
 
  !--------------------
  ! module variables
  !--------------------

  ! gas phase compounds
  INTEGER ::  id_dms,     &   ! Dimethyl sulphide
              id_so2          ! Sulphur dioxide    

  ! aerosol phase compounds
  INTEGER ::  id_so4,     &   ! Sulphate (aerosol)
              id_bc,      &   ! Black carbon
              id_oc,      &   ! Organic carbon 
              id_ss,      &   ! Sea Salt
              id_du,      &   ! Dust
              id_wat          ! Aerosol water

  CONTAINS

!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_species

    USE mo_subm_trac_para_lite,      ONLY: GAS, AEROSOL
    USE mo_subm_spec_lite,           ONLY: new_species
    USE mo_ham_lite_main,            ONLY: mw_so2, mw_so4
    USE mo_ham_lite_rad_data,        ONLY: iradso4, iradbc, iradoc, iradss, iraddu, iradwat

    IMPLICIT NONE
 
    ! values for hygroscopicity (kappa) are taken from GISS-E2.1-MATRIX
    ! as summarized in Fanourgakis et al. 2019

    !--- Sulphur Dioxide    
 
    CALL new_species(nphase      = GAS,                       &
                     longname    = 'Sulphur Dioxide',         &
                     shortname   = 'SO2',                     &
                     units       = 'kg kg-1',                 &   
                     mw          = mw_so2,                    & 
                     ldrydep     = .TRUE.,                    &
                     lwetdep     = .TRUE.,                    &  
                     idx         = id_so2                     )

    !--- Sulphate 

    CALL new_species(nphase       = AEROSOL,                  &
                     longname     = 'Sulphate',               &
                     shortname    = 'SO4',                    &
                     units        = 'kg kg-1',                &
                     mw           = mw_so4,                   & 
                     ldrydep      = .TRUE.,                   &
                     lwetdep      = .TRUE.,                   & 
                     density      = 1841._dp,                 &
                     kappa        = 0.507_dp,                 &     
                     iaerorad     = iradso4,                  & 
                     idx          = id_so4                    )

    !--- Black Carbon

    CALL new_species(nphase       = AEROSOL,                  &
                     longname     = 'Black carbon',           &
                     shortname    = 'BC',                     &
                     units        = 'kg kg-1',                &
                     mw           = 12.010_dp,                & 
                     ldrydep      = .TRUE.,                   &
                     lwetdep      = .TRUE.,                   &    
                     density      = 2000._dp,                 &
                     kappa        = 5.0e-7_dp,                &
                     iaerorad     = iradbc,                   &
                     idx          = id_bc                     )    

 
    !--- Organic Carbon
        
    CALL new_species(nphase       = AEROSOL,                  &
                     longname     = 'Organic carbon',         &
                     shortname    = 'OC',                     &
                     units        = 'kg kg-1',                &
                     mw           = 180._dp,                  & 
                     ldrydep      = .TRUE.,                   &
                     lwetdep      = .TRUE.,                   &  
                     density      = 2000._dp,                 &  
                     kappa        = 0.141_dp,                 &
                     iaerorad     = iradoc,                   &                         
                     idx          = id_oc                     )
 

    !--- Sea salt

    CALL new_species(nphase       = AEROSOL,                  &
                     longname     = 'Sea salt',               &
                     shortname    = 'SS',                     &
                     units        = 'kg kg-1',                &
                     mw           = 58.443_dp,                & 
                     ldrydep      = .TRUE.,                   &
                     lwetdep      = .TRUE.,                   & 
                     density      = 2165._dp,                 &
                     kappa        = 1.335_dp,                 &   
                     iaerorad     = iradss,                   &
                     idx          = id_ss                     )


    !--- Mineral Dust

    CALL new_species(nphase       = AEROSOL,                  &
                     longname     = 'Dust',                   &
                     shortname    = 'DU',                     &
                     units        = 'kg kg-1',                &
                     mw           = 250._dp,                  & 
                     ldrydep      = .TRUE.,                   &
                     lwetdep      = .TRUE.,                   & 
                     density      = 2650._dp,                 &
                     kappa        = 0.140_dp,                 &
                     iaerorad     = iraddu,                   &  
                     idx          = id_du                     )


    !--- Water (includes only water on aerosols, not water vapour, cloud water, or cloud ice)                

    CALL new_species(nphase       = AEROSOL,                  &
                     longname     = 'Aerosol water',          &
                     shortname    = 'WAT',                    &
                     units        = 'kg kg-1',                &
                     mw           = 18.0_dp,                  & 
                     iaerorad     = iradwat,                  & 
                     lemis        = .FALSE.,                  &
                     ldrydep      = .FALSE.,                  &
                     lwetdep      = .FALSE.,                  &     
                     density      = 1000._dp,                 &    
                     idx          = id_wat                    )
 
  END SUBROUTINE ham_lite_species

END MODULE mo_ham_lite_spec

