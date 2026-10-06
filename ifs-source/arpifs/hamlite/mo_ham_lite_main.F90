! 
! based on mo_ham.f90
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

MODULE mo_ham_lite_main
 
  USE mo_kind_lite,            ONLY: dp  
  USE mo_util_extfield_lite,   ONLY: EF_FILE, EF_MODULE   
  USE mo_hammoz_lite_config,   ONLY: nmaxclass

  IMPLICIT NONE
  PRIVATE
 
  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: set_ham_lite

  PUBLIC :: nmaxclass, nclass        
  PUBLIC :: nrad, nraddiag 
  PUBLIC :: ndrydep, nwetdep, nseasalt, ndust  
  PUBLIC :: ncdnc
  PUBLIC :: npath
  PUBLIC :: rqdu, rqss, rqca, rqsu
  PUBLIC :: burden_keytype
  PUBLIC :: mw_s, mw_so2, mw_so4, mw_dms, mw_oc     
  PUBLIC :: sigma_fine, sigma_coarse 
  PUBLIC :: sigma_mode, silog_mode 
  PUBLIC :: cmr2ras, cmr2mmr, cmr2ram 
  PUBLIC :: sizeclass 
  PUBLIC :: ibc_dust, ibc_seasalt,  &
            idsec_dust, idsec_seasalt, idsec_biogenic, idsec_wfire

  !--------------------
  ! module parameters
  !--------------------
 
  ! number of classes
  INTEGER, PARAMETER  :: nclass = 4 

  ! molecular weights [g/mol]
  REAL(dp), PARAMETER ::  mw_s   = 32.0655_dp,  &   ! molecular weight S
                          mw_so2 = 64.0643_dp,  &   ! molecular weight SO2
                          mw_so4 = 96.0631_dp,  &   ! molecular weight SO4
                          mw_dms = 62.1345_dp,  &   ! molecular weight DMS
                          mw_oc  = 180._dp          ! molecular weight OC 

  ! diameter limits [m] 
  REAL(dp), PARAMETER :: crdiv(4) = (/ 0.5E-9_dp, 0.5E-8_dp, 0.5E-7_dp, 0.5E-6_dp /)
  !                                    |          |          |          |    
  !                                 nucleation  aitken  accumulation  coarse  

  ! standard deviations
  REAL(dp), PARAMETER ::  sigma_fine   = 1.59_dp, & ! for fine modes
                          sigma_coarse = 2.00_dp    ! for coarse modes

  ! standard deviations of modes
  REAL(dp), PARAMETER :: sigma_mode(nclass) = &
    (/ sigma_coarse, sigma_coarse, sigma_fine, sigma_fine /)

  CHARACTER(LEN=*), PARAMETER :: thismodule='mo_ham_lite_main' 

  !--------------------
  ! module variables
  !--------------------
 
  INTEGER :: ndrydep      ! dry deposition scheme 
                          !
  INTEGER :: nwetdep      ! wet deposition scheme  
                          !
  INTEGER :: nseasalt     ! sea salt emission scheme 
                          !  
                          ! 1: Long et al. (2011) + Sofiev et al. (2011)
                          ! 2: Gong et al. (2003) + Sofiev et al. (2011)
                          ! 
  INTEGER :: ndust        ! choice of parameter set for the BGC dust scheme 
                          ! 3: Stier et al. (2005) default
                          ! 4: Stier et al. (2005) + East Asia soil properties
                          ! 5: Stier et al. (2005) + MSG-based Saharan dust sources
                          !    (Schepanski et al., GRL 2007; RSE 2012) + East Asia soil properties (default)
                          !  
  REAL(dp) :: ncdnc       ! minimum cloud droplet number concentration [m-3] 
                          !  
  
  INTEGER  :: npath       ! definition of carbonaceous and sulfuric mode based on 
                          !   1: present-day emissions
                          !   2: pre-industrial emissions
                          !
  REAL(dp) :: rqdu        ! radius of dust mode [m]
  REAL(dp) :: rqss        ! radius of sea salt mode [m]
  REAL(dp) :: rqca        ! radius of carbonaceous mode [m]
  REAL(dp) :: rqsu        ! radius of sulfuric mode [m]
                          !
  
  INTEGER :: nrad(nmaxclass)  ! radiation calculations (for each mode)
                              !
                              !    nrad = 0 off
                              !         = 1 SW    
                              !         = 2 LW    
                              !         = 3 SW + LW
                              ! 
  INTEGER :: nraddiag         ! radiation diagnostics
                              !
                              !    nraddiag = 0 off
                              !             = 1 2D   
                              !             = 2 2D + 3D
                              !

  INTEGER :: burden_keytype ! options: OFF, BYTRACER, BYSPECIES
  
  ! boundary condition indices 
  INTEGER ::  ibc_dust = 0, ibc_seasalt = 0   
  INTEGER ::  idsec_dust, idsec_seasalt, idsec_biogenic, idsec_wfire 

  ! derived types
  !   sigma, silog and the conversion factors are kept separate
  !   to avoid too many impacts on other modules and subroutines
  TYPE :: t_sizeclass 
     CHARACTER (LEN=32) :: longname           ! long name 
     CHARACTER (LEN=4)  :: shortname          ! short name 
     LOGICAL            :: lsoluble           ! soluble  
     LOGICAL            :: lacti              ! can activate to cloud droplets 
     LOGICAL            :: lsedi              ! sedimentation  
     INTEGER            :: idt_no             ! tracer index
     REAL(dp)           :: lite_radi          ! dry count median radius [m]
     REAL(dp)           :: lite_dens          ! density [kg/m^3]
     REAL(dp)           :: lite_kappa         ! hygroscopicity parameter 
     INTEGER            :: lite_nspe          ! number of species
     INTEGER            :: lite_ispe(20)      ! indices of species
     REAL(dp)           :: lite_vspe(20)      ! volume fractions of species 
     INTEGER            :: lite_nemi          ! number of emission sectors
     CHARACTER (LEN=64) :: lite_iemi(20)      ! indices of emission sectors 
  END TYPE t_sizeclass     
  
  TYPE(t_sizeclass), TARGET :: sizeclass(nmaxclass)
  
  ! natural logarithm of the standard deviation of each mode
  REAL(dp) :: silog_mode(nclass)  

  ! conversion factors for lognormal particle size distributions
  REAL(dp) :: cmr2ras(nclass),  &   ! count median radius to radius of average surface
              cmr2mmr(nclass),  &   ! count median radius to mass mean radius 
              cmr2ram(nclass)       ! count median radius to radius of average mass 

  CONTAINS 

!---------------------------------------------------------------------------------------------------

  SUBROUTINE set_ham_lite ( p_patch )
 
    USE mo_model_domain_lite,            ONLY: t_patch 
    USE mo_exception_lite,               ONLY: finish, message, message_text, print_value 
    USE mo_util_string_lite,             ONLY: separator
    USE mo_util_config_lite,             ONLY: set_config
    USE mo_util_extfield_lite,           ONLY: EF_FILE  
    USE mo_subm_diag_lite,               ONLY: OFF, ON, BYTRACER, BYSPECIES 
    USE mo_lite_main_list_config,        ONLY: lite_main_list

    IMPLICIT NONE

    TYPE(t_patch), DIMENSION(:), INTENT(IN) :: p_patch
    INTEGER :: jclass, ndomain 

    ndomain = SIZE( p_patch ) 
    IF (ndomain > 1) CALL finish(thismodule, 'nesting not supported')
    IF (nclass > nmaxclass) CALL finish('set_ham_lite', 'maximum number of aerosol modes/bins exceeded')

    burden_keytype  = BYTRACER
     
    ! import values from namelist
 
    CALL message('','')
    CALL message('',separator)
    CALL message('set_ham_lite', 'import parameters from lite_main_list')
    CALL message('','---')

    CALL set_config( ndrydep              , lite_main_list % ndrydep             )         
    CALL set_config( nwetdep              , lite_main_list % nwetdep             )           
    CALL set_config( nseasalt             , lite_main_list % nseasalt            )  
    CALL set_config( ndust                , lite_main_list % ndust               )    
    CALL set_config( ncdnc                , lite_main_list % ncdnc               )
    CALL set_config( npath                , lite_main_list % npath               )   
    CALL set_config( rqdu                 , lite_main_list % rqdu                )  
    CALL set_config( rqss                 , lite_main_list % rqss                )  
    CALL set_config( rqca                 , lite_main_list % rqca                )  
    CALL set_config( rqsu                 , lite_main_list % rqsu                )              
    CALL set_config( nrad                 , lite_main_list % nrad                )                
    CALL set_config( nraddiag             , lite_main_list % nraddiag            )             
    CALL set_config( burden_keytype       , lite_main_list % burden_keytype      )   

    ! print and check parameters

    CALL print_value('ndrydep             = ', ndrydep  ) 
    CALL print_value('nwetdep             = ', nwetdep  ) 
    CALL print_value('nseasalt            = ', nseasalt )  
    CALL print_value('ndust               = ', ndust    )
    CALL print_value('ncdnc               = ', ncdnc    )
    CALL print_value('npath               = ', npath    )
    CALL print_value('rqdu                = ', rqdu     )
    CALL print_value('rqss                = ', rqss     )
    CALL print_value('rqca                = ', rqca     )
    CALL print_value('rqsu                = ', rqsu     )

    WRITE (message_text,'(a,25i3)') 'nrad                = ', nrad   
    CALL message('', message_text) 
    
    CALL print_value('nraddiag            = ', nraddiag) 
    CALL print_value('burden_keytype      = ', burden_keytype)  

    IF ( ndust < 3 .OR. ndust > 5) THEN
      WRITE(message_text,'(a,i0)') 'invalid option for dust scheme, ndust = ', ndust
      CALL message('set_ham_lite', message_text)
    ENDIF

    ! conversion factors for lognormal modes (Hinds 1982, Chapters 4.5 and 4.6 and Equation 4.53) 
    DO jclass=1, nclass 

      silog_mode(jclass) = LOG(sigma_mode(jclass))

      ! count median radius to mass mean radius 
      cmr2mmr(jclass) = EXP(3.5_dp*(LOG(sigma_mode(jclass)))**2) 
      ! count median radius to radius of average mass  
      cmr2ram(jclass) = EXP(1.5_dp*(LOG(sigma_mode(jclass)))**2) 
      ! count median radius to radius of average surface
      cmr2ras(jclass) = EXP(1.0_dp*(LOG(sigma_mode(jclass)))**2) 

    END DO
    

  END SUBROUTINE set_ham_lite
 
END MODULE mo_ham_lite_main
