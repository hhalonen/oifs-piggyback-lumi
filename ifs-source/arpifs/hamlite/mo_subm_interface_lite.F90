! 
! based on mo_submodel_interface.f90 
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
 
MODULE mo_subm_interface_lite

  USE mo_kind_lite,            ONLY: wp 
  USE mo_impl_constants_lite,  ONLY: max_dom
  !USE mo_timer,           ONLY: ltimer, timer_start, timer_stop

  IMPLICIT NONE
  PRIVATE
  
  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: init_hamlite_submodel,  & ! called from construct_atmo_model
            aes_phy_bcs_subm,       & !             aes_phy_bcs
            cloud_mig_subm_1,       & !             cloud_mig
            cloud_mig_subm_2,       & !             cloud_mig 
            radiation_subm_1,       & !             rte_rrtmgp_interface
            radiation_subm_2,       & !             rte_rrtmgp_interface       
            interface_tmx_subm,     & !             interface_aes_tmx
            aes_phy_main_subm
            !interface_aes_hamlite     !             aes_phy_main
 
  !--------------------
  ! module variables
  !--------------------

  LOGICAL, SAVE, DIMENSION (max_dom) :: lfirst = .TRUE.

  CONTAINS
           
!---------------------------------------------------------------------------------------------------

  SUBROUTINE init_hamlite_submodel ( p_patch, datetime  )
   
    USE mo_model_domain_lite,          ONLY: t_patch
    USE mtime_lite,                    ONLY: t_datetime => datetime
    USE mo_subm_main_lite,             ONLY: set_submodel  
    
    IMPLICIT NONE
    
    TYPE(t_patch),    INTENT(IN)          :: p_patch(:)
    TYPE(t_datetime), INTENT(IN), POINTER :: datetime

    CALL set_submodel  
    CALL init_subm ( p_patch, datetime ) 
    !CALL init_subm_memory ( p_patch )
  
  END SUBROUTINE init_hamlite_submodel

!---------------------------------------------------------------------------------------------------

  SUBROUTINE init_subm ( p_patch, datetime )
  
    USE mo_model_domain_lite,          ONLY: t_patch
    USE mtime_lite,                         ONLY: t_datetime => datetime
    USE mo_subm_main_lite,             ONLY: lhamlite, ldrydep,  &
                                        lemissions, id_ham
    USE mo_subm_bcs_lite,              ONLY: start_boundary_condition
    USE mo_subm_trac_lite,             ONLY: init_tracer, complete_tracer, print_tracer
    USE mo_subm_spec_lite,             ONLY: init_splist, print_species 
    USE mo_ham_lite_init,              ONLY: ham_lite_start,     &
                                        ham_lite_tracer,    &
                                        ham_lite_init  
    USE mo_hammoz_lite_drydep,         ONLY: init_drydep   
    USE mo_hammoz_lite_emi,            ONLY: init_emi 
    !USE mo_hammoz_timer,          ONLY: init_hammoz_timers   

    IMPLICIT NONE

    TYPE(t_patch),    INTENT(IN), DIMENSION(:)  :: p_patch
    TYPE(t_datetime), INTENT(IN), POINTER       :: datetime

    IF (.NOT. lhamlite) RETURN 

    CALL init_tracer
    CALL init_splist 

    CALL start_boundary_condition ( p_patch )   

    CALL ham_lite_start ( p_patch ) 
    CALL ham_lite_tracer 
    CALL ham_lite_init

    IF (ldrydep)    CALL init_drydep  ( p_patch )  
    IF (lemissions) CALL init_emi     ( p_patch, datetime ) 

    CALL print_species
    CALL print_tracer

    CALL complete_tracer

    !IF (ltimer) CALL init_hammoz_timers
 
  END SUBROUTINE init_subm 

!---------------------------------------------------------------------------------------------------

  ! SUBROUTINE init_subm_memory ( p_patch )

  !   USE mo_model_domain,            ONLY: t_patch 
  !   USE mo_subm_main,               ONLY: lhamlite, lemissions, lwetdep, ldrydep, &
  !                                         lsedimentation, lburden 
  !   USE mo_subm_bcs,                ONLY: bc_lists_init
  !   USE mo_subm_trac,               ONLY: trlist
  !   USE mo_subm_streams,            ONLY: init_submodel_streams
  !   USE mo_subm_trac_proc,          ONLY: xt_burden_init_mem
  !   USE mo_hammoz_wetdep,           ONLY: init_wetdep_stream
  !   USE mo_hammoz_drydep,           ONLY: init_drydep_stream
  !   USE mo_hammoz_sed,              ONLY: init_sed_stream
  !   USE mo_hammoz_emi,              ONLY: init_emi_stream
  !   USE mo_hammoz_act,              ONLY: init_act_stream
  !   USE mo_ham_lite_init,           ONLY: ham_lite_init_memory  

  !   IMPLICIT NONE
 
  !   TYPE(t_patch), INTENT(IN), DIMENSION(:)  :: p_patch
 
  !   IF (.NOT. lhamlite) RETURN
 
  !   CALL init_submodel_streams       
 
  !   IF (lwetdep)        CALL init_wetdep_stream ( p_patch )
  !   IF (lwetdep)        CALL init_act_stream    ( p_patch )
  !   IF (ldrydep)        CALL init_drydep_stream ( p_patch )
  !   IF (lsedimentation) CALL init_sed_stream    ( p_patch )
  !   IF (lemissions)     CALL init_emi_stream    ( p_patch )   
 
  !   CALL ham_lite_init_memory ( p_patch ) 

  !   IF (lburden) CALL xt_burden_init_mem ( p_patch )  

  !   CALL bc_lists_init ( p_patch ) 

  ! END SUBROUTINE init_subm_memory

!---------------------------------------------------------------------------------------------------

  SUBROUTINE aes_phy_bcs_subm(  jg,                                       &
                                p_patch, datetime,                        &
                                pareacella, pclon, pclat, pzf, pzh, pdz,  &
                                pmair, prho, paphm1                       )
     
    USE mo_model_domain_lite,      ONLY: t_patch
    USE mtime_lite,                ONLY: t_datetime => datetime!, &
                                    !newDatetime !OPERATOR(/=), newDatetime
    !USE mo_master_config,     ONLY: experimentStartDate   
    USE mo_hammoz_lite_emi,        ONLY: emi_read_bc_interface   
    USE mo_ham_lite_misc,          ONLY: lemimod
    USE mo_ham_lite_init,          ONLY: ham_lite_init_modes, ham_lite_print_modes

    IMPLICIT NONE
    
    INTEGER, INTENT (IN) :: jg 
    REAL(wp), INTENT(IN) :: pareacella  (:,:),    & ! grid box area [m2] (nproma, nblk)  
                            pclon       (:,:),    & ! grid cell center longtude (nproma, nblk)
                            pclat       (:,:),    & ! grid cell center latitide (nproma, nblk)
                            pzf         (:,:,:),  & ! geometric height of full levels [m] (nproma, klev, nblk)  
                            pzh         (:,:,:),  & ! geometric height of half levels [m] (nproma, klevp1, nblk)  
                            pdz         (:,:,:),  & ! geometric height thickness of layer [m]
                            pmair       (:,:,:),  & ! air mass [kg/m2] 
                            prho        (:,:,:),  & ! density [kg/m3]
                            paphm1      (:,:,:)     ! half level pressure [Pa]
    TYPE(t_patch),  INTENT(IN) :: p_patch         
    TYPE(t_datetime), POINTER, INTENT(IN) :: datetime

    TYPE(t_datetime), POINTER :: stime

    ! IF ( lfirst(jg) ) THEN
    !   stime => newDatetime( experimentStartDate ) ! check for restarts
    !   IF ( datetime /= stime ) lfirst(jg) = .FALSE.  
    ! END IF
 
    IF ( lfirst(jg) ) THEN 
      IF ( lemimod ) CALL ham_lite_init_modes(  jg, p_patch, pareacella, pclon, pclat,  &
                                                pzf, pzh, pdz, pmair, paphm1, datetime            ) 
      CALL ham_lite_print_modes                    
    END IF

    CALL emi_read_bc_interface( p_patch, datetime, pareacella, pclon, pclat, pzh )

    
  END SUBROUTINE aes_phy_bcs_subm
 
!---------------------------------------------------------------------------------------------------

  SUBROUTINE cloud_mig_subm_1(  jg, jb, jcs, jce, jks, klev,        &
                                kproma, ktrac,                      &
                                pdz, prho, pwa, ptm1, papm1,        &
                                pqm1, pcm1, pim1, pxtm1, pcdnc, psew)
 
    !USE mo_hammoz_timer,      ONLY: timer_lite_activation
    USE mo_ham_lite_main,     ONLY: nclass 
    !USE mo_ham_lite_streams,  ONLY: ham_field
    USE mo_ham_lite_misc,     ONLY: lcheck_wdep,                  & 
                                    pfrac, pnact, pbase, pnsum,   &  
                                    ham_lite_check_i2d, ham_lite_check_r2d  
    USE mo_ham_lite_act,      ONLY: zwam,               &
                                    ham_lite_act_para,  &
                                    ham_lite_act_nact,  &
                                    ham_lite_act_cdnc!,  &
                                    !ham_lite_act_diag    

    IMPLICIT NONE

    INTEGER, INTENT(IN) ::  jg,       & ! grid index
                            jb,       & ! index of block loop
                            jcs, jce, & ! start and end indices of columns loop 
                            jks,      & ! index of top level
                            klev,     & ! number of full levels 
                            kproma,   & ! maximum number of columns
                            ktrac       ! number of tracers excluding water vapour and hydrometeors
    REAL(wp), INTENT(IN) :: pdz   (kproma,klev),      & ! geometric height thickness of layer [m] 
                            prho  (kproma,klev),      & ! air density [kg/m3]
                            pwa   (kproma,klev),      & ! vertical velocity [m/s]    
                            ptm1  (kproma,klev),      & ! temperature [K]  
                            papm1 (kproma,klev),      & ! full level pressure [Pa]  
                            pqm1  (kproma,klev),      & ! specific humidity [kg/kg]  
                            pcm1  (kproma,klev),      & ! cloud water [kg/kg]
                            pim1  (kproma,klev),      & ! cloud ice [kg/kg]
                            pxtm1 (kproma,klev,ktrac),& ! tracer number mixing ratio [#/kg]
                            psew(kproma,klev)          ! saturation water vapor pressure [Pa]

    REAL(wp), INTENT(INOUT) :: pcdnc (kproma,klev) ! cloud droplet number concentration [1/m3] 

    INTEGER :: jclass, jk, jl
    LOGICAL :: ltest
    REAL(wp) :: zrdry (nclass),             & ! dry radius for each class [m]
                zbhyg (nclass),             & ! hygroscopicity parameter of the Koehler equation
                zwa   (kproma,klev),        & ! updraft velocity [m/s]
                zacur (kproma,klev),        & ! curvature parameter of the Koehler equation
                zcsup (kproma,klev,nclass), & ! critical supersaturation [% 0-1]
                zcrad (kproma,klev,nclass), & ! critical radius of activation per mode [m]
                zsmax (kproma,klev)            ! maximum supersaturation [% 0-1]
 
    IF (.NOT. ALLOCATED(pfrac)) ALLOCATE(pfrac(kproma,klev,nclass)) 
    IF (.NOT. ALLOCATED(pnact)) ALLOCATE(pnact(kproma,klev,nclass)) 
    IF (.NOT. ALLOCATED(pbase)) ALLOCATE(pbase(kproma,klev))
    IF (.NOT. ALLOCATED(pnsum)) ALLOCATE(pnsum(kproma,klev)) 

    !IF (ltimer) CALL timer_start(timer_lite_activation)

    ! set lower bound for updraft velocity
    DO jk = 1,klev
      DO jl = jcs,jce
        ltest = (pwa(jl,jk) > 0._wp)  
        zwa(jl,jk) = MERGE(MAX(zwam, pwa(jl,jk)), 0._wp, ltest)
      END DO 
    END DO
 
    ! activation with Abdul-Razzak & Ghan scheme 
    CALL ham_lite_act_para( jb, jcs, jce, jks, klev, kproma, & 
                            ptm1, zrdry, zbhyg, zacur         )

    CALL ham_lite_act_nact( jg, jb, jcs, jce, jks, klev, kproma,      &
                            prho, zwa, ptm1, papm1, pqm1, pxtm1,      &
                            zrdry, zbhyg, zacur,                      &
                            pnact, pfrac, zcsup, zcrad, zsmax, pnsum, psew)
                 
    CALL ham_lite_act_cdnc( jg, jb, jcs, jce, jks, klev, kproma,  &
                            pcm1, pim1, pnsum, pbase, pcdnc       )
 
    ! diagnostics
    ! CALL ham_lite_act_diag( jg, jb, jcs, jce, jks, klev, kproma,      &
    !                         pdz, zwa, pcm1, pim1,                     & 
    !                         zacur, zbhyg, pnact, pfrac, zcsup, zcrad, & 
    !                         zsmax, pnsum, pbase, pcdnc                ) 
  
    !IF (ltimer) CALL timer_stop(timer_lite_activation) 
 
    ! print checks 
    IF (lcheck_wdep) THEN   
      CALL ham_lite_check_i2d( jb, jcs, jce, jks, klev, kproma, pbase, 'cloud_mig_subm_1', 'pbase' ) 
      CALL ham_lite_check_r2d( jb, jcs, jce, jks, klev, kproma, pcdnc, 'cloud_mig_subm_1', 'pcdnc' ) 
    END IF  

  END SUBROUTINE cloud_mig_subm_1
 
!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE cloud_mig_subm_2(  jg, jb, jcs, jce, jks, klev,              &
                                kproma, ktrac,                            & 
                                pdtime,                                   &  
                                prho, pxlta, pmair,                       &
                                pxlqc, pxlqi, pxlqr, pxlqs, pxlqg,        &
                                pxprr, pxpri, pxprs, pxprg,               & 
                                pxtm1, ptend, pflux_wdep                  )
 
    USE mo_mpi_lite,               ONLY: p_pe, p_io 
    USE mo_subm_main_lite,         ONLY: lwetdep
    USE mo_subm_trac_lite,         ONLY: trlist    
    USE mo_hammoz_lite_wetdep,     ONLY: wetdep_lite_interface
    !USE mo_hammoz_timer,      ONLY: timer_lite_wetdep    
    USE mo_ham_lite_main,          ONLY: nclass, sizeclass  
    
    IMPLICIT NONE

    INTEGER, INTENT(IN) ::  jg,       & ! grid index
                            jb,       & ! index of block loop
                            jcs, jce, & ! start and end indices of columns loop 
                            jks,      & ! index of top level
                            klev,     & ! number of full levels 
                            kproma,   & ! maximum number of columns
                            ktrac       ! number of tracers excluding water vapour and hydrometeors
    REAL(wp), INTENT(IN) :: pdtime,                 & ! time step [s]
                            prho  (kproma,klev),    & ! density [kg/m3]
                            pxlta (kproma,klev),    & ! temperature [K]
                            pmair (kproma,klev),    & ! air mass [kg/m2] 
                            pxlqc (kproma,klev),    & ! cloud water [kg/kg]
                            pxlqi (kproma,klev),    & ! cloud ice [kg/kg]
                            pxlqr (kproma,klev),    & ! rain [kg/kg] 
                            pxlqs (kproma,klev),    & ! snow [kg/kg]
                            pxlqg (kproma,klev),    & ! graupel [kg/kg]
                            pxprr (kproma),         & ! precip rate rain [kg/m2/s] 
                            pxpri (kproma),         & ! precip rate ice [kg/m2/s] 
                            pxprs (kproma),         & ! precip rate snow [kg/m2/s] 
                            pxprg (kproma),         & ! precip rate graupel [kg/m2/s] 
                            pxtm1 (kproma,klev,ktrac) ! tracer number mixing ratio tendency [#/kg]
    REAL(wp), INTENT(INOUT) :: ptend (kproma,klev,ktrac)  ! tracer tendency [#/kg/s]
    REAL(wp), INTENT(INOUT) :: pflux_wdep (kproma,ktrac)  ! wet dep flux [#/m2/s]

    INTEGER :: jl, jk, jt 

    !$ACC DATA PRESENT(ptend)
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR COLLAPSE(3)
    DO jt = 1,ktrac
      DO jk = jks,klev
        DO jl = jcs,jce
          ptend(jl,jk,jt) = 0.0_wp 
        END DO 
      END DO 
    END DO
    !$ACC END PARALLEL LOOP
    !$ACC WAIT(1)
    !$ACC END DATA
  
    IF (lwetdep .AND. ANY(trlist%ti(:)%nwetdep > 0)) THEN
        
      !IF (ltimer) CALL timer_start(timer_lite_wetdep)
   
      CALL wetdep_lite_interface( jg, jb, jcs, jce, jks, klev, kproma,    &
                                  pdtime,                                 &
                                  prho, pxlta, pxlqc, pxlqi,              &
                                  pxlqr, pxlqs, pxlqg,                    &
                                  pxprr, pxpri, pxprs, pxprg,             &
                                  pmair, pxtm1, ptend, pflux_wdep         )
 
      !IF (ltimer) CALL timer_stop(timer_lite_wetdep)

    END IF 

  END SUBROUTINE cloud_mig_subm_2

!---------------------------------------------------------------------------------------------------

  SUBROUTINE radiation_subm_1(  jg, jb, jcs, jce,                                           &
                                kproma, klev, ntracer, ntrac, pqm1, iqt,                     &
                                kbndlw, kbndsw, kband_lw, kband_sw,                         &
                                xq_trc, xm_air, tk_fl, pp_fl,                               &                                                         
                                aer_tau_sw_vr, aer_ssa_sw_vr, aer_asy_sw_vr, aer_tau_lw_vr  )
 
    USE mo_subm_main_lite,     ONLY: lhamlite, lradiation
    !USE mo_hammoz_timer,  ONLY: timer_lite_radiation_1
    USE mo_ham_lite_misc,      ONLY: lcheck_rad   
    USE mo_ham_lite_rad,       ONLY: ham_lite_rad_cache, ham_lite_rad, &
                                ham_lite_rad_gp2g, ham_lite_rad_check    

    IMPLICIT NONE
  
    INTEGER, INTENT(IN) ::  jg,       & ! grid index              
                            jb,       & ! index of block loop
                            jcs, jce, & ! start and end indices of columns loop  
                            kproma,   & ! maximum number of columns
                            klev,     & ! number of levels 
                            ntracer,  & ! number of tracers
                            ntrac,    & ! number of tracers excluding water species
                            !iqv,      & ! index of specific humidity
                            iqt,      & ! index of first non-water tracer
                            kbndlw,   & ! number of LW bands
                            kbndsw      ! number of SW bands 
    REAL(wp), INTENT(IN) :: kband_lw  (2,kbndlw),           & ! limits of longwave bands [1/cm] - RRTMGP
                            kband_sw  (2,kbndsw),           & ! limits of shortwave bands [1/cm] - RRTMGP
                            xq_trc  (kproma,klev,ntracer),  & ! tracer mass fraction [kg/kg]
                            xm_air  (kproma,klev),          & ! air mass in layer [kg/m2] 
                            tk_fl   (kproma,klev),          & ! temperature at full levels [K]
                            pp_fl   (kproma,klev),          & ! pressure at full levels [Pa]
                            pqm1    (kproma,klev)             ! specific humidity [kg/kg]

    REAL(wp), INTENT(INOUT) ::  aer_tau_sw_vr (kproma,klev,kbndsw), & ! SW aerosol optical depth
                                aer_ssa_sw_vr (kproma,klev,kbndsw), & ! SW aerosol single scattering albedo
                                aer_asy_sw_vr (kproma,klev,kbndsw), & ! SW aerosol asymmetry factor
                                aer_tau_lw_vr (kproma,klev,kbndlw)    ! LW aerosol optical depth
                              
    INTEGER :: jk, jl, js, jt, jwv
    REAL(wp) :: sh_fl   (kproma,klev),  &     ! specific humidity at full levels [kg/kg]
                xm_trc  (kproma,klev,ntrac)   ! tracer number [#/m2]
    REAL(wp) :: zband_lw (2,kbndlw),  & ! limits of longwave bands [1/cm] - RRTMG
                zband_sw (2,kbndsw)     ! limits of shortwave bands [1/cm] - RRTMG

    IF (.NOT. lhamlite) RETURN  

    IF (.NOT. lradiation) RETURN

    !IF (ltimer) CALL timer_start(timer_lite_radiation_1)
 
    DO jk = 1,klev
      DO jl = jcs,jce
        sh_fl(jl,jk) = pqm1(jl,jk)!xq_trc(jl,jk,iqv) 
      END DO 
    END DO  

    DO jt = 1,ntrac
      js = jt - 1 + iqt
      DO jk = 1,klev
        DO jl = jcs,jce
          xm_trc(jl,jk,jt) = xm_air(jl,jk) * xq_trc(jl,jk,js)
        END DO 
      END DO  
    END DO 

    DO jwv = 1,kbndsw
      DO jk = 1,klev
        DO jl = jcs,jce
          aer_tau_sw_vr(jl,jk,jwv) = 0._wp 
          aer_ssa_sw_vr(jl,jk,jwv) = 0._wp 
          aer_asy_sw_vr(jl,jk,jwv) = 0._wp 
        END DO 
      END DO 
    END DO 

    DO jwv = 1,kbndlw
      DO jk = 1,klev
        DO jl = jcs,jce
          aer_tau_lw_vr(jl,jk,jwv) = 0._wp   
        END DO 
      END DO 
    END DO 

    CALL ham_lite_rad_cache( kproma, klev )  
    CALL ham_lite_rad(  jg, jb, jcs, jce, kproma, klev, ntrac, kbndlw, kbndsw,      &
                        xm_trc, tk_fl, pp_fl, sh_fl,                                &
                        aer_tau_sw_vr, aer_ssa_sw_vr, aer_asy_sw_vr, aer_tau_lw_vr  ) 

    !IF (ltimer) CALL timer_stop(timer_lite_radiation_1)
 
    ! print checks 
    IF (lcheck_rad) THEN
      CALL ham_lite_rad_gp2g( kbndlw, kbndsw, kband_lw, kband_sw, zband_lw, zband_sw )
      CALL ham_lite_rad_check(  jb, jcs, jce, kproma, klev, kbndlw, kbndsw, zband_lw, zband_sw,   &
                                aer_tau_sw_vr, aer_ssa_sw_vr, aer_asy_sw_vr, aer_tau_lw_vr        )
    END IF 
 
  END SUBROUTINE radiation_subm_1

!---------------------------------------------------------------------------------------------------

  SUBROUTINE radiation_subm_2(  jg, jb, jcs, jce,                                                 &
                                kproma, klev, kbndlw, kbndsw,                                     & 
                                pdz, aer_tau_sw_vr, aer_ssa_sw_vr, aer_asy_sw_vr, aer_tau_lw_vr   )

    USE mo_subm_main_lite,     ONLY: lhamlite, lradiation
    !USE mo_hammoz_timer,  ONLY: timer_lite_radiation_2
    !USE mo_ham_lite_rad,  ONLY: ham_lite_rad_diag, ham_lite_rad_cache_cleanup
    USE mo_ham_lite_rad,       ONLY: ham_lite_rad_cache_cleanup

    IMPLICIT NONE

    INTEGER, INTENT(IN) ::  jg,         & ! grid index
                            jb,         & ! index of block loop
                            jcs, jce,   & ! start and end indices of columns loop 
                            kproma,     & ! maximum number of columns
                            klev,       & ! number of levels  
                            kbndlw,     & ! number of LW bands
                            kbndsw        ! number of SW bands
    REAL(wp), INTENT(IN) :: pdz           (kproma,klev),         & ! geometric height thickness of layer [m] 
                            aer_tau_sw_vr (kproma,klev,kbndsw),  & ! SW aerosol optical depth
                            aer_ssa_sw_vr (kproma,klev,kbndsw),  & ! SW aerosol single scattering albedo
                            aer_asy_sw_vr (kproma,klev,kbndsw),  & ! SW aerosol asymmetry factor
                            aer_tau_lw_vr (kproma,klev,kbndlw)     ! LW aerosol optical depth

    IF (.NOT. lhamlite) RETURN

    IF (.NOT. lradiation) RETURN

    !IF (ltimer) CALL timer_start(timer_lite_radiation_2)

    !CALL ham_lite_rad_diag( jg, jb, jcs, jce, kproma, klev, kbndlw, kbndsw,                 &
    !                        pdz, aer_tau_sw_vr, aer_ssa_sw_vr, aer_asy_sw_vr, aer_tau_lw_vr )
    CALL ham_lite_rad_cache_cleanup 

    !IF (ltimer) CALL timer_stop(timer_lite_radiation_2)

 
  END SUBROUTINE radiation_subm_2

!---------------------------------------------------------------------------------------------------

  !SUBROUTINE interface_tmx_subm(  p_patch,                              &
  SUBROUTINE interface_tmx_subm(  jg, jb, jcs, jce,                     &
                                  kproma, klev, klevp1, ktrac,          &
                                  ksfc_type, idx_wtr, idx_ice, idx_lnd, &
                                  !datetime, pdtime,                     & 
                                  pdtime,                     & 
                                  plsmask, pglac, palake,               & 
                                  pseaice, psfcWind,                    &
                                  pclon, pclat,                         &
                                  pfrc_tile, pz0m_tile,                 &
                                  ptsfc_tile, psfcWind_tile,            &
                                  pkh_tile, pkm_tile, pmwind,           &
                                  pzf, pzh, pdz,                        &
                                  pmair, papm1, paphm1,                 &
                                  ptm1, prho, pqm1,                     &
                                  pxtm1,                                &
                                  ptend, pm6rp, prhop, pvgf, plai,      &
                                  pflux_ddep)

    USE mo_mpi_lite,                 ONLY: p_pe, p_io 
    !USE mo_model_domain,        ONLY: t_patch
    !USE mtime_lite,                       ONLY: t_datetime => datetime 
    USE mo_subm_main_lite,           ONLY: lemissions, ldrydep  
    USE mo_subm_trac_lite,           ONLY: trlist 
    USE mo_hammoz_lite_emi,          ONLY: emi_lite_interface
    USE mo_hammoz_lite_drydep,       ONLY: drydep_lite_interface
    !USE mo_hammoz_timer,        ONLY: timer_lite_emissions, timer_lite_drydep 
    USE mo_ham_lite_misc,            ONLY: lcheck_ddep, lcheck_emi,  &
                                      !ham_lite_mprops,          &
                                      ham_lite_check_r1d, ham_lite_check_r1d, ham_lite_check_wci
  
    IMPLICIT NONE

    INTEGER, INTENT(IN) ::  jg,       & ! grid index
                            jb,       & ! index of block loop
                            jcs, jce, & ! start and end indices of columns loop
                            kproma,   & ! maximum number of columns
                            klev,     & ! number of full levels
                            klevp1,   & ! number of half levels  
                            ktrac,    & ! number of tracers excluding water vapour and hydrometeors
                            ksfc_type, idx_wtr, idx_ice, idx_lnd  
    REAL(wp), INTENT(IN) :: pdtime,             & ! time step  
                            plsmask   (:),  & ! land-sea mask (1. = land, 0. = sea/lakes)     
                            pglac     (:),  & ! fraction of land covered by glaciers
                            palake    (:),  & ! lake fraction
                            pseaice   (:),  & ! fraction of ocean covered by sea ice 
                            psfcWind  (:),  & ! 10m windspeed
                            pclon     (:),  & ! longitude
                            pclat     (:),  & ! latitude  
                            pfrc_tile     (:,:),  & ! area fraction of each surface type (land/water/ice)
                            pz0m_tile     (:,:),  & ! surface roughness length
                            ptsfc_tile    (:,:),  & ! surface temperature  
                            psfcWind_tile (:,:),  & ! 10m windspeed          
                            pkh_tile      (:,:),  & ! exchange coefficient for scalar 
                            pkm_tile      (:,:),  & ! exchange coefficient for momentum 
                            pmwind        (:)       ! atm wind speed
    REAL(wp), INTENT(IN) :: pzf     (:,:),  & ! geometric height above sea level, full level [m] 
                            pzh     (:,:),  & ! geometric height above sea level, half level [m] 
                            pdz     (:,:),  & ! geometric height thickness of layer [m] 
                            pmair   (:,:),  & ! air mass [kg/m2] 
                            papm1   (:,:),  & ! air pressure, full level [Pa] 
                            paphm1  (:,:),  & ! air pressure, half level [Pa]    
                            ptm1    (:,:),  & ! air temperature [K]  
                            prho    (:,:),  & ! air density [kg/m3] 
                            pqm1    (:,:),  & ! specific humidity [kg/kg]
                            pxtm1   (:,:,:),& ! tracer number mixing ratio [#/kg]
                            pm6rp   (:,:,:,:),& ! aerosol wet radius per mode [m]
                            prhop   (:,:,:,:),& ! mean mode particle density [kg m-3]
                            pvgf    (:),      & ! vegetation fraction
                            plai    (:)         ! leaf area index
    
    REAL(wp), INTENT(OUT) :: pflux_ddep(:,:) !drydeposition flux per tracer

    !TYPE(t_patch),  INTENT(IN) :: p_patch  
    !TYPE(t_datetime), POINTER, INTENT(IN) :: datetime 
    REAL(wp), INTENT(INOUT) ::  ptend (:,:,:) ! tracer tendencies [#/kg/s] 

    REAL(wp), PARAMETER :: zeps = EPSILON(1.0_wp)

    LOGICAL :: ltest 
    INTEGER :: jl, jk, jt, nblks_c
    REAL(wp)  ::  zustar_tile(kproma,ksfc_type),  & ! friction velocity  
                  zfheat_tile(kproma,ksfc_type),  & ! heat profile
                  ztend_surf (kproma,ktrac)         ! tracer tendencies at surface due to
                                                    ! emission and dry deposition [#/m2/s]
               
    !$ACC DATA PRESENT(plsmask,pglac,palake,pseaice,psfcWind,pclon,pclat) &
    !$ACC  PRESENT(pfrc_tile,pz0m_tile,ptsfc_tile,psfcWind_tile) &
    !$ACC  PRESENT(pkh_tile,pkm_tile,pmwind,pzf,pzh,pdz,pmair,papm1,paphm1) &
    !$ACC  PRESENT(ptm1,prho,pqm1,pxtm1,ptend,pm6rp,prhop,pvgf,plai) &             
    !$ACC  CREATE(zustar_tile,zfheat_tile,ztend_surf)  

    nblks_c = 1 !p_patch%nblks_c

    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR COLLAPSE(3)
    DO jt = 1,ktrac
      DO jk = 1,klev
        DO jl = jcs,jce
          ptend (jl,jk,jt) = 0.0_wp 
        END DO 
      END DO 
    END DO
    !$ACC END PARALLEL LOOP

    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR COLLAPSE(2)
    DO jt = 1,ktrac 
      DO jl = jcs,jce
        ztend_surf (jl,jt) = 0.0_wp  
      END DO 
    END DO
    !$ACC END PARALLEL LOOP
    
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR COLLAPSE(2) PRIVATE(ltest)
    DO jk = 1,ksfc_type
      DO jl = jcs,jce
        zustar_tile (jl,jk) = SQRT(pkm_tile(jl,jk)) * pmwind(jl)
        ltest = (ABS(pkh_tile(jl,jk)) > zeps)
                
        IF (ltest .AND. (pkm_tile(jl,jk)>0._wp)) THEN
          zfheat_tile(jl,jk) = SQRT(pkm_tile(jl,jk)) / pkh_tile(jl,jk)
        ELSE
          zfheat_tile(jl,jk) = 0._wp
        END IF

        !zfheat_tile (jl,jk) = MERGE(SQRT(pkm_tile(jl,jk)) / pkh_tile(jl,jk), 0._wp, ltest)
      END DO 
    END DO
    !$ACC END PARALLEL LOOP

    ! diagnostics
    !$ACC UPDATE HOST(ptm1,papm1,pqm1) ASYNC(1)
    !$ACC WAIT(1)
    !CALL ham_lite_mprops( jg, jb, jcs, jce, kproma, klev, ptm1, papm1, pqm1 ) 

    ! emissions  
    ! IF (lemissions .AND. ANY(trlist%ti(:)%nemis > 0)) THEN

    !   !IF (ltimer) CALL timer_start(timer_lite_emissions)

    !   !$ACC UPDATE HOST(pzf,pzh,pdz,pclon,pclat,plsmask,pglac,palake,pseaice) &  
    !   !$ACC   HOST(paphm1,ptm1,pmair,psfcWind,pz0m_tile,zustar_tile,ptsfc_tile,ptend,ztend_surf) ASYNC(1) 
    !   !$ACC WAIT(1)

    !   CALL emi_lite_interface(  jg, jb, jcs, jce,                       &
    !                             nblks_c, kproma, ktrac, klev, klevp1,   &  
    !                             ksfc_type, idx_wtr, idx_ice, idx_lnd,   & 
    !                             datetime,                               &
    !                             pzf, pzh, pdz, pclon, pclat,            &
    !                             plsmask, pglac, palake, pseaice,        &
    !                             paphm1, ptm1, pmair, psfcWind,          & 
    !                             pz0m_tile, zustar_tile, ptsfc_tile,     &
    !                             ptend, ztend_surf                       ) 

    !   !$ACC UPDATE DEVICE(ptend,ztend_surf) ASYNC(1) 
    !   !$ACC WAIT(1)
                                
    !   !IF (ltimer) CALL timer_stop(timer_lite_emissions)

    ! END IF

    ! dry depostion  
    IF (ldrydep .AND. ANY(trlist%ti(:)%ndrydep > 0)) THEN

      !IF (ltimer) CALL timer_start(timer_lite_drydep)
  
      CALL drydep_lite_interface( jg, jb, jcs, jce,                       &
                                  nblks_c, kproma, klev, klevp1, ktrac,   &  
                                  ksfc_type, idx_wtr, idx_ice, idx_lnd,   & 
                                  !datetime, pdtime,                       &
                                  pdtime,                       &
                                  pzf, pzh, pdz,                          &
                                  papm1, prho, ptm1,                      & 
                                  pfrc_tile, ptsfc_tile, psfcWind_tile,   &
                                  zustar_tile, zfheat_tile,               & 
                                  pxtm1, ztend_surf, pm6rp, prhop, pvgf, plai)  
 
      !IF (ltimer) CALL timer_stop(timer_lite_drydep)

    END IF  

    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR COLLAPSE(2)  
    DO jt = 1,ktrac 
      DO jl = jcs,jce
        pflux_ddep(jl,jt) = -1.0_wp * ztend_surf(jl,jt) !eehol: assign drydep flux to diagnostics (multiply with -1 to get positive)
        ptend(jl,klev,jt) = ptend(jl,klev,jt) + ztend_surf(jl,jt) / pmair(jl,klev) 
      END DO  
    END DO
    !$ACC END PARALLEL LOOP

    ! Copy ptend back to host for accumulation in driver
    !$ACC UPDATE HOST(ptend) ASYNC(1)
    !$ACC WAIT(1)

    ! print checks 
    IF (lcheck_ddep) THEN       
      !$ACC UPDATE HOST(psfcWind,ptsfc_tile,psfcWind_tile,zustar_tile,zfheat_tile) ASYNC(1) 
      !$ACC WAIT(1)
      CALL ham_lite_check_r1d( jb, jcs, jce, kproma, psfcWind, 'interface_tmx_subm', 'psfcWind' ) 
      DO jt = 1,ksfc_type
        CALL ham_lite_check_wci( jb, jt, 'surface type' ) 
        CALL ham_lite_check_r1d( jb, jcs, jce, kproma, ptsfc_tile(:,jt), 'interface_tmx_subm', 'ptsfc_tile' )
        CALL ham_lite_check_r1d( jb, jcs, jce, kproma, psfcWind_tile(:,jt), 'interface_tmx_subm', 'psfcWind_tile' )
        CALL ham_lite_check_r1d( jb, jcs, jce, kproma, zustar_tile(:,jt), 'interface_tmx_subm', 'zustar_tile' )
        CALL ham_lite_check_r1d( jb, jcs, jce, kproma, zfheat_tile(:,jt), 'interface_tmx_subm', 'zfheat_tile' )
      END DO      
    END IF

    !$ACC END DATA
 
  END SUBROUTINE interface_tmx_subm

!---------------------------------------------------------------------------------------------------

  ! SUBROUTINE interface_aes_hamlite( jg, jb, jcs, jce )
 
  !   USE mo_mpi,               ONLY: p_pe, p_io 
  !   USE mo_aes_phy_dims,      ONLY: aes_phy_dims
  !   USE mo_aes_phy_config,    ONLY: aes_phy_tc
  !   USE mo_aes_phy_memory,    ONLY: t_aes_phy_field, t_aes_phy_tend, prm_field, prm_tend
  !   USE mo_run_config,        ONLY: ntracer, iqv, iqt 
  !   USE mo_subm_trac,         ONLY: ntrac

  !   IMPLICIT NONE

  !   INTEGER, INTENT(IN) ::  jg, jb, jcs, jce 

  !   INTEGER :: jc, jk, js, jt
  !   INTEGER :: nproma, nlev, nlevp1 
  !   REAL(wp)  :: pdtime
  !   REAL(wp) :: tend_qa_ham(aes_phy_dims(jg)%nproma,aes_phy_dims(jg)%nlev,ntrac) 
  !   LOGICAL ::  is_in_sd_ed_interval, is_active 
  !   TYPE(t_aes_phy_field), POINTER :: field    
  !   TYPE(t_aes_phy_tend), POINTER :: tend
  
  !   nproma  = aes_phy_dims(jg)%nproma
  !   nlev    = aes_phy_dims(jg)%nlev
  !   nlevp1  = nlev + 1

  !   pdtime                = aes_phy_tc(jg)%dt_phy_sec
  !   is_in_sd_ed_interval  = aes_phy_tc(jg)%is_in_sd_ed_interval_ham
  !   is_active             = aes_phy_tc(jg)%is_active_ham

  !   ! associate pointers
  !   field => prm_field(jg)
  !   tend  => prm_tend(jg)

  !   !$ACC DATA PRESENT(field,tend) &
  !   !$ACC   CREATE(tend_qa_ham)   

  !   IF ( is_in_sd_ed_interval .AND. is_active ) THEN
  
  !     CALL aes_phy_main_subm (  jg, jb, jcs, jce,                         & 
  !                               nproma, nlev, nlevp1, ntrac,              &
  !                               pdtime,                                   &    
  !                               field% dz       (:,:,jb),                 & 
  !                               field% ta       (:,:,jb),                 &  
  !                               field% mair     (:,:,jb),                 & 
  !                               field% rho      (:,:,jb),                 &
  !                               field% pfull    (:,:,jb),                 &
  !                               field% phalf    (:,:,jb),                 &
  !                               field% qtrc_phy (:,:,jb,iqv),             &   
  !                               field% qtrc_phy (:,:,jb,iqt:),            & ! in 
  !                               tend_qa_ham     (:,:,:)                   ) ! inout 

  !     DO jt = iqt,ntracer
  !       js = jt - iqt + 1
  !       !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
  !       !$ACC LOOP GANG VECTOR COLLAPSE(2)
  !       DO jk = 1,nlev
  !         DO jc = jcs,jce         
  !           tend%qtrc_phy(jc,jk,jb,jt) = tend%qtrc_phy(jc,jk,jb,jt) + tend_qa_ham(jc,jk,js)              
  !           field%qtrc_phy(jc,jk,jb,jt) = field%qtrc_phy(jc,jk,jb,jt) + tend_qa_ham(jc,jk,js) * pdtime
  !         END DO
  !       END DO
  !       !$ACC END PARALLEL LOOP
  !     END DO

  !   END IF
 
  !   !$ACC END DATA
 
  ! END SUBROUTINE interface_aes_hamlite

!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE aes_phy_main_subm (  jg, jb, jcs, jce,             &
                                  kproma, klev, klevp1, ktrac,  &
                                  pdtime,                       &  
                                  pdz, ptm1,                    &  
                                  pmair, prho,                  &  
                                  papm1, paphm1,                &  
                                  pqm1, pxtm1, ptend, pm6rp,    &
                                  prhop, pflux_sedi)  
                  
    USE mo_subm_main_lite,           ONLY: lhamlite, lsedimentation, lburden 
    USE mo_subm_trac_lite,           ONLY: trlist
    !USE mo_subm_trac_proc,      ONLY: xt_burden
    !USE mo_hammoz_timer,        ONLY: timer_lite_sedimentation, timer_lite_burden
    USE mo_hammoz_lite_sed,          ONLY: sed_lite_interface 
    USE mo_ham_lite_misc,            ONLY: ham_lite_mprops  

    IMPLICIT NONE
    
    INTEGER, INTENT(IN) ::  jg,       & ! grid index 
                            jb,       & ! index of block loop
                            jcs, jce, & ! start and end indices of columns loop
                            kproma,   & ! maximum number of columns
                            klev,     & ! number of full levels
                            klevp1,   & ! number of half levels (klev + 1)
                            ktrac       ! number of tracers excluding water vapour and hydrometeors
    REAL(wp), INTENT(IN) :: pdtime,         & ! time step [s]  
                            pdz     (:,:),  & ! geometric height thickness of layer [m]
                            ptm1    (:,:),  & ! air temperature [K]  
                            pmair   (:,:),  & ! air mass [kg/m2] 
                            prho    (:,:),  & ! air density [kg/m3]
                            papm1   (:,:),  & ! air pressure at full level [Pa]
                            paphm1  (:,:),  & ! air pressure at half level [Pa] 
                            pqm1    (:,:),  & ! specific humidity [kg/kg] 
                            pxtm1   (:,:,:),& ! tracer number mixing ratio [#/kg]
                            pm6rp   (:,:,:,:),& ! aerosol wet radius per mode [m]
                            prhop   (:,:,:,:)   ! mean mode particle density [kg m-3]
    REAL(wp), INTENT(INOUT) ::  ptend (:,:,:) ! tracer tendency [#/kg/s]
    REAL(wp), INTENT(INOUT) ::  pflux_sedi (:,:) ! sedimentation flux [#/kg/s]
             
    INTEGER  :: jl, jk, jt 

    IF (.NOT. lhamlite) RETURN

    !$ACC DATA PRESENT(pdz,ptm1,pmair,prho,papm1,paphm1,pqm1,pxtm1,ptend)

    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR COLLAPSE(3)
    DO jt = 1,ktrac
      DO jk = 1,klev
        DO jl = jcs,jce
          ptend(jl,jk,jt) = 0.0_wp
        END DO 
      END DO
    END DO
    !$ACC END PARALLEL LOOP

    ! diagnostics
    !$ACC UPDATE HOST(ptm1,papm1,pqm1) ASYNC(1)
    !$ACC WAIT(1)
    !CALL ham_lite_mprops( jg, jb, jcs, jce, kproma, klev, ptm1, papm1, pqm1 ) 
 
    ! aerosol sedimentation
    IF (lsedimentation .AND. ANY(trlist%ti(:)%nsedi > 0)) THEN

      !IF (ltimer) CALL timer_start(timer_lite_sedimentation)

      CALL sed_lite_interface(  jg, jb, jcs, jce, kproma, klev,   &
                                pdtime, pdz, pmair, prho, ptm1,   &
                                papm1, pxtm1, ptend, pm6rp, prhop,&
                                pflux_sedi )
                                  
      !IF (ltimer) CALL timer_stop(timer_lite_sedimentation)

    END IF 

    ! tracer burdens
    ! IF (lburden) THEN
 
    !   !IF (ltimer) CALL timer_start(timer_lite_burden)

    !   CALL xt_burden( jg, jb, jcs, jce, kproma, klev, klevp1, &
    !                   pdtime, pmair, pxtm1, ptend             )

    !   !IF (ltimer) CALL timer_stop(timer_lite_burden)

    ! END IF

    ! Copy ptend back to host for accumulation in driver
    !$ACC UPDATE HOST(ptend) ASYNC(1)
    !$ACC WAIT(1)

    !$ACC END DATA
 
  END SUBROUTINE aes_phy_main_subm
 
END MODULE mo_subm_interface_lite

