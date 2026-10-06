! 
! based on mo_ham_m7_emissions.f90  
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

MODULE mo_ham_lite_emi

  USE mo_kind_lite,            ONLY: dp
  USE mo_ham_lite_main,   ONLY: nclass, mw_so2, mw_so4, mw_s, sigma_fine, sigma_coarse

  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: ham_lite_init_emi,    &
            ham_lite_emi,         &
            ham_lite_get_factor
 
  !--------------------
  ! module variables
  !--------------------

  ! conversion factors from emission masses into aerosol numbers 
  REAL(dp), ALLOCATABLE ::  zm2n_bc(:),   &
                            zm2n_oc(:),   &
                            zm2n_so4(:),  &
                            zm2n_du(:),   &
                            zm2n_ss(:)

  ! sector names                     
  CHARACTER (LEN=10), ALLOCATABLE :: sect_name(:)

  !--------------------
  ! module parameters
  !--------------------

  REAL(dp), PARAMETER ::        & 
    zfacoc    = 3.4_dp,         & ! scaling factor for oc emissions (Kaiser et al., 2012) 
    zomtoc    = 1.4_dp,         & ! conversion factor from pom to oc 
    zso2tso4  = mw_so4/mw_so2     ! conversion factor from so2 to so4   

  CONTAINS

!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_init_emi( nsectors, p_patch, datetime )

    USE mtime_lite,                      ONLY: t_datetime => datetime
    USE mo_model_domain_lite,            ONLY: t_patch
    USE mo_math_constants_lite,          ONLY: pi 
    USE mo_util_string_lite,             ONLY: toupper
    USE mo_util_extfield_lite,           ONLY: EF_MODULE
    USE mo_subm_bcs_lite,                ONLY: bc_query
    USE mo_subm_spec_lite,               ONLY: speclist 
    USE mo_hammoz_lite_emi_mat,          ONLY: em_get_sector_info
    USE mo_ham_lite_main,                ONLY: sizeclass, cmr2ram, ibc_dust,  &
                                          idsec_dust, idsec_seasalt, idsec_biogenic, idsec_wfire
    USE mo_ham_lite_dust,                ONLY: bgc_dust_init
    USE mo_ham_lite_spec,                ONLY: id_bc, id_oc, id_so4, id_du, id_ss  

    TYPE(t_patch), INTENT(IN) :: p_patch
    TYPE(t_datetime), POINTER, INTENT(IN) :: datetime
    INTEGER, INTENT(IN)                 :: nsectors ! number of sectors defined in emi matrix

    INTEGER           ::  jsect, jmode, nvars, ieftype
    REAL(dp)          ::  zradius
    CHARACTER(LEN=64) ::  secname

    !-- initialize
    idsec_dust      = -1
    idsec_seasalt   = -1            
    idsec_biogenic  = -1
    idsec_wfire     = -1

    IF (.NOT. ALLOCATED(sect_name)) ALLOCATE(sect_name(nsectors))

    DO jsect = 1, nsectors
      CALL em_get_sector_info(jsect, secname, nvars)
      sect_name(jsect) = (TRIM(toupper(secname)))

      IF ((TRIM(toupper(secname)) == 'DUST') .AND. (nvars > 0))     idsec_dust = jsect
      IF ((TRIM(toupper(secname)) == 'SEASALT') .AND. (nvars > 0))  idsec_seasalt = jsect
      IF ((TRIM(toupper(secname)) == 'BIOGENIC') .AND. (nvars > 0)) idsec_biogenic = jsect           
      IF ((TRIM(toupper(secname)) == 'WFIRE') .AND. (nvars > 0))    idsec_wfire = jsect    
    END DO

    !-- get conversion factor from mass to number flux  
    !
    !    N = M/m = M / (4/3 * pi * dens * r_average-mass**3)
    !            = M * 3 / (4 * pi * dens * r_average-mass**3)
    !            = M * zm2n_xx
    
    IF (.NOT. ALLOCATED(zm2n_bc))   ALLOCATE(zm2n_bc(nclass))
    IF (.NOT. ALLOCATED(zm2n_oc))   ALLOCATE(zm2n_oc(nclass))
    IF (.NOT. ALLOCATED(zm2n_so4))  ALLOCATE(zm2n_so4(nclass))
    IF (.NOT. ALLOCATED(zm2n_du))   ALLOCATE(zm2n_du(nclass))
    IF (.NOT. ALLOCATED(zm2n_ss))   ALLOCATE(zm2n_ss(nclass))

    DO jmode = 1,nclass

      zradius = sizeclass(jmode)%lite_radi * cmr2ram(jmode) 

      zm2n_bc(jmode)  = 3._dp / (4._dp * pi * speclist(id_bc)%density * zradius**3) 
      zm2n_oc(jmode)  = 3._dp / (4._dp * pi * speclist(id_oc)%density * zradius**3) 
      zm2n_so4(jmode) = 3._dp / (4._dp * pi * speclist(id_so4)%density * zradius**3) 
      zm2n_du(jmode)  = 3._dp / (4._dp * pi * speclist(id_du)%density * zradius**3) 
      zm2n_ss(jmode)  = 3._dp / (4._dp * pi * speclist(id_ss)%density * zradius**3) 
      
    END DO  

    ! initialize dust module
    IF (idsec_dust > 0) THEN   
      CALL bc_query(p_patch%id, ibc_dust, ef_type=ieftype) 
      IF (ieftype == EF_MODULE) CALL bgc_dust_init( p_patch%id, p_patch ) 
    END IF

  END SUBROUTINE ham_lite_init_emi

!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_emi(  jg, jb, jcs, jce,                           &
                            kblks_c, kproma, klev, ktrac, ksect, kspec, &
                            ksfc_type, idx_wtr, idx_ice, idx_lnd,       & 
                            datetime,                                   &
                            plsmask, pglac, palake, pseaice, psfcWind,  &
                            pz0m_tile, pustar_tile, ptsfc_tile,         &       
                            ibc_extra, pfactor                          )

    USE mtime_lite,                  ONLY: t_datetime => datetime 
    USE mo_util_extfield_lite,       ONLY: EF_MODULE
    USE mo_subm_bcs_lite,            ONLY: bc_query 
    USE mo_subm_spec_lite,           ONLY: nmaxtrspec
    USE mo_ham_lite_main,            ONLY: sizeclass, idsec_dust, ibc_dust, &
                                      idsec_seasalt, ibc_seasalt, idsec_biogenic, idsec_wfire
    USE mo_ham_lite_spec,            ONLY: id_du, id_ss, id_bc, id_oc, id_so4 

    INTEGER, INTENT(IN) ::  jg,         & ! grid index
                            jb,         & ! index of block loop
                            jcs, jce,   & ! start and end indices of columns loop
                            kblks_c,    & ! number of geographic blocks
                            kproma,     & ! maximum number of columns
                            klev,       & ! number of levels 
                            ktrac,      & ! number of tracers
                            ksect,      & ! emission sector number (match with idsec_xyz)
                            kspec,      & ! species number!
                            ksfc_type,  & ! number of surface types 
                            idx_wtr,    & ! index for water
                            idx_ice,    & ! index for ice  
                            idx_lnd       ! index for land
    REAL(dp), INTENT(IN)  ::  plsmask     (kproma),           & ! land-sea mask (1. = land, 0. = sea/lakes)     
                              pglac       (kproma),           & ! fraction of land covered by glaciers
                              palake      (kproma),           & ! lake fraction
                              pseaice     (kproma),           & ! sea ice fraction
                              psfcWind    (kproma),           & ! 10m wind speed
                              pz0m_tile   (kproma,ksfc_type), & ! surface roughness length
                              pustar_tile (kproma,ksfc_type), & ! friction velocity 
                              ptsfc_tile  (kproma,ksfc_type)    ! surface temperature 
    TYPE(t_datetime), POINTER, INTENT(IN) :: datetime 
    INTEGER,  INTENT(INOUT) :: ibc_extra(nmaxtrspec)  ! some sectors (dust, seasalt) use more than one bc
    REAL(dp), INTENT(INOUT) :: pfactor(nmaxtrspec)    ! weighting factor for size distribution
 
    INTEGER :: ieftype, jmode, jsect, nsect 

    ibc_extra(:) = 0
    pfactor(:) = 0.0_dp

    !--- dust emissions
    IF (ksect == idsec_dust .AND. kspec == id_du) THEN
      CALL bc_query(jg, ibc_dust, ef_type=ieftype) 
      IF (ieftype == EF_MODULE) THEN
        CALL ham_lite_dust_emissions( jg, jb, jcs, jce,                       &
                                      kblks_c, kproma,                        &
                                      ksfc_type, idx_wtr, idx_ice, idx_lnd,   & 
                                      datetime,                               & 
                                      plsmask, pglac, psfcWind,               &
                                      pz0m_tile, pustar_tile                  )
        DO jmode = 1,nclass
          nsect = sizeclass(jmode)%lite_nemi
          DO jsect = 1,nsect
            IF (sizeclass(jmode)%lite_iemi(jsect) == sect_name(ksect)) THEN
              ibc_extra(jmode) = ibc_dust  
              pfactor(jmode) = zm2n_du(jmode)
            END IF 
          END DO
        END DO  
      END IF 
    END IF

    !--- seasalt emissions
    IF (ksect == idsec_seasalt .AND. kspec == id_ss) THEN 
      CALL ham_lite_seasalt_emissions(  jg, jb, jcs, jce, kblks_c, kproma,            &
                                        ksfc_type, idx_wtr,                           &
                                        plsmask,                                      &
                                        pglac, palake, pseaice, psfcWind, ptsfc_tile  )
      DO jmode = 1,nclass
        nsect = sizeclass(jmode)%lite_nemi
          DO jsect = 1,nsect
            IF (sizeclass(jmode)%lite_iemi(jsect) == sect_name(ksect)) THEN
              ibc_extra(jmode) = ibc_seasalt   
              pfactor(jmode) = zm2n_ss(jmode) 
            END IF 
          END DO
      END DO 
    END IF

    DO jmode = 1,nclass
      nsect = sizeclass(jmode)%lite_nemi

      DO jsect = 1,nsect
        IF (sizeclass(jmode)%lite_iemi(jsect) == sect_name(ksect)) THEN 
            
          IF (kspec == id_bc) THEN
            !--- black carbon emissions
            pfactor(jmode) = zm2n_bc(jmode)
          ELSE IF (kspec == id_oc) THEN
            !--- organic carbon emissions
            IF (ksect == idsec_biogenic) THEN 
              pfactor(jmode) = zm2n_oc(jmode)   
            ELSE IF (ksect == idsec_wfire) THEN 
              pfactor(jmode) = zfacoc * zm2n_oc(jmode)   
            ELSE 
              pfactor(jmode) = zomtoc * zm2n_oc(jmode)
            ENDIF
          ELSE IF (kspec == id_so4) THEN
            !--- sulfur emissions 
            pfactor(jmode) = zso2tso4 * zm2n_so4(jmode) 
          END IF

        END IF 
      END DO

    END DO

  END SUBROUTINE ham_lite_emi

!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_get_factor( ksect, kspec, pfactor )

    USE mo_subm_spec_lite,             ONLY: nmaxtrspec
    USE mo_ham_lite_main,              ONLY: sizeclass, idsec_biogenic, idsec_wfire
    USE mo_ham_lite_spec,              ONLY: id_bc, id_oc, id_so4  
  
    INTEGER,  INTENT(IN) :: ksect,  & ! emission sector number  
                            kspec     ! species number  
    REAL(dp), INTENT(INOUT) :: pfactor(nmaxtrspec) ! weighting factor for size distribution

    INTEGER :: jmode, jsect, nsect 
 
    pfactor(:) = 0.0_dp

    DO jmode = 1,nclass
      nsect = sizeclass(jmode)%lite_nemi

      DO jsect = 1,nsect
        IF (sizeclass(jmode)%lite_iemi(jsect) == sect_name(ksect)) THEN   

          IF (kspec == id_bc) THEN 
            !--- black carbon emissions 
            pfactor(jmode) = zm2n_bc(jmode)  
          ELSE IF (kspec == id_oc) THEN
            !--- organic carbon emissions
            IF (ksect == idsec_biogenic) THEN 
              pfactor(jmode) = zm2n_oc(jmode)   
            ELSE IF (ksect == idsec_wfire) THEN 
              pfactor(jmode) = zfacoc * zm2n_oc(jmode)   
            ELSE 
              pfactor(jmode) = zomtoc * zm2n_oc(jmode)
            ENDIF 
          ELSE IF (kspec == id_so4) THEN
            !--- sulfur emissions 
            pfactor(jmode) = zso2tso4 * zm2n_so4(jmode) 
          END IF

        END IF
      END DO

    END DO 

  END SUBROUTINE ham_lite_get_factor

!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_dust_emissions( jg, jb, jcs, jce,                       &
                                      kblks_c, kproma,                        &
                                      ksfc_type, idx_wtr, idx_ice, idx_lnd,   &
                                      datetime,                               &
                                      plsmask, pglac, psfcWind,               &
                                      pz0m_tile, pustar_tile                  )

    USE mtime_lite,                  ONLY: t_datetime => datetime 
    USE mo_exception_lite,           ONLY: finish
    USE mo_subm_bcs_lite,            ONLY: bc_set
    USE mo_ham_lite_main,            ONLY: ibc_dust
    USE mo_ham_lite_dust,            ONLY: bgc_dust_calc_emis, ntrace 
    USE mo_ham_lite_spec,            ONLY: id_du
 
    IMPLICIT NONE 

    INTEGER,  INTENT(IN)  ::  jg,         &   ! grid index
                              jb,         &   ! index of block loop
                              jcs, jce,   &   ! start and end indices of columns loop
                              kblks_c,    &   ! number of geographic blocks
                              kproma,     &   ! maximum number of columns 
                              ksfc_type, idx_wtr, idx_ice, idx_lnd   
    REAL(dp), INTENT(IN)  ::  plsmask     (kproma),            & ! land-sea mask (1. = land, 0. = sea/lakes)     
                              pglac       (kproma),            & ! fraction of land covered by glaciers
                              psfcWind    (kproma),            & ! 10m surface wind 
                              pz0m_tile   (kproma,ksfc_type),  & ! surface roughness length
                              pustar_tile (kproma,ksfc_type)  
    TYPE(t_datetime), POINTER, INTENT(IN) :: datetime 
 
    INTEGER, PARAMETER  ::  ibmin = 1, ibmax = 4  ! index boundaries for bins
    INTEGER ::  ibin, jl 
    REAL(dp)  ::  zfbin(kproma,ntrace), &
                  zmass(kproma),        &    
                  ztmp1(kproma)    
    LOGICAL ::  ltest            

    ! calculate dust emissions [kg m-2 s-1] 
    CALL bgc_dust_calc_emis(  jg, jb, jcs, jce,                               &
                              kblks_c, kproma,                                & 
                              datetime,                                       &
                              plsmask, pglac, psfcWind,                       &
                              pz0m_tile(:,idx_lnd), pustar_tile(:,idx_lnd),   &
                              zfbin                                           )

    ! sum over bins of emission scheme
    DO jl = jcs,jce
      zmass(jl) = 0._dp 
    END DO 

    DO ibin = ibmin, ibmax
      DO jl = jcs,jce
        zmass(jl) = zmass(jl) + zfbin(jl,ibin)
      END DO 
    END DO 

    ! make sure mass flux is positive  
    DO jl = jcs,jce
      ltest = (zmass(jl) <= 0._dp)
      zmass(jl) = MERGE( 0._dp, zmass(jl), ltest)
    END DO 

    ! mask out glacier and ocean values 
    DO jl = jcs,jce
      ltest = (pglac(jl) > 0.5_dp) .OR. (plsmask(jl) < 0.5_dp)
      zmass(jl) = MERGE(0._dp, zmass(jl), ltest)
    END DO

    ! reduce flux over glacier grids according to glacier fraction 
    DO jl = jcs,jce
      ltest = (pglac(jl) > 0.1_dp)
      ztmp1(jl) = (1.0_dp - pglac(jl))**3.0_dp
      zmass(jl) = MERGE(zmass(jl) * ztmp1(jl), zmass(jl), ltest)
    END DO

    ! reduce flux over not-all-land grids according to land fraction
    DO jl = jcs,jce
      ltest = (plsmask(jl) < 0.99_dp)
      ztmp1(jl) = MIN(plsmask(jl), 0.7_dp)**4.0_dp
      zmass(jl) = MERGE(zmass(jl) * ztmp1(jl), zmass(jl), ltest)
      zmass(jl) = MAX(zmass(jl), 0._dp)
    END DO

    CALL bc_set(jg, jb, jcs, jce, ibc_dust, kblks_c, kproma, zmass)

  END SUBROUTINE ham_lite_dust_emissions
 
!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_seasalt_emissions(  jg, jb, jcs, jce, kblks_c,                              &
                                          kproma, ksfc_type, idx_wtr,                             &
                                          plsmask, pglac, palake, pseaice, psfcWind, ptsfc_tile   )

    USE mo_subm_bcs_lite,            ONLY: bc_set 
    USE mo_ham_lite_main,            ONLY: nseasalt, ibc_seasalt
    USE mo_ham_lite_seasalt,         ONLY: emi_seasalt_long, emi_seasalt_gong
 
    IMPLICIT NONE 

    INTEGER, INTENT(IN) ::  jg,         & ! grid index
                            jb,         & ! index of block loop
                            jcs, jce,   & ! start and end indices of columns loop
                            kproma,     & ! maximum number of columns 
                            kblks_c,    & ! number of geographic blocks
                            ksfc_type,  & ! number of surface types 
                            idx_wtr       ! index for water
    REAL(dp), INTENT(IN)  ::  plsmask(kproma),            & ! land-sea mask (1. = land, 0. = sea/lakes)     
                              pglac(kproma),              & ! fraction of land covered by glaciers
                              palake(kproma),             & ! lake fraction
                              pseaice(kproma),            & ! sea ice fraction
                              psfcWind(kproma),           & ! 10m wind speed
                              ptsfc_tile(kproma,ksfc_type)  ! surface temperature 

    INTEGER :: jl 
    REAL(dp)  ::  zmass(kproma), znumb(kproma)                             

    IF (nseasalt == 1) THEN
      CALL emi_seasalt_long(  jcs, jce, kproma,                               &
                              plsmask, palake, pseaice,                       & 
                              psfcWind, ptsfc_tile(:,idx_wtr), zmass, znumb   ) 
    ELSE IF (nseasalt == 2) THEN
      CALL emi_seasalt_gong(  jcs, jce, kproma,                               &
                              plsmask, palake, pseaice,                       & 
                              psfcWind, ptsfc_tile(:,idx_wtr), zmass, znumb   ) 
    END IF

    CALL bc_set(jg, jb, jcs, jce, ibc_seasalt, kblks_c, kproma, zmass)

  END SUBROUTINE ham_lite_seasalt_emissions

END MODULE mo_ham_lite_emi
