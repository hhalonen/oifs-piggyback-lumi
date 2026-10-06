! 
! based on mo_ham_drydep.f90 
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

MODULE mo_ham_lite_drydep

  USE mo_kind_lite,                ONLY: dp 
  !USE mo_ham_lite_streams,    ONLY: ham_field ! rwet, daer
 
  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: ham_lite_prep_drydep, & ! calculate aerodynamc resistance 
            ham_lite_drydep         ! calculate dry deposition velocity 

  !--------------------
  ! module parameters
  !--------------------
 
  REAL(dp), PARAMETER :: zeps = EPSILON(1.0_dp)

  CONTAINS
  
!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE ham_lite_prep_drydep(  jg, jcs, jce, kproma,                   &
                                    ksfc_type, idx_wtr, idx_ice, idx_lnd,   &  
                                    papm1, prho, ptm1,                      & 
                                    pfrc_tile, pustar_tile, pfheat_tile,    & 
                                    pdyvi, pkivi, pmfpa, praero_tile        )
 
    USE mo_ham_lite_misc,   ONLY: ham_lite_mfpath, ham_lite_dyvisc 

    INTEGER, INTENT(IN) ::  jg, jcs, jce, kproma, &
                            ksfc_type,            &     ! number of surface types 
                            idx_wtr, idx_ice, idx_lnd   ! index for water, ice, land 
    REAL(dp), INTENT(IN)  ::  papm1       (:),    &  
                              prho        (:),    &  
                              ptm1        (:),    &   
                              pfrc_tile   (:,:),  &  
                              pustar_tile (:,:),  &  
                              pfheat_tile (:,:) 
    REAL(dp), INTENT(INOUT) ::  pdyvi   (:),  &  
                                pkivi   (:),  &  
                                pmfpa   (:),  &   
                                praero_tile (:,:)
 
    INTEGER   ::  jl, jls, js, jsfc,  &
                  loidx(kproma,ksfc_type), is(ksfc_type) ! counter for masks  
    LOGICAL   ::  ltest 

    !$ACC DATA PRESENT(papm1,prho,ptm1,pfrc_tile,pustar_tile,pfheat_tile) &
    !$ACC   PRESENT(pdyvi,pkivi,pmfpa,praero_tile) &
    !$ACC   CREATE(loidx,is)  

    ! get dynamic viscosity, kinematic viscosity, and mean free path
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR  
    DO jl = jcs,jce
      pdyvi(jl) = ham_lite_dyvisc(ptm1(jl))   
      pkivi(jl) = pdyvi(jl) / prho(jl)   
      pmfpa(jl) = ham_lite_mfpath(ptm1(jl), papm1(jl))  
    END DO
    !$ACC END PARALLEL LOOP
    
    ! get aerodynamic resistance for each surface cover [s/m] 
    ! Ganzeveld and Lelieveld (1995) 
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR COLLAPSE(2)
    DO jsfc = 1,ksfc_type
      DO jl = 1,kproma  
        praero_tile(jl,jsfc) = 0._dp
      END DO
    END DO
    !$ACC END PARALLEL LOOP
    
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR  
    DO jsfc = 1,ksfc_type ! surface type loop

      ! surface type mask
      is(jsfc) = 0 
      !$ACC LOOP SEQ
      DO jl = jcs,jce
        IF (pfrc_tile(jl,jsfc) .GT. 0._dp) THEN
          is(jsfc) = is(jsfc) + 1
          loidx(is(jsfc),jsfc) = jl
        END IF
      END DO  

      ! loop over mask only 
      !$ACC LOOP SEQ
      DO jls = 1,is(jsfc) ! jls loop
        js = loidx(jls,jsfc)
        ltest = (pustar_tile(js,jsfc) > zeps)
        IF (ltest) THEN
          praero_tile(js,jsfc) = pfheat_tile(js,jsfc) / pustar_tile(js,jsfc)
        ELSE
          praero_tile(js,jsfc) = 0._dp
        END IF
      END DO   

    END DO 
    !$ACC END PARALLEL LOOP

    !$ACC END DATA

  END SUBROUTINE ham_lite_prep_drydep

!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE ham_lite_drydep( jg, jb, jcs, jce, jt,                   &
                              kproma, klev,                           &
                              ksfc_type, idx_wtr, idx_ice, idx_lnd,   &    
                              pdtime,                                 & 
                              pdz, ptm1, prho,                        &
                              pvgf, plai, pdyvi, pkivi, pmfpa,        &
                              pfrc_tile, ptsfc_tile, psfcWind_tile,   &
                              pustar_tile, praero_tile,               &
                              pvdep, pm6rp, prhop                     )

    ! calculate dry deposition velocity of a log-normal aerosol distribution   
    ! [1] Pleim, Ran, Saylor, Willison, and Binkowski (2022) 
    ! [2] Riemer (2002)
    ! 
    ! equation numbers from reference [1] unless otherwise quoted 

    USE mo_math_constants_lite,      ONLY: pi
    USE mo_physical_constants_lite,  ONLY: grav, ak  
    USE mo_subm_trac_lite,           ONLY: trlist    
    USE mo_ham_lite_main,            ONLY: sizeclass, silog_mode, nclass
    USE mo_ham_lite_misc,            ONLY: lcheck_ddep, ham_lite_check_r1d,  &  
                                      ham_lite_check_wcc, ham_lite_check_wci 

    IMPLICIT NONE
 
    INTEGER, INTENT(IN) ::  jg, jb, jcs, jce, jt,     &
                            kproma, klev,             &
                            ksfc_type,                & ! number of surface types 
                            idx_wtr, idx_ice, idx_lnd   ! index for water, ice, land  
    REAL(dp), INTENT(IN)  ::  pdtime,               &   
                              pdz   (:),            &  
                              ptm1  (:),            & 
                              prho  (:),            & 
                              pvgf  (:),            &   
                              plai  (:),            & 
                              pdyvi (:),            &  
                              pkivi (:),            &  
                              pmfpa (:),            &   
                              pfrc_tile     (:,:),  & 
                              ptsfc_tile    (:,:),  & 
                              psfcWind_tile (:,:),  & 
                              pustar_tile   (:,:),  &
                              praero_tile   (:,:),  &
                              pm6rp         (:,:,:,:),& ! aerosol wet radius per mode [m]
                              prhop         (:,:,:,:)   ! mean mode particle density [kg m-3]
    REAL(dp), INTENT(INOUT) :: pvdep(:)
   
    INTEGER, PARAMETER :: nsurf = 4 ! number of surface types (land/water/ice/vegetation)
    REAL(dp), PARAMETER ::  zf_mic = 0.008_dp,  & ! fraction of impaction due to microscale features
                            za_mic = 1.0E-6_dp, & ! size of microscale features [m] (table 1)
                            za_mac = 10.E-3_dp    ! size of macroscale features [m] (table 1)

    INTEGER :: idx_veg, jl, jk, js, jclass 
    LOGICAL :: ltest 
    REAL(dp)  ::  ztemp, zsigm, zknud,  &
                  zt_cls, za_fac, zb_fac, zf_cap, zs_mic, zs_mac, zebd_tile, zeim_tile
    REAL(dp)  ::  zdiff(kproma), zvsed(kproma), &   
                  zustar_tile(kproma,nsurf), zraero_tile(kproma,nsurf), zrsurf_tile(kproma,nsurf) 
    REAL(dp)  ::  zdwet(kproma),  & ! wet diameter [m] 
                  zvdep(kproma)     ! dry deposition velocity [m/s] 
    !REAL(dp), POINTER :: rwet_p(:,:,:), daer_p(:,:,:) 
    REAL(dp) :: rwet_p(kproma,klev,jb), daer_p(kproma,klev,jb)
 
    !$ACC DATA PRESENT(pdz,ptm1,prho,pvgf,plai,pdyvi,pkivi,pmfpa) &
    !$ACC   PRESENT(pfrc_tile,ptsfc_tile,psfcWind_tile,pustar_tile,praero_tile,pvdep) &
    !$ACC   PRESENT(pm6rp,prhop) &
    !$ACC   CREATE(zdiff,zvsed,zustar_tile,zraero_tile,zrsurf_tile,zdwet,zvdep,rwet_p,daer_p)  

    idx_veg = nsurf
 
    !--- copy aerodynamic resistance and friction velocity 
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR  
    DO jl = jcs,jce

      zustar_tile(jl,idx_veg) = pustar_tile(jl,idx_lnd)
      zraero_tile(jl,idx_veg) = praero_tile(jl,idx_lnd)

      !$ACC LOOP SEQ
      DO js = 1,ksfc_type
        zustar_tile(jl,js) = pustar_tile(jl,js)
        zraero_tile(jl,js) = praero_tile(jl,js)
      END DO

    END DO 
    !$ACC END PARALLEL LOOP

    ! get standard deviation, wet diameter, and wet density
    jclass  = trlist%ti(jt)%mode
    zsigm   = silog_mode(jclass) 
    
    ! Copy wet radius and density slices to local arrays on GPU
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR COLLAPSE(2)
    DO jk = 1,klev
      DO jl = jcs,jce
        rwet_p(jl,jk,jb) = pm6rp(jl,jk,jb,jclass)
        daer_p(jl,jk,jb) = prhop(jl,jk,jb,jclass)
      END DO
    END DO
    !$ACC END PARALLEL LOOP

    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR  
    DO jl = jcs,jce
      zdwet(jl) = 2._dp * rwet_p(jl,klev,jb) 
    END DO
    !$ACC END PARALLEL LOOP

    !--- calculate settling velocity, Schmidt number, and Stokes number
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR PRIVATE(zknud)
    DO jl = jcs,jce

      ! Guard against zero wet diameter (mode with no mass / uninit)
      IF (zdwet(jl) <= zeps) THEN
        zknud      = 0._dp
        zdiff(jl)  = 0._dp
        zvsed(jl)  = 0._dp
        CYCLE
      END IF

      ! Knudsen number 
      zknud = 2._dp * pmfpa(jl) / zdwet(jl)
 
      ! diffusivity [m2/s] (equation 15 in [1] and  3.68 in [2])
      zdiff(jl) = (ak * ptm1(jl)) / (3._dp * pi * pdyvi(jl) * zdwet(jl))
      zdiff(jl) = zdiff(jl) * (exp(0.5_dp * zsigm**2._dp) + 1.246_dp * zknud * exp(2._dp * zsigm**2._dp))

      ! settling velocity [m/s] (equation 16 in [1] and  3.69 in [2])
      zvsed(jl) = (grav * (daer_p(jl,klev,jb) - prho(jl)) * zdwet(jl)**2._dp) / (18._dp * pdyvi(jl))
      zvsed(jl) = zvsed(jl) * (exp(2._dp * zsigm**2._dp) + 1.246_dp * zknud * exp(0.5_dp * zsigm**2._dp))
 
    END DO
    !$ACC END PARALLEL LOOP

    IF (lcheck_ddep) THEN    
      !$ACC UPDATE HOST(zdiff,zvsed) ASYNC(1) 
      !$ACC WAIT(1)
      CALL ham_lite_check_wcc( jb, sizeclass(jclass)%shortname, 'mode' ) 
      CALL ham_lite_check_r1d( jb, jcs, jce, kproma, zdiff, 'ham_lite_drydep', 'zdiff' )
      CALL ham_lite_check_r1d( jb, jcs, jce, kproma, zvsed, 'ham_lite_drydep', 'zvsed' )  
    END IF 

    !--- calculate surface resistance     
    DO js = 1,nsurf

      !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
      !$ACC LOOP GANG VECTOR &
      !$ACC   PRIVATE(zt_cls,za_fac,zb_fac,zf_cap,zs_mic,zs_mac) &
      !$ACC   PRIVATE(zebd_tile,zeim_tile,ztemp,ltest)
      DO jl = jcs,jce

        ! Skip tiles with zero coverage: no contribution and avoids divides
        ! on uninitialized fields (root cause of FPE crash in OpenIFS).
        IF (pfrc_tile(jl,js) <= zeps) THEN
          zrsurf_tile(jl,js) = 0._dp
          CYCLE
        END IF

        ! collection efficiency due to Brownian diffusion (equation 5 and 10)
        IF (js == idx_wtr) THEN 
          zt_cls = ptsfc_tile(jl,idx_wtr) - 273.15 
          za_fac = 8.46E-05 + 1.63E-6 * zt_cls - 3.35E-8 * zt_cls**2
          zb_fac = 3.354 - 0.062 * zt_cls 
          zf_cap = za_fac * (zb_fac + psfcWind_tile(jl,idx_wtr))**2
          IF (zdiff(jl) > zeps) THEN
            ztemp = (1._dp / 3._dp) * (pkivi(jl) / zdiff(jl))**(-2._dp/3._dp)
          ELSE
            ztemp = 0._dp
          END IF
          zebd_tile = (1._dp - zf_cap) * ztemp +  &
            zf_cap * zustar_tile(jl,idx_wtr) / MAX(1.0_dp, psfcWind_tile(jl,idx_wtr)) !TODO: div by zero
        ELSE 
          IF (zdiff(jl) > zeps) THEN
            zebd_tile = (1._dp / 3._dp) * (pkivi(jl) / zdiff(jl))**(-2._dp/3._dp)
          ELSE
            zebd_tile = 0._dp
          END IF
        END IF 
 
        ! collection efficiency due to impaction (equation 6, 7, 8, and 9) 
        IF (js == idx_veg) THEN 
          zs_mic = (zvsed(jl) * zustar_tile(jl,idx_lnd)) / (grav * za_mic)
          zs_mac = (zvsed(jl) * zustar_tile(jl,idx_lnd)) / (grav * za_mac) 
          zeim_tile = (1._dp - zf_mic) * zs_mac**2 / (1._dp + zs_mac**2) + zf_mic * zs_mic**2 / (1._dp + zs_mic**2)  
        ELSE  
          ztemp = (prho(jl) * zvsed(jl)) / (grav * pdyvi(jl)) * zustar_tile(jl,js)**2 
          zeim_tile = 10._dp**(-3._dp / MAX(1.0_dp,ztemp))  !TODO: div by zero
        END IF 
 
        ! surface resistance [s/m] (equation 4 and 12) 
        IF (js == idx_veg) THEN
          ztemp = plai(jl) * zustar_tile(jl,js) * (zebd_tile + zeim_tile) 
        ELSE 
          ztemp = zustar_tile(jl,js) * (zebd_tile + zeim_tile) 
        END IF
 
        ltest = (ztemp > zeps)  
        IF (ltest) THEN
          zrsurf_tile(jl,js) = 1._dp / ztemp
        ELSE
          zrsurf_tile(jl,js) = 0._dp
        END IF

      END DO 
      !$ACC END PARALLEL LOOP

    END DO    

    IF (lcheck_ddep) THEN  
      !$ACC UPDATE HOST(zrsurf_tile) ASYNC(1) 
      !$ACC WAIT(1)
      CALL ham_lite_check_wci( jb, idx_veg, 'idx_veg' )  
      CALL ham_lite_check_r1d( jb, jcs, jce, kproma, zrsurf_tile(:,idx_veg), 'ham_lite_drydep', 'zrsurf_veg' )
      CALL ham_lite_check_wci( jb, idx_lnd, 'idx_lnd' )  
      CALL ham_lite_check_r1d( jb, jcs, jce, kproma, zrsurf_tile(:,idx_lnd), 'ham_lite_drydep', 'zrsurf_lnd' ) 
      CALL ham_lite_check_wci( jb, idx_wtr, 'idx_wtr' )  
      CALL ham_lite_check_r1d( jb, jcs, jce, kproma, zrsurf_tile(:,idx_wtr), 'ham_lite_drydep', 'zrsurf_wtr' )
      CALL ham_lite_check_wci( jb, idx_ice, 'idx_ice' )     
      CALL ham_lite_check_r1d( jb, jcs, jce, kproma, zrsurf_tile(:,idx_ice), 'ham_lite_drydep', 'zrsurf_ice' )  
    END IF 

    !--- calculate dry deposition velocity [m/s] (equation 3)
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR 
    DO jl = jcs,jce 
      zvdep(jl) = 0._dp 
    END DO
    !$ACC END PARALLEL LOOP
 
    DO js = 1,nsurf
  
      !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
      !$ACC LOOP GANG VECTOR PRIVATE(ztemp,ltest)
      DO jl = jcs,jce

        ! Skip tiles with no coverage (root-cause guard for FPE divide-by-zero)
        IF (pfrc_tile(jl,js) <= zeps) CYCLE

        ztemp = zraero_tile(jl,js) + zrsurf_tile(jl,js) 
        ! Slinn (1982) deposition velocity v_d = v_s / (1 - exp(-v_s * R_tot)).
        ! Guard against (a) R_tot=0 and (b) v_s*R_tot underflow (-> exp=1 -> div/0).
        ! For v_s*R_tot << 1 the Taylor limit gives v_d -> 1 / R_tot.
        IF (ABS(zvsed(jl) * ztemp) <= zeps) THEN
          ztemp = 0._dp
        ELSE IF (zvsed(jl) * ztemp > 1.0e-8_dp) THEN
          ztemp = zvsed(jl) / (1._dp - exp(-1._dp * zvsed(jl) * ztemp))
        ELSE
          ztemp = 1._dp / ztemp
        END IF

        IF (js == idx_veg) THEN
          zvdep(jl) = zvdep(jl) + pfrc_tile(jl,idx_lnd) * pvgf(jl) * ztemp
        ELSE IF (js == idx_lnd) THEN
          zvdep(jl) = zvdep(jl) + pfrc_tile(jl,idx_lnd) * (1._dp - pvgf(jl)) * ztemp
        ELSE 
          zvdep(jl) = zvdep(jl) + pfrc_tile(jl,js) * ztemp 
        END IF
 
      END DO
     !$ACC END PARALLEL LOOP

    END DO    

    !--- output deposition velocity 
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR PRIVATE(ltest)
    DO jl = jcs,jce  
      ltest = (zvdep(jl) > 0._dp) 
      pvdep(jl) = MERGE(zvdep(jl), 0._dp, ltest)  
    END DO
    !$ACC END PARALLEL LOOP

    !--- limit deposition velocity to grid velocity (dz / dt)
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR  
    DO jl = jcs,jce
      pvdep(jl) = MIN(pvdep(jl), pdz(jl) / pdtime)  
      pvdep(jl) = MAX(0.0_dp, pvdep(jl))
    END DO
    !$ACC END PARALLEL LOOP
      
    !$ACC END DATA 

  END SUBROUTINE ham_lite_drydep
 
END MODULE mo_ham_lite_drydep