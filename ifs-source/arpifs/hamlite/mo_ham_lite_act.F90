! 
! based on mo_ham_activ.f90  
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

MODULE mo_ham_lite_act

  USE mo_kind_lite,                  ONLY: dp   
  USE mo_math_constants_lite,        ONLY: pi 
  USE mo_physical_constants_lite,    ONLY: amw, amd, rhoh2o, argas, cpd, grav, alv, tmelt 
  USE mo_subm_trac_lite,             ONLY: ntrac       
  USE mo_hammoz_lite_act,            ONLY: activ_field   
  !USE mo_ham_lite_streams,      ONLY: ham_field 
  USE mo_ham_lite_main,         ONLY: ncdnc, nclass, sizeclass, silog_mode   
  USE mo_ham_lite_misc,         ONLY: lcheck_wdep, ham_lite_tail, &!ham_lite_satpre, ham_lite_tail,  &
                                      ham_lite_check_r1d, ham_lite_check_r2d,       &
                                      ham_lite_check_wcc, ham_lite_stat_cld  
  USE mo_ham_lite_wetdep,       ONLY: tfrz_hom, qcld_min

  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC  zeps, zwam 
  
  PUBLIC  ham_lite_act_para,    &
          ham_lite_act_nact,    &
          ham_lite_act_cdnc!,    &
!          ham_lite_act_diag 

  !--------------------
  ! module parameters
  !--------------------

  REAL(dp), PARAMETER ::    &
    zeps = EPSILON(1._dp),  &
    zwam = 0.0_dp,          & ! minimum updraft velocity [m/s] 
    zamw = amw*1.E-3_dp,    & ! molecular weight of water [kg mol-1] 
    zamd = amd*1.E-3_dp,    & ! molecular weight of dry air [kg mol-1]
    zsten = 75.0E-3_dp        ! surface tension of water [J m-2]
     
  CONTAINS

!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_act_para( jb, jcs, jce, jks, klev, kproma,  &
                                ptm1, prdry, pbhyg, pacur         )
  
    ! [1] Abdul-Razzak et al., JGR, 103, 6123-6131, 1998 
    ! [2] Abdul-Razzak and Ghan, JGR, 105, 6837-6844, 2000  
 
    INTEGER, INTENT(IN) :: jb, jcs, jce, jks, klev, kproma
    REAL(dp), INTENT(IN) :: ptm1(kproma,klev) ! temperature
    REAL(dp), INTENT(INOUT) ::  prdry(nclass),  &   ! dry radius
                                pbhyg(nclass),  &   ! hygroscopicity parameter B of the Koehler equation
                                pacur(kproma,klev)  ! curvature parameter A of the Koehler equation

    INTEGER :: jclass, jk, jl 
  
    ! dry radius
    DO jclass = 1,nclass 
      prdry(jclass) = sizeclass(jclass)%lite_radi 
    END DO

    ! equation (4) in reference [2]
    DO jclass = 1,nclass    
      pbhyg(jclass) = sizeclass(jclass)%lite_kappa  
    END DO 
 
    ! equation (5) in reference [1] 
    DO jk = jks,klev
      DO jl = jcs,jce 
        pacur(jl,jk) = 2._dp*zsten*zamw / (rhoh2o*argas) / ptm1(jl,jk)
      END DO 
    END DO
   
    ! print checks 
    IF (lcheck_wdep) THEN  
      CALL ham_lite_check_r2d( jb, jcs, jce, jks, klev, kproma, pacur, 'ham_lite_act_para', 'pacur' ) 
    END IF

  END SUBROUTINE ham_lite_act_para

!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_act_nact( jg, jb, jcs, jce, jks, klev, kproma,      &
                                prho, pwa, ptm1, papm1, pqm1,             &
                                pxtm1, prdry, pbhyg, pacur,               &
                                pnact, pfrac, pcsup, pcrad, psmax, pnsum, psew)

    ! [1] Abdul-Razzak et al., JGR, 103, 6123-6131, 1998 
    ! [2] Abdul-Razzak and Ghan, JGR, 105, 6837-6844, 2000 
    ! [3] Pruppbacher and Klett, Kluewer Ac. Pub., 1997 
    ! 
    ! equation numbers from reference [2] unless otherwise quoted 
 
    INTEGER, INTENT(IN) :: jg, jb, jcs, jce, jks, klev, kproma
    REAL(dp), INTENT(IN)  :: prho(kproma,klev),        & ! air density
                             pwa(kproma,klev),         & ! updraft velocity [m/s] 
                             ptm1(kproma,klev),        & ! temperature
                             papm1(kproma,klev),       & ! pressure 
                             pqm1(kproma,klev),        & ! specific humidity 
                             pxtm1(kproma,klev,ntrac), & ! tracers number mixing ratio [#/kg]
                             psew(kproma,klev)           ! saturation water vapor pressure [Pa]
    REAL(dp), INTENT(IN)  :: prdry(nclass),     & ! dry radius for each mode [m]
                             pbhyg(nclass),     & ! hygroscopicity parameter B of the Koehler equation  
                             pacur(kproma,klev)   ! curvature parameter A of the Koehler equation
    REAL(dp), INTENT(INOUT) ::  pnact(kproma,klev,nclass),  & ! number of activated particles per mode [m-3]
                                pfrac(kproma,klev,nclass),  & ! fraction of activated particles per mode
                                pcsup(kproma,klev,nclass),  & ! critical supersaturation [% 0-1]
                                pcrad(kproma,klev,nclass),  & ! critical radius of activation per mode [m]
                                psmax(kproma,klev),         & ! maximum supersaturation [% 0-1]
                                pnsum(kproma,klev)            ! number of activated particles [m-3]
                                

    INTEGER :: jclass, jk, jl, jt  
    REAL(dp) :: zdif, zxv, zk, zka, zkv, zeta, zxi 
    REAL(dp) :: zf(nclass), zg(nclass),     & 
                zsum(kproma,klev),          &                      
                zsat(kproma,klev),          & ! saturation water vapor pressure [Pa] 
                znum(kproma,klev,nclass),   & ! aerosol number concentration for each mode [m-3] 
                zalpha(kproma,klev),        &
                zgamma(kproma,klev),        &
                zgrowth(kproma,klev) 
    LOGICAL :: ltest

    !--- initialization 
    DO jk = 1,klev
      DO jl = jcs,jce
        pnsum(jl,jk)    = 0._dp  
        psmax(jl,jk)    = 0._dp
        zsum(jl,jk)     = 0._dp
        zsat(jl,jk)     = 0._dp
        zalpha(jl,jk)   = 0._dp
        zgamma(jl,jk)   = 0._dp
        zgrowth(jl,jk)  = 0._dp
      END DO 
    END DO 

    DO jclass = 1,nclass
      DO jk = 1,klev
        DO jl = jcs,jce 
          pnact(jl,jk,jclass) = 0._dp
          pfrac(jl,jk,jclass) = 0._dp
          pcsup(jl,jk,jclass) = 0._dp  
          pcrad(jl,jk,jclass) = 1._dp ! initialized with 1 m, only changed if activation occurs  
        END DO 
      END DO 
    END DO 

    !CALL ham_lite_satpre( jb, jcs, jce, jks, klev, kproma, ptm1, zsat )
    zsat(:,:) = psew(:,:)

    ! number per unit volume for each mode
    DO jclass = 1,nclass
      jt = sizeclass(jclass)%idt_no
  
      DO jk = jks,klev
        DO jl = jcs,jce 
          ltest = (pxtm1(jl,jk,jt) > 0.0_dp)
          znum(jl,jk,jclass) = MERGE(pxtm1(jl,jk,jt) * prho(jl,jk), 0.0_dp, ltest)
        END DO 
      END DO  

      ! equation (7) 
      zf(jclass) = 0.5_dp*EXP(2.5_dp*silog_mode(jclass)**2._dp)

      ! equation (8) 
      zg(jclass) = 1._dp + 0.25_dp*silog_mode(jclass)
    END DO
  
    !--- growth coefficient 
    DO jk = jks,klev
      DO jl = jcs,jce
  
        ltest = ((pwa(jl,jk) > zeps) .AND. (pqm1(jl,jk) > zeps) .AND. (ptm1(jl,jk)  > tfrz_hom))
        IF (ltest) THEN  

          ! equation (11) in [1]  
          zalpha(jl,jk) = (grav*zamw*alv) / (cpd*argas*ptm1(jl,jk)**2) - (grav*zamd) / (argas*ptm1(jl,jk)) 

          ! equation (12) in [1]  

          zgamma(jl,jk) = (argas*ptm1(jl,jk)) / (zsat(jl,jk)*zamw) + (zamw*alv**2) / (cpd*papm1(jl,jk)*zamd*ptm1(jl,jk))

          ! diffusivity of water vapour in air, equation (13.3) in [3], [m2/s]  
          zdif = 0.211_dp * (ptm1(jl,jk)/tmelt)**1.94_dp * (101325._dp/papm1(jl,jk)) * 1.E-4_dp
 
          ! thermal conductivity, equation (13.18) in [3], [cal/cm/s/K] 
          zka = (5.69_dp + 0.017_dp*(ptm1(jl,jk) - 273.15_dp))*1.E-5_dp
          zkv = (3.78_dp + 0.020_dp*(ptm1(jl,jk) - 273.15_dp))*1.E-5_dp

         ! mole fraction of water  
          zxv = pqm1(jl,jk) * (zamd/zamw)
    
          ! thermal conductivity [J/m/s/K] 
          zk = zka * (1._dp - (1.17_dp - 1.02_dp*zkv/zka)*zxv) * 4.1868_dp*1.E+2_dp

          ! equation (16) in [1] 
          zgrowth(jl,jk) = 1._dp / ( (rhoh2o*argas*ptm1(jl,jk)) / (zsat(jl,jk)*zdif*zamw) + &
            (alv*rhoh2o) / (zk*ptm1(jl,jk)) * ((alv*zamw) / (ptm1(jl,jk)*argas) -1._dp) )

        END IF 

      END DO  
    END DO  

    !--- maximum supersaturation 
    DO jclass = 1,nclass
      IF (sizeclass(jclass)%lacti) THEN

        DO jk = jks,klev
          DO jl = jcs,jce

            ltest = ((pwa(jl,jk) > zeps) .AND. (pqm1(jl,jk) > zeps) .AND. (ptm1(jl,jk)  > tfrz_hom))
            IF (ltest .AND. (znum(jl,jk,jclass) > zeps)) THEN

              ! equation (9)  
              pcsup(jl,jk,jclass) = 2._dp/SQRT(pbhyg(jclass)) * (pacur(jl,jk)/(3._dp*prdry(jclass)))**1.5_dp

              ! equation (10)  
              zxi = 2._dp * pacur(jl,jk)/3._dp * SQRT(zalpha(jl,jk)*pwa(jl,jk)/zgrowth(jl,jk))

              ! equation (11)  
              zeta = ((zalpha(jl,jk)*pwa(jl,jk)/zgrowth(jl,jk))**1.5_dp) / &
                (2._dp*pi*rhoh2o*zgamma(jl,jk)*znum(jl,jk,jclass))
                      
              ! equation (6)   
              zsum(jl,jk) = zsum(jl,jk) + ( 1._dp/pcsup(jl,jk,jclass)**2 * ( zf(jclass)*(zxi/zeta)**1.5_dp + &
                zg(jclass) * ( pcsup(jl,jk,jclass)**2._dp / (zeta + 3._dp*zxi) )**0.75_dp ) ) 

            END IF
          END DO 
        END DO  

      END IF
    END DO  
            
    DO jk = jks,klev
      DO jl = jcs,jce

        ltest = ((pwa(jl,jk) > zeps) .AND. (pqm1(jl,jk) > zeps) .AND. (ptm1(jl,jk)  > tfrz_hom))
        IF (ltest .AND. (zsum(jl,jk) > zeps)) THEN  

            psmax(jl,jk) = 1._dp / SQRT(zsum(jl,jk))
            
        END IF
      END DO  
    END DO  

    !--- number of activated particles
    DO jclass = 1,nclass
      IF (sizeclass(jclass)%lacti) THEN  

        DO jk = jks,klev
          DO jl = jcs,jce
            IF ( psmax(jl,jk) > zeps .AND. pcsup(jl,jk,jclass) > zeps ) THEN
                  
              ! equation (12) 
              pcrad(jl,jk,jclass) = prdry(jclass) * ( pcsup(jl,jk,jclass) / psmax(jl,jk) )**(2._dp/3._dp)

            END IF
          END DO 
        END DO
 
        ! fraction of each mode larger than critical radius
        CALL ham_lite_tail( jcs, jce, jks, jclass, kproma, klev,                &
                            prdry(jclass), pcrad(:,:,jclass), pfrac(:,:,jclass) )

      END IF ! lactivation 
    END DO ! jclass 

    !--- diagnostics 
    DO jclass = 1,nclass
      IF (sizeclass(jclass)%lacti) THEN  
  
        DO jk = jks,klev
          DO jl = jcs,jce 
            pnact(jl,jk,jclass) = pfrac(jl,jk,jclass) * znum(jl,jk,jclass)
            pnsum(jl,jk)        = pnsum(jl,jk) + pnact(jl,jk,jclass)
          END DO 
        END DO

      END IF
    END DO

    ! print checks 
    IF (lcheck_wdep) THEN 
      CALL ham_lite_check_r2d( jb, jcs, jce, jks, klev, kproma, psmax, 'ham_lite_act_nact', 'psmax' ) 
      DO jclass = 1,nclass 
        CALL ham_lite_check_wcc( jb, sizeclass(jclass)%shortname, 'mode' ) 
        CALL ham_lite_check_r2d( jb, jcs, jce, jks, klev, kproma, pfrac(:,:,jclass), 'ham_lite_act_nact', 'pfrac' )
        CALL ham_lite_check_r2d( jb, jcs, jce, jks, klev, kproma, pcrad(:,:,jclass), 'ham_lite_act_nact', 'pcrad' ) 
      END DO 
    END IF 
 
  END SUBROUTINE ham_lite_act_nact

!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_act_cdnc( jg, jb, jcs, jce, jks, klev, kproma,  &
                                pcm1, pim1, pnsum, pbase, pcdnc       )

    INTEGER, INTENT(IN)  :: jg, jb, jcs, jce, jks, klev, kproma 
    REAL(dp), INTENT(IN) :: pcm1  (kproma,klev),  & ! cloud water   
                            pim1  (kproma,klev),  & ! cloud ice
                            pnsum (kproma,klev)     ! number of activated particles [1/m3]
    INTEGER, INTENT(INOUT)  :: pbase  (kproma,klev) ! index of cloud base
    REAL(dp), INTENT(INOUT) :: pcdnc  (kproma,klev) ! cloud droplet number concentration [1/m3] 

    INTEGER :: jk, jkp1, jkba, jl  
    LOGICAL :: ltest

    DO jk = 1,klev
      DO jl = jcs,jce
        pbase(jl,jk) = klev 
        pcdnc(jl,jk) = ncdnc
      END DO 
    END DO

    ! get index of cloud base
    DO jl = jcs,jce  
      DO jk = klev,jks,-1 
        ltest = (jk == klev) 
        jkp1 = MERGE(jk, jk + 1, ltest)
        IF ((pcm1(jl,jk) + pim1(jl,jk)) > qcld_min) THEN  
          ltest = ((pcm1(jl,jkp1) + pim1(jl,jkp1)) > qcld_min)
          pbase(jl,jk) = MERGE(pbase(jl,jkp1), jk, ltest)
        END IF
      END DO  
    END DO 

    ! update cdnc at cloud base and above 
    DO jk = jks,klev 
      DO jl = jcs,jce   
        jkba = pbase(jl,jk)  
        IF ((jkba >= jks) .AND. (jkba <= klev)) THEN   
          ltest = (pnsum(jl,jkba) > 0.0_dp)
          pcdnc(jl,jk) = MERGE(ncdnc + pnsum(jl,jkba), ncdnc, ltest)  
        END IF  
      END DO 
    END DO 

  END SUBROUTINE ham_lite_act_cdnc

!---------------------------------------------------------------------------------------------------

  ! SUBROUTINE ham_lite_act_diag( jg, jb, jcs, jce, jks, klev, kproma,      &
  !                               pdz, pwa, pcm1, pim1,                     &
  !                               pbhyg, pacur, pnact, pfrac, pcsup, pcrad, &  
  !                               psmax, pnsum, pbase, pcdnc                )

  !   INTEGER, INTENT(IN)  :: jg, jb, jcs, jce, jks, klev, kproma, &
  !                           pbase(kproma,klev) ! index of cloud base
  !   REAL(dp), INTENT(IN) :: pdz   (kproma,klev),        & ! geometric height thickness of layer [m]  
  !                           pwa   (kproma,klev),        & ! vertical velocity [m/s] 
  !                           pcm1  (kproma,klev),        & ! cloud water  
  !                           pim1  (kproma,klev)           ! cloud ice 
  !   REAL(dp), INTENT(IN) :: pbhyg (nclass),             & ! hygroscopicity parameter B of the Koehler equation
  !                           pacur (kproma,klev),        & ! curvature parameter A of the Koehler equation
  !                           pnact (kproma,klev,nclass), & ! number of activated particles per mode [1/m3]
  !                           pfrac (kproma,klev,nclass), & ! fraction of activated particles per mode 
  !                           pcsup (kproma,klev,nclass), & ! critical supersaturation [% 0-1]
  !                           pcrad (kproma,klev,nclass), & ! critical radius of activation per mode [m]
  !                           psmax (kproma,klev),        & ! maximum supersaturation [% 0-1]
  !                           pnsum (kproma,klev),        & ! number of activated particles [1/m3] 
  !                           pcdnc (kproma,klev)           ! cloud droplet number concentration [1/m3] 

  !   INTEGER :: jclass, jk, jl 
  !   REAL(dp) :: zmin, zmax, zmean,  &
  !               zback, zbase(kproma,klev), zwa(kproma,klev)
  !   LOGICAL :: ltest

  !   DO jk = 1,klev
  !     DO jl = jcs,jce
  !       activ_field(jg)%swat(jl,jk,jb) = psmax(jl,jk)
  !       activ_field(jg)%nsum(jl,jk,jb) = pnsum(jl,jk)
  !       activ_field(jg)%base(jl,jk,jb) = REAL(pbase(jl,jk))
  !       activ_field(jg)%cdnc(jl,jk,jb) = pcdnc(jl,jk)  
  !     END DO 
  !   END DO

  !   zback = REAL(klev)

  !   DO jk = 1,klev
  !     DO jl = jcs,jce
  !       zbase(jl,jk) = REAL(pbase(jl,jk))
  !     END DO 
  !   END DO

  !   DO jk = 1,klev
  !     DO jl = jcs,jce
  !       IF (pwa(jl,jk) > zeps) THEN
  !         zwa(jl,jk) = pwa(jl,jk)
  !       ELSE 
  !         zwa(jl,jk) = 0._dp
  !       END IF
  !     END DO 
  !   END DO

  !   DO jl = jcs,jce  

  !     CALL ham_lite_stat_cld( jks, klev, qcld_min, zback,         &
  !                             pdz(jl,:), pcm1(jl,:), pim1(jl,:),  &
  !                             zbase(jl,:), zmin, zmax, zmean      ) 
  !     activ_field(jg)%base_2d(jl,jb) = zmean

  !     CALL ham_lite_stat_cld( jks, klev, qcld_min, ncdnc,         &
  !                             pdz(jl,:), pcm1(jl,:), pim1(jl,:),  &
  !                             pcdnc(jl,:), zmin, zmax, zmean      ) 
  !     activ_field(jg)%cdnc_2d(jl,jb) = zmean 
  !     activ_field(jg)%cdnc_mx(jl,jb) = zmax 

  !     CALL ham_lite_stat_cld( jks, klev, qcld_min, 0._dp,         &
  !                             pdz(jl,:), pcm1(jl,:), pim1(jl,:),  &
  !                             zwa(jl,:), zmin, zmax, zmean        ) 
  !     activ_field(jg)%wa_2d(jl,jb) = zmean 
  !     activ_field(jg)%wa_mx(jl,jb) = zmax 

  !   END DO

  !   DO jclass = 1,nclass 
  !     IF (sizeclass(jclass)%lacti) THEN

  !       DO jk = 1,klev
  !         DO jl = jcs,jce 
  !           ham_field(jg)%sc(jclass)%p_3d(jl,jk,jb) = pcsup(jl,jk,jclass)
  !         END DO 
  !       END DO  

  !       DO jk = 1,klev
  !         DO jl = jcs,jce
  !           ltest = ( pcrad(jl,jk,jclass) /= 1._dp )
  !           ham_field(jg)%rc(jclass)%p_3d(jl,jk,jb)   = MERGE(pcrad(jl,jk,jclass), 0.0_dp, ltest)
  !           ham_field(jg)%nact(jclass)%p_3d(jl,jk,jb) = pnact(jl,jk,jclass)
  !           ham_field(jg)%frac(jclass)%p_3d(jl,jk,jb) = pfrac(jl,jk,jclass)
  !         END DO  
  !       END DO
        
  !       DO jl = jcs,jce

  !         CALL ham_lite_stat_cld( jks, klev, qcld_min, 0.0_dp,          &
  !                                 pdz(jl,:), pcm1(jl,:), pim1(jl,:),    & 
  !                                 pnact(jl,:,jclass), zmin, zmax, zmean ) 
  !         ham_field(jg)%nact_2d(jclass)%p_2d(jl,jb) = zmean

  !         CALL ham_lite_stat_cld( jks, klev, qcld_min, 0.0_dp,          &
  !                                 pdz(jl,:), pcm1(jl,:), pim1(jl,:),    &
  !                                 pfrac(jl,:,jclass), zmin, zmax, zmean )  
  !         ham_field(jg)%frac_2d(jclass)%p_2d(jl,jb) = zmean

  !       END DO

  !     END IF 
  !   END DO 
 
  !   ! print checks 
  !   IF (lcheck_wdep) THEN 
  !     CALL ham_lite_check_r1d( jb, jcs, jce, kproma, &
  !       activ_field(jg)%base_2d(:,jb), 'ham_lite_act_diag', 'base_2d' ) 
  !     CALL ham_lite_check_r1d( jb, jcs, jce, kproma, &
  !       activ_field(jg)%cdnc_2d(:,jb), 'ham_lite_act_diag', 'cdnc_2d' ) 
  !     CALL ham_lite_check_r1d( jb, jcs, jce, kproma, &
  !       activ_field(jg)%cdnc_mx(:,jb), 'ham_lite_act_diag', 'cdnc_mx' ) 
  !     CALL ham_lite_check_r1d( jb, jcs, jce, kproma, &
  !       activ_field(jg)%wa_2d(:,jb), 'ham_lite_act_diag', 'wa_2d' ) 
  !     CALL ham_lite_check_r1d( jb, jcs, jce, kproma, &
  !       activ_field(jg)%wa_mx(:,jb), 'ham_lite_act_diag', 'wa_mx' ) 
  !     DO jclass = 1,nclass 
  !       CALL ham_lite_check_wcc( jb, sizeclass(jclass)%shortname, 'mode' ) 
  !       CALL ham_lite_check_r1d( jb, jcs, jce, kproma, &
  !         ham_field(jg)%nact_2d(jclass)%p_2d(:,jb), 'ham_lite_act_diag', 'nact_2d' )
  !       CALL ham_lite_check_r1d( jb, jcs, jce, kproma, &
  !         ham_field(jg)%frac_2d(jclass)%p_2d(:,jb), 'ham_lite_act_diag', 'frac_2d' ) 
  !     END DO 
  !   END IF 

  ! END SUBROUTINE ham_lite_act_diag
 
END MODULE mo_ham_lite_act
