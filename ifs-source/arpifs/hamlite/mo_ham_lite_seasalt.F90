! 
! based on mo_ham_seasalt.f90 
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

MODULE mo_ham_lite_seasalt

  USE mo_kind_lite,  ONLY: dp

  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: start_emi_seasalt,  &  
            emi_seasalt_long,   &
            emi_seasalt_gong  

  !--------------------
  ! module parameters
  !--------------------

  INTEGER,  PARAMETER :: nbin = 300          ! number of bins

  REAL(dp), PARAMETER :: wexp = 3.41_dp      ! exponent of wind speed |u| (|u|**wexp) 

  REAL(dp), PARAMETER :: ddry0 = 0.10E-06_dp ! lower limit for dry diameter [m] 
  REAL(dp), PARAMETER :: ddry1 = 1.00E-05_dp ! upper limit for dry diameter [m]   

  REAL(dp), PARAMETER :: dmod0 = 0.10E-06_dp 
  REAL(dp), PARAMETER :: dmod1 = 1.00E-05_dp  

  REAL(dp), PARAMETER :: ddry_long = 0.551E-06_dp ! dry diameter limit [m]
  REAL(dp), PARAMETER :: wexp_long = 3.74_dp  
  REAL(dp), PARAMETER :: dmtb_gong = 0.221E-06_dp ! dry diameter limit [m]  
  REAL(dp), PARAMETER :: wexp_gong = 3.41_dp
  !--------------------
  ! module variables
  !--------------------

  REAL(dp) :: dsdry(nbin), dswet(nbin)     

  CONTAINS

!---------------------------------------------------------------------------------------------------

  SUBROUTINE start_emi_seasalt

    USE mo_kind_lite,  ONLY: dp   

    IMPLICIT NONE
   
    INTEGER :: jb
    REAL(dp) :: zdx, zdd
 
    ! construct bins

    zdx = (LOG(ddry1) - LOG(ddry0)) / REAL(nbin,dp)

    zdd = 0._dp

    DO jb = 1,nbin

      ! dry diameter [m]
      dsdry(jb) = EXP(LOG(ddry0) + zdd) 
 
      ! wet diameter [um] at RH = 80% 
      dswet(jb) = 1.814_dp * dsdry(jb) * 1.E+06_dp 

      zdd = zdd + zdx
      
    END DO

  END SUBROUTINE start_emi_seasalt

!---------------------------------------------------------------------------------------------------

  SUBROUTINE emi_seasalt_long(  jcs, jce, kproma,                     &
                                plsmask, palake, pseaice, psfcWind,   &
                                ptsfc_wtr, pmassf, pnumbf             )

    !  
    ! Description:
    ! ------------
    ! calculate the emitted sea salt flux from the 10m wind speed following Long et al. 2011
    ! 
    !   based on laboratory data from Keene et al. 2007 
    !   size: 0.03um < d80 < 20um 
    !   SST correction according to Sofiev et al. 2011  
    !   
    ! Authors:
    ! ------------ 
    ! I. Tegen, S. Barthel, 2016 (TROPOS)
    !   
    ! References:
    ! ------------
    ! Long M.S., Keene W.D., Kieber D.J., Erickson D.J., Maring H., 
    ! A sea-state based source function for size- and composition-resolved marine aerosol
    ! production, Atmospheric Chemistry and Physics, 2011
    !
    ! Sofiev M., Soares J., Prank M., deLeeuw G., Kukkonen J., 
    ! A regional-to-global model of emission and transport of sea salt particles
    ! in the atmosphere, Journal of Geophysical Research, 2011
    ! 
 
    USE mo_kind_lite,                ONLY: dp
    USE mo_math_constants_lite,      ONLY: pi
    USE mo_subm_spec_lite,           ONLY: speclist
    USE mo_ham_lite_spec,            ONLY: id_ss 

    IMPLICIT NONE

    INTEGER, INTENT(IN) ::  jcs, jce, kproma                  
    REAL(dp), INTENT(IN)  ::  plsmask(kproma),    & ! land-sea mask (1. = land, 0. = sea/lakes)   
                              palake(kproma),     & ! lake fraction            
                              pseaice(kproma),    & ! sea ice fraction
                              psfcWind(kproma),   & ! 10m wind speed
                              ptsfc_wtr(kproma)     ! water temperature
    REAL(dp),INTENT(OUT)  ::  pmassf(kproma),     & ! mass flux of sea salt particles 
                              pnumbf(kproma)        ! number flux of sea salt particles 
      
    INTEGER   ::  jb, jl
    REAL(dp)  ::  p0, p1, p2, p11, p12, p13, p14, p21, p22, p23, p24
    REAL(dp)  ::  logdp, dstmp
    REAL(dp)  ::  zav               ! particle volume 
    REAL(dp)  ::  zseafrac(kproma)  ! fraction of sea water not covered with sea ice [0. - 1.]
    REAL(dp)  ::  zseatemp(kproma)  ! sea surface temperature [271.15 K - 298.15 K] 
    REAL(dp)  ::  nflux(kproma,nbin)  ! number flux for each bin [#/m2/s] 
    REAL(dp)  ::  SST_corr, SST_corr_1, SST_corr_2 
    REAL(dp)  ::  SST_mask1(kproma), SST_mask2(kproma), SST_mask3(kproma), SST_corr_all(kproma)  

    !--- initialize 
    DO jb = 1,nbin
      DO jl = jcs,jce
        nflux(jl,jb) = 0._dp
      END DO
    END DO 

    DO jl = jcs,jce
      pmassf(jl) = 0._dp  
      pnumbf(jl) = 0._dp  
    END DO 

    p0  = 2.E-8_dp
    p11 = 1.46E0_dp
    p12 = 1.33E0_dp
    p13 = -1.82E0_dp
    p14 = 8.83E0_dp
    p21 = -1.53E0_dp
    p22 = -8.1E-2_dp
    p23 = -4.26E-1_dp
    p24 = 8.84E0_dp
  
    !--- calculate fraction of sea water not covered with sea ice
    DO jl = jcs,jce
      zseafrac(jl) = (1._dp - plsmask(jl) - palake(jl)) * (1._dp - pseaice(jl))
      zseafrac(jl) = MAX(0._dp, MIN(zseafrac(jl), 1._dp))
      zseafrac(jl) = MERGE(0._dp, zseafrac(jl), (plsmask(jl) > 0.5_dp))
    END DO

    !--- limit sea surface temperature
    DO jl = jcs,jce
      zseatemp(jl) = MAX(271.15_dp, ptsfc_wtr(jl))
      zseatemp(jl) = MIN(298.15_dp, zseatemp(jl))
    END DO

    !--- calculate number flux (Long et al. 2011) 
    DO jb = 2,nbin
   
      logdp = LOG10(dswet(jb)) - LOG10(dswet(jb-1)) 

      IF ((dsdry(jb) .GT. ddry0) .AND. (dsdry(jb) .LE. ddry_long)) THEN
         
        p1 = p11*(LOG10(dswet(jb)))**3 + p12*(LOG10(dswet(jb)))**2 + p13*(LOG10(dswet(jb))) + p14 
        DO jl = jcs,jce
          nflux(jl,jb) = 10._dp**p1 * (p0*psfcWind(jl)**wexp_long) * logdp
        END DO 

      ELSE IF ((dsdry(jb) .GT. ddry_long) .AND. (dsdry(jb) .LE. ddry1)) THEN
           
        p2 = p21*(LOG10(dswet(jb)))**3 + p22*(LOG10(dswet(jb)))**2 + p23*(LOG10(dswet(jb))) + p24 
        DO jl = jcs,jce
          nflux(jl,jb) = 10._dp**p2 * (p0*psfcWind(jl)**wexp_long) * logdp
        END DO 

      END IF
      
    END DO

    !--- correct with sea surface temperature (Sofiev et al. 2011) 
    DO jb = 2,nbin

      dstmp = dsdry(jb)*1.E06_dp ! convert m to um

      ! SST_corr_1 = 0.092_dp*dstmp**(-0.96_dp)   ! valid for Tw = 271.15K ; -2°C ; Tbase = 25°C
      ! SST_corr_2 = 0.15_dp*dstmp**(-0.88_dp)    ! valid for Tw = 278.15K ; 5°C 
      SST_corr_1 = 0.13_dp*dstmp**(-0.78_dp)    ! valid for Tw = 271.15K ; -2°C ; Tbase = 20°C
      SST_corr_2 = 0.22_dp*dstmp**(-0.70_dp)    ! valid for Tw = 278.15K ; 5°C 

      DO jl = jcs,jce
        SST_corr = (SST_corr_1*(278.15_dp-zseatemp(jl)) + SST_corr_2*(zseatemp(jl)-271.15_dp)) / 7._dp
        SST_mask1(jl) = MERGE(SST_corr, 0._dp, (zseatemp(jl) .LE. 278.15_dp))
      END DO

      ! SST_corr_1 = 0.15_dp*dstmp**(-0.88_dp)    ! valid for Tw = 278.15K ; 5°C ; Tbase = 25°C
      ! SST_corr_2 = 0.48_dp*dstmp**(-0.36_dp)    ! valid for Tw = 288.15K ; 15°C 
      SST_corr_1 = 0.22_dp*dstmp**(-0.70_dp)    ! valid for Tw=278.15K ; 5°C ; Tbase = 20°C
      SST_corr_2 = 0.70_dp*dstmp**(-0.18_dp)    ! valid for Tw=288.15K ; 15°C

      DO jl = jcs,jce
        SST_corr = (SST_corr_1*(288.15_dp-zseatemp(jl)) + SST_corr_2*(zseatemp(jl)-278.15_dp)) / 1.E+01_dp
        SST_mask2(jl) = MERGE(SST_corr, 0._dp, (zseatemp(jl) .GT. 278.15_dp .AND. zseatemp(jl) .LE. 288.15_dp))
      END DO

      ! SST_corr_1 = 0.48_dp*dstmp**(-0.36_dp)    ! valid for Tw = 288.15K ; 15°C ; Tbase = 25°C
      ! SST_corr_2 = 1._dp                        ! valid for Tw = 298.15K ; 25°C 
      SST_corr_1 = 0.70_dp*dstmp**(-0.18_dp)    ! valid for Tw=288.15K ; 15°C ; Tbase = 20°C
      SST_corr_2 = 1.45_dp*dstmp**(0.18_dp)     ! valid for Tw=298.15K ; 25°C

      DO jl = jcs,jce
        SST_corr = (SST_corr_1*(298.15_dp-zseatemp(jl)) + SST_corr_2*(zseatemp(jl)-288.15_dp)) / 1.E+01_dp
        SST_mask3(jl) = MERGE(SST_corr, 0._dp, (zseatemp(jl) .GT. 288.15_dp))
      END DO

      DO jl = jcs,jce
        SST_corr_all(jl) = SST_mask1(jl) + SST_mask2(jl) + SST_mask3(jl) 
        nflux(jl,jb) = nflux(jl,jb) * SST_corr_all(jl)
      END DO
    END DO

    !--- output mass flux [kg/m2/s] and number flux [#/m2/s]
    DO jb = 2,nbin

      zav = speclist(id_ss)%density* pi/6._dp *dsdry(jb)**3

      IF (((dsdry(jb) .GT. dmod0) .AND. (dsdry(jb) .LE. dmod1))) THEN
        DO jl = jcs,jce
          pnumbf(jl) = pnumbf(jl) + nflux(jl,jb)*zseafrac(jl)
          pmassf(jl) = pmassf(jl) + nflux(jl,jb)*zseafrac(jl)*zav
        END DO
      END IF
 
    END DO

  END SUBROUTINE emi_seasalt_long

!---------------------------------------------------------------------------------------------------

  SUBROUTINE emi_seasalt_gong(  jcs, jce, kproma,                     &
                                plsmask, palake, pseaice, psfcWind,   &
                                ptsfc_wtr, pmassf, pnumbf             ) 

    !  
    ! Description:
    ! ------------
    ! calculate the emitted sea salt flux from the 10m wind speed following Gong 2003 
    ! 
    !   size: 0.07um < r80 < 20um
    !   SST correction according to Sofiev et al. 2011  
    !   
    ! Authors:
    ! ------------ 
    ! K. Zhang, 2009, I. Tegen, 2016
    !   
    ! References:
    ! ------------
    ! Gong S. L., 
    ! A parameterization of sea-salt aerosol source function for sub- and super-micron particles, 
    ! Global Biogeochemical Cycles, 2003 
    !
    ! Sofiev M., Soares J., Prank M., deLeeuw G., Kukkonen J., 
    ! A regional-to-global model of emission and transport of sea salt particles
    ! in the atmosphere, Journal of Geophysical Research, 2011
    !
 
    USE mo_kind_lite,                ONLY: dp
    USE mo_math_constants_lite,      ONLY: pi
    USE mo_subm_spec_lite,           ONLY: speclist
    USE mo_ham_lite_spec,            ONLY: id_ss 

    IMPLICIT NONE
 
    INTEGER, INTENT(IN) :: jcs, jce, kproma 
    REAL(dp), INTENT(IN)  ::  plsmask(kproma),    & ! land-sea mask (1. = land, 0. = sea/lakes)   
                              palake(kproma),     & ! lake fraction            
                              pseaice(kproma),    & ! sea ice fraction
                              psfcWind(kproma),   & ! 10m wind speed
                              ptsfc_wtr(kproma)     ! water temperature
    REAL(dp),INTENT(OUT)  ::  pmassf(kproma),     & ! mass flux of sea salt particles 
                              pnumbf(kproma)        ! number flux of sea salt particles 
 
    INTEGER ::  jb, jl, m, nn
     
    REAL(dp):: dr, rw, bm, p0, p1, p2, p3
    REAL(dp)  ::  dstmp
    REAL(dp)  ::  zav                 ! particle volume 
    REAL(dp)  ::  zseafrac(kproma)    ! fraction of sea water not covered with sea ice [0. - 1.]
    REAL(dp)  ::  zseatemp(kproma)    ! sea surface temperature [271.15 K - 298.15 K] 
    REAL(dp)  ::  nflux(kproma,nbin)  ! number flux for each bin [#/m2/s]
    REAL(dp)  ::  SST_corr, SST_corr_1, SST_corr_2 
    REAL(dp)  ::  SST_mask1(kproma), SST_mask2(kproma), SST_mask3(kproma), SST_corr_all(kproma)  
 
    !--- initialize 
    DO jb = 1,nbin
      DO jl = jcs,jce
        nflux(jl,jb) = 0._dp
      END DO
    END DO 

    DO jl = jcs,jce 
      pmassf(jl) = 0._dp  
      pnumbf(jl) = 0._dp  
    END DO 

    !--- calculate fraction of sea water not covered with sea ice
    DO jl = jcs,jce
#ifdef HAMMOZ   
      zseafrac(jl) = (1._dp - plsmask(jl) - palake(jl)) * (1._dp - pseaice(jl))
#else
      zseafrac(jl) = 1._dp-plsmask(jl)-palake(jl)-pseaice(jl)
#endif
      zseafrac(jl) = MAX(0._dp, MIN(zseafrac(jl), 1._dp))
      zseafrac(jl) = MERGE(0._dp, zseafrac(jl), (plsmask(jl) > 0.5_dp))
    END DO

    !--- limit sea surface temperature
    DO jl = jcs,jce
      zseatemp(jl) = MAX(271.15_dp, ptsfc_wtr(jl))
      zseatemp(jl) = MIN(298.15_dp, zseatemp(jl))
    END DO
  
    !--- calculate number flux
    DO jb = 2,nbin

      dr = (dswet(jb) - dswet(jb-1)) / 2._dp 
      rw = dswet(jb) / 2._dp  

      IF (dsdry(jb) .GT. ddry0 .and. dsdry(jb) .LE. dmtb_gong) THEN 

        bm = (0.433_dp - LOG10(rw)) / 0.433_dp

        p0 = 4.7_dp*(1._dp+30._dp*rw)**(-0.017_dp*rw**(-1.44_dp)) 
        p1 = 1.373_dp * rw**(-p0) 
        p2 = 1._dp + 0.057_dp * rw**3.45_dp
        p3 = 10**(1.607_dp * EXP(-bm**2)) 

        DO jl = jcs,jce
          nflux(jl,jb) = p1*p2*p3*dr * psfcWind(jl)**wexp_gong
        END DO

      ELSEIF (dsdry(jb) .GT. dmtb_gong .and. dsdry(jb) .LE. ddry1) THEN 

        bm = (0.380_dp - LOG10(rw)) / 0.650_dp

        p1 = 1.373_dp * rw**(-3) 
        p2 = 1._dp + 0.057_dp * rw**1.05_dp
        p3 = 10**(1.19_dp * EXP(-bm**2))  

        DO jl = jcs,jce 
          nflux(jl,jb) = p1*p2*p3*dr * psfcWind(jl)**wexp_gong
        END DO

      END IF 

    END DO

    !--- correct with sea surface temperature (Sofiev et al. 2011) 
    DO jb = 2,nbin

      dstmp = dsdry(jb)*1.E06_dp ! convert m to um

      ! SST_corr_1 = 0.092_dp*dstmp**(-0.96_dp)   ! valid for Tw = 271.15K ; -2°C ; Tbase = 25°C
      ! SST_corr_2 = 0.15_dp*dstmp**(-0.88_dp)    ! valid for Tw = 278.15K ; 5°C 
      SST_corr_1 = 0.13_dp*dstmp**(-0.78_dp)    ! valid for Tw = 271.15K ; -2°C ; Tbase = 20°C
      SST_corr_2 = 0.22_dp*dstmp**(-0.70_dp)    ! valid for Tw = 278.15K ; 5°C 

      DO jl = jcs,jce
        SST_corr = (SST_corr_1*(278.15_dp-zseatemp(jl)) + SST_corr_2*(zseatemp(jl)-271.15_dp)) / 7._dp
        SST_mask1(jl) = MERGE(SST_corr, 0._dp, (zseatemp(jl) .LE. 278.15_dp))
      END DO

      ! SST_corr_1 = 0.15_dp*dstmp**(-0.88_dp)    ! valid for Tw = 278.15K ; 5°C ; Tbase = 25°C
      ! SST_corr_2 = 0.48_dp*dstmp**(-0.36_dp)    ! valid for Tw = 288.15K ; 15°C 
      SST_corr_1 = 0.22_dp*dstmp**(-0.70_dp)    ! valid for Tw=278.15K ; 5°C ; Tbase = 20°C
      SST_corr_2 = 0.70_dp*dstmp**(-0.18_dp)    ! valid for Tw=288.15K ; 15°C

      DO jl = jcs,jce
        SST_corr = (SST_corr_1*(288.15_dp-zseatemp(jl)) + SST_corr_2*(zseatemp(jl)-278.15_dp)) / 1.E+01_dp
        SST_mask2(jl) = MERGE(SST_corr, 0._dp, (zseatemp(jl) .GT. 278.15_dp .AND. zseatemp(jl) .LE. 288.15_dp))
      END DO

      ! SST_corr_1 = 0.48_dp*dstmp**(-0.36_dp)    ! valid for Tw = 288.15K ; 15°C ; Tbase = 25°C
      ! SST_corr_2 = 1._dp                        ! valid for Tw = 298.15K ; 25°C 
      SST_corr_1 = 0.70_dp*dstmp**(-0.18_dp)    ! valid for Tw=288.15K ; 15°C ; Tbase = 20°C
      SST_corr_2 = 1.45_dp*dstmp**(0.18_dp)     ! valid for Tw=298.15K ; 25°C

      DO jl = jcs,jce
        SST_corr = (SST_corr_1*(298.15_dp-zseatemp(jl)) + SST_corr_2*(zseatemp(jl)-288.15_dp)) / 1.E+01_dp
        SST_mask3(jl) = MERGE(SST_corr, 0._dp, (zseatemp(jl) .GT. 288.15_dp))
      END DO

      DO jl = jcs,jce
        SST_corr_all(jl) = SST_mask1(jl) + SST_mask2(jl) + SST_mask3(jl) 
        nflux(jl,jb) = nflux(jl,jb) * SST_corr_all(jl)
      END DO
    END DO
 
    !--- output mass flux [kg/m2/s] and number flux [#/m2/s]
    DO jb = 2,nbin

      zav = speclist(id_ss)%density * pi/6._dp * dsdry(jb)**3

      IF (((dsdry(jb) .GT. dmod0) .AND. (dsdry(jb) .LE. dmod1))) THEN
        DO jl = jcs,jce
          pnumbf(jl) = pnumbf(jl) + nflux(jl,jb)*zseafrac(jl)
          pmassf(jl) = pmassf(jl) + nflux(jl,jb)*zseafrac(jl)*zav
        END DO
      END IF
 
    END DO
 
    END SUBROUTINE emi_seasalt_gong

END MODULE mo_ham_lite_seasalt