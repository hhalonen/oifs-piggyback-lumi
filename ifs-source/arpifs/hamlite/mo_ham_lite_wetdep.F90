! 
! based on mo_ham_wetdep.f90 
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

MODULE mo_ham_lite_wetdep

  USE mo_kind_lite,                  ONLY: dp   
  USE mo_physical_constants_lite,    ONLY: tmelt  
  
  IMPLICIT none
  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: ham_lite_prep_wetdep, ham_lite_wetdep 
  PUBLIC :: qnum_min, qcld_min, tfrz_hom

  !--------------------
  ! module parameters
  !--------------------

  REAL(dp), PARAMETER ::        &
    qnum_min  = 1.0E-15_dp,     & ! threshold for computations
    qcld_min  = 1.0E-06_dp,     & ! threshold for cloud water and ice
    tfrz_hom  = tmelt - 37.0_dp   ! temperature for homogeneous freezing of cloud water

  CONTAINS 
 
!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_prep_wetdep(  jcs, jce, jks, klev, kproma,        & 
                                    pxlta, pxlqc, pxlqi,                &
                                    pxlqr, pxlqs, pxlqg,                & 
                                    pxprr, pxpri, pxprs, pxprg, pmair,  &
                                    peffic, pratio                      ) 
 
    USE mo_kind_lite,                ONLY: wp, dp 
    !USE mo_mpi_lite,                 ONLY: p_pe, p_io 
    USE mo_physical_constants_lite,  ONLY: tmelt ! melting temperature of ice/snow

    INTEGER, INTENT(IN) ::  jcs, jce, & ! start and end indices of columns loop 
                            jks,      & ! index of top level 
                            klev,     & ! number of levels  
                            kproma      ! maximum number of columns
    REAL(dp), INTENT(IN) :: pxlta(kproma,klev),   & ! air temperature
                            pxlqc(kproma,klev),   & ! cloud water
                            pxlqi(kproma,klev),   & ! cloud ice
                            pxlqr(kproma,klev),   & ! rain  
                            pxlqs(kproma,klev),   & ! snow
                            pxlqg(kproma,klev),   & ! graupel
                            pxprr(kproma),        & ! precip rate rain    
                            pxpri(kproma),        & ! precip rate ice    
                            pxprs(kproma),        & ! precip rate snow    
                            pxprg(kproma),        & ! precip rate graupel  
                            pmair(kproma,klev)      ! air mass  
    REAL(dp), INTENT(INOUT) :: peffic(kproma),    & ! precipitation efficiency [1/s]
                               pratio(kproma,klev)  ! ratio of rain, snow, and graupel to cloud water and ice

    LOGICAL :: ltest
    INTEGER :: jl, jk  
    REAL(dp) :: zxlqa, zxlqb, &
                zxlqv(kproma) ! vertically integrated rain, snow, and graupel [kg/m2]
 
    DO jl = jcs,jce
      zxlqv(jl) = 0.0_dp 
    END DO

    ! compute ratio of rain, snow, and graupel to cloud water and ice
    DO jk = jks,klev
      DO jl = jcs,jce  
        zxlqa = pxlqc(jl,jk) + pxlqi(jl,jk) + pxlqr(jl,jk) + pxlqs(jl,jk) + pxlqg(jl,jk)   
        zxlqb = pxlqr(jl,jk) + pxlqs(jl,jk) + pxlqg(jl,jk) 
        ltest = (zxlqa > qcld_min)  
        pratio(jl,jk) = MERGE(zxlqb / zxlqa, 0.0_dp, ltest) 
      END DO   
    END DO 

    ! compute precipitation efficiency
    DO jk = jks,klev
      DO jl = jcs,jce  
        zxlqa = pxlqr(jl,jk) + pxlqs(jl,jk) + pxlqg(jl,jk)
        zxlqv(jl) = zxlqv(jl) + zxlqa * pmair(jl,jk)  
      END DO   
    END DO 
 
    DO jl = jcs,jce 
      zxlqa = pxprr(jl) + pxpri(jl) + pxprs(jl) + pxprg(jl) 
      ltest = (zxlqv(jl) > qnum_min)  
      peffic(jl) = MERGE(zxlqa / zxlqv(jl), 0.0_dp, ltest)  
    END DO

  END SUBROUTINE ham_lite_prep_wetdep

!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_wetdep( jcs, jce, jks, jt, klev, kproma,    & 
                              pdtime,                             &
                              pxlta, pxlqc, pxlqi, pmair, pxtm1,  &
                              peffic, pratio, pxtte, pdepflux     ) 
 
    USE mo_kind_lite,                ONLY: wp, dp 
    !USE mo_mpi_lite,                 ONLY: p_pe, p_io
    USE mo_ham_lite_misc,            ONLY: pbase, pfrac
    USE mo_physical_constants_lite,  ONLY: tmelt ! melting temperature of ice/snow

    INTEGER, INTENT(IN) ::  jcs, jce, & ! start and end indices of columns loop 
                            jks,      & ! index of top level
                            jt,       & ! tracer index
                            klev,     & ! number of levels 
                            kproma      ! maximum number of columns
    REAL(dp), INTENT(IN) :: pdtime,               & ! time step
                            pxlta(kproma,klev),   & ! air temperature
                            pxlqc(kproma,klev),   & ! cloud water  
                            pxlqi(kproma,klev),   & ! cloud ice    
                            pmair(kproma,klev),   & ! air mass [kg/m2]
                            pxtm1(kproma,klev),   & ! tracer number mixing ratio  
                            peffic(kproma),       & ! precipitation efficiency  
                            pratio(kproma,klev)     ! ratio of rain, snow, and graupel to cloud water and ice
    REAL(dp), INTENT(INOUT) :: pxtte(kproma,klev),  & ! tracer tendency [#/kg/s]
                               pdepflux(kproma)       ! wet deposition flux [#/m2/s]  

    LOGICAL :: ltest
    INTEGER :: jl, jk, jkba
    REAL(dp) :: zxtte 
 
    ! compute tracer tendency 
    DO jk = jks,klev
      DO jl = jcs,jce 
        jkba = pbase(jl,jk)
        IF ((jkba >= jks) .AND. (jkba <= klev)) THEN  
          ! compute tendency
          ltest = ((pxlqc(jl,jk) + pxlqi(jl,jk)) > qcld_min)  
          zxtte = MERGE(pfrac(jl,jkba,jt) * pratio(jl,jk) * peffic(jl), 0.0_dp, ltest)
          ltest = (pxtm1(jl,jk) > 0.0_dp)
          zxtte = MERGE(pxtm1(jl,jk) * zxtte, 0.0_dp, ltest)
          
          ! limit tendency
          zxtte = MIN(zxtte, pxtm1(jl,jk) / pdtime)   
          zxtte = MAX(zxtte, 0.0_dp) 
          pxtte(jl,jk) = -1.0_dp * zxtte
        END IF
      END DO  
    END DO
    
    ! compute wet deposition flux
    DO jk = jks,klev
      DO jl = jcs,jce
        pdepflux(jl) = pdepflux(jl) - pxtte(jl,jk) * pmair(jl,jk)
      END DO
    END DO

  END SUBROUTINE ham_lite_wetdep

END MODULE mo_ham_lite_wetdep
