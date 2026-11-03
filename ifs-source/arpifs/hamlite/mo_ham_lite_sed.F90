! 
! based on mo_ham_sedimentation.f90  
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

MODULE mo_ham_lite_sed

  USE mo_kind_lite,  ONLY: dp

  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: ham_lite_prep_sed, ham_lite_sed

  CONTAINS

!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE ham_lite_prep_sed( jcs, jce, kproma, klev, ptm1, papm1, pvisc, ppath )
  
    USE mo_ham_lite_misc,   ONLY: ham_lite_mfpath, ham_lite_dyvisc

    IMPLICIT NONE

    INTEGER, INTENT(IN)     ::  jcs, jce, kproma, klev         
    REAL(dp), INTENT(IN)    ::  ptm1  (:,:),  & ! temperature 
                                papm1 (:,:)     ! full level pressure
    REAL(dp), INTENT(INOUT) ::  pvisc (:,:),  & ! dynamic viscosity
                                ppath (:,:)     ! mean free path  
  
    INTEGER  :: jk, jl
    REAL(dp) :: ztmpc, ztmp1  

    !$ACC DATA PRESENT(ptm1,papm1,pvisc,ppath)

    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)  
    !$ACC LOOP GANG VECTOR COLLAPSE(2)
    DO jk = 1,klev
      DO jl = jcs,jce

        !--- dynamic viscosity of air [Pa s]  
        pvisc(jl,jk) = ham_lite_dyvisc(ptm1(jl,jk))

        !--- mean free path of air [m] 
        ppath(jl,jk) = ham_lite_mfpath(ptm1(jl,jk), papm1(jl,jk))

      END DO
    END DO
    !$ACC END PARALLEL LOOP

    !$ACC END DATA

  END SUBROUTINE ham_lite_prep_sed

!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE ham_lite_sed(  jg, jb, jcs, jce, jt, kproma, klev,   & 
                            pdtime,                               &
                            pdz, prho, pmair, pxtm1,              & 
                            pvisc, plair,                         &
                            pvsed, psedflux, psedtend, pm6rp, prhop)

    USE mo_exception_lite,           ONLY: message, message_text  
    USE mo_physical_constants_lite,  ONLY: grav 
    USE mo_subm_trac_lite,           ONLY: trlist
    USE mo_ham_lite_main,            ONLY: silog_mode
    !USE mo_ham_lite_streams,    ONLY: ham_field ! rwet, daer 

    IMPLICIT NONE
    
    INTEGER, INTENT(IN)    :: jg, jb, jcs, jce, jt, kproma, klev 
    REAL(dp), INTENT(IN)   :: pdtime,       &   ! time step
                              pdz   (:,:),  &   ! geometric height thickness of layer
                              prho  (:,:),  &   ! density
                              pmair (:,:),  &   ! air mass [kg/m2]
                              pxtm1 (:,:),  &   ! tracer number mixing ratio
                              pvisc (:,:),  &   ! dynamic viscosity
                              plair (:,:),  &   ! mean free path
                              pm6rp (:,:,:,:),& ! aerosol wet radius per mode [m]
                              prhop (:,:,:,:)   ! mean mode particle density [kg m-3]
    REAL(dp), INTENT(INOUT) ::  pvsed     (:,:),  & ! sedimentation velocity [m/s]
                                psedflux  (:,:),  & ! sedimentation flux [#/m2/s] 
                                psedtend  (:,:)     ! sedimentation tendency [#/kg/s]
                                
    INTEGER :: jclass, jk, jl
    REAL(dp)  ::  zslinn, zsigma
    REAL(dp)  ::  zdwet     (kproma,klev),  &  
                  zvsed     (kproma,klev),  & ! sedimentation velocity
                  zsedtend  (kproma,klev)     ! sedimentation tendency   
    !REAL(dp), POINTER :: rwet_p(:,:,:), daer_p(:,:,:)
    REAL(dp) :: rwet_p(kproma,klev,jb), daer_p(kproma,klev,jb)
    LOGICAL :: ltest 
 
    !$ACC DATA PRESENT(pdz,prho,pmair,pxtm1,pvisc,plair,pvsed,psedflux,psedtend) &
    !$ACC   PRESENT(pm6rp,prhop) &
    !$ACC   CREATE(zdwet,zvsed,zsedtend,rwet_p,daer_p)

    ! look up mode of tracer and get wet radius and density
    jclass = trlist%ti(jt)%mode
    zsigma = silog_mode(jclass)
    
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

    ! select diameter and limit it to maximal 50 um  
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR COLLAPSE(2)
    DO jk = 1,klev
      DO jl = jcs,jce
        zdwet(jl,jk) = MIN(rwet_p(jl,jk,jb) * 2._dp, 50.E-6_dp)
      END DO
    END DO 
    !$ACC END PARALLEL LOOP

    ! get Stokes velocity, Seinfeld and Pandis (2010)
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR COLLAPSE(2)
    DO jk = 1,klev
      DO jl = jcs,jce 
        zvsed(jl,jk) = 2._dp / 9._dp * (daer_p(jl,jk,jb) - prho(jl,jk)) *  &
          grav / pvisc(jl,jk) * (zdwet(jl,jk) / 2._dp)**2._dp
      END DO
    END DO 
    !$ACC END PARALLEL LOOP

    ! add Cunningham correction, Riemer (2002)  
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR COLLAPSE(2)
    DO jk = 1,klev
      DO jl = jcs,jce
        zvsed(jl,jk) = zvsed(jl,jk) * (exp(2._dp * zsigma**2._dp) + &
          1.246_dp * 2._dp * plair(jl,jk) / zdwet(jl,jk) * exp(0.5_dp * zsigma**2._dp))
      END DO
    END DO 
    !$ACC END PARALLEL LOOP

    ! limit sedimentation velocity to grid velocity (dz / dt) 
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR COLLAPSE(2)
    DO jk = 1,klev
      DO jl = jcs,jce
        zvsed(jl,jk) = MIN(zvsed(jl,jk), pdz(jl,jk) / pdtime)
        pvsed(jl,jk) = MAX(0.0_dp, zvsed(jl,jk))
      END DO
    END DO 
    !$ACC END PARALLEL LOOP

    ! get sedimentation tendency and flux  
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR COLLAPSE(2) PRIVATE(ltest)
    DO jk = 1,klev
      DO jl = jcs,jce
        ltest = (pxtm1(jl,jk) > 0.0_dp)  
        zsedtend(jl,jk) = MERGE(pxtm1(jl,jk) * pvsed(jl,jk) / pdz(jl,jk), 0.0_dp, ltest) 
        psedflux(jl,jk) = zsedtend(jl,jk) * pmair(jl,jk)
      END DO
    END DO 
    !$ACC END PARALLEL LOOP

    ! apply tracer tendency throughout the column and
    ! add tracer tendency of the level above to the level below  
    ! sedimentation to the surface is done in the dry deposition scheme  
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR COLLAPSE(2)
    DO jk = 1,klev
      DO jl = jcs,jce 
        IF (jk == 1) THEN
          psedtend(jl,jk) = -1.0_dp * zsedtend(jl,jk)
        ELSE IF (jk == klev) THEN
          psedtend(jl,jk) = psedflux(jl,jk-1) / pmair(jl,jk)
        ELSE
          psedtend(jl,jk) = -1.0_dp * zsedtend(jl,jk) + (psedflux(jl,jk-1) / pmair(jl,jk))
        END IF 
      END DO
    END DO 
    !$ACC END PARALLEL LOOP

    !$ACC END DATA 

  END SUBROUTINE ham_lite_sed

END MODULE mo_ham_lite_sed
