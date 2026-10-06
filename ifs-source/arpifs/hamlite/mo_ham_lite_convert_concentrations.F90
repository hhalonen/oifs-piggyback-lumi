!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!>
!! \filename
!!   mo_ham_lite_convert_concentrations.f90
!!
!! Utilities to convert between modal mass mixing ratios and number
!! concentrations for HAM-lite (box) model.
!!
!! This version fixes unit consistency and particle mass computation:
!! - pxtm1 is number concentration per kg of air [#/kg].
!! - Particle mass uses particle material density (not air density).
!! - Correct 4/3 factor in sphere volume.
!! - Safer guards for zero/near-zero values.
!! - More consistent mapping between 11 modes and 4 tracer groups.
!!
!! Optional per-mode densities can be provided to override defaults.
!!
!! Author(s):
!!   Original: Hermanni Halonen (UEF)
!!   Updates:  Arup Sarkar (FMI)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE mo_ham_lite_convert_concentrations

    USE mo_kind_lite, ONLY : wp, dp
    USE PARKIND1,     ONLY: JPRB

    IMPLICIT NONE

    ! Self-contained kind and constants to avoid external module dependency
    !INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307)
    REAL(dp), PARAMETER :: pi = 3.1415926535897932384626433832795_dp

    PUBLIC :: conc2mmr
    PUBLIC :: mmr2conc
    
    CONTAINS

  !--------------------------------------------------------------------
  ! Convert number-concentration tendency to mass-mixing-ratio tendency
  ! and distribute to modes using provided modal fractions.
  !
  ! Units:
  !   ptend_tot: [#/kg/s] (number tendency per kg of air)
  !   pxtte:     [kg/kg/s] (mass mixing ratio tendency)
  !   modefrac:  dimensionless fractions per mode (sum to 1 within group)
  !
  ! Notes:
  !   - Particle mass is computed as m_p = rho_p(mode) * (4/3)*pi*(D/2)^3.
  !   - A single diameter field is used for all modes at a grid point.
  !   - Optional rhop_modes can override default particle densities.
  !--------------------------------------------------------------------
  SUBROUTINE conc2mmr(kidia, kfdia, kproma, klev, &
        kmodes, ktrac, pxtte, ptend_tot, pxtm1, prhoa, pprad, modefrac_num, rhop_modes)

    INTEGER, INTENT(IN)  :: kidia, kfdia, kproma, klev, kmodes, ktrac

    REAL(wp), INTENT(OUT):: pxtte(kproma,klev,kmodes)
    REAL(wp), INTENT(INOUT) :: modefrac_num(kproma,klev,kmodes)
         
    REAL(KIND=JPRB), INTENT(IN) :: prhoa(kproma,klev)       ! Air density [kg/m^3] (not used in mass; kept for interface)
    REAL(dp), INTENT(IN) :: pprad(ktrac)                    ! Representative radius (dry) [m]
    REAL(dp), INTENT(IN) :: pxtm1(kproma,klev,ktrac)        ! Unused here, kept for interface
    REAL(dp), INTENT(IN) :: ptend_tot(kproma,klev,ktrac)    ! [#/kg/s]
    
    REAL(dp), INTENT(IN), OPTIONAL :: rhop_modes(kmodes)    ! Particle densities [kg/m^3] per mode
    
    !-- locals
    INTEGER :: i, k, m
    REAL(dp), PARAMETER :: one_third = 1.0_dp/3.0_dp
    REAL(dp), PARAMETER :: four_thirds = 4.0_dp*one_third
    REAL(dp), PARAMETER :: epsD = 1.0e-20_dp
    REAL(dp) :: D, vol, mp
    REAL(dp) :: rho_p_mode
    INTEGER :: grp_of_mode(kmodes)
    INTEGER :: grp_of_tracer(kmodes)
    REAL(dp) :: rho_p_defaults(kmodes)
    
    ! Map tracers
    grp_of_tracer = (/ 1,1,1,1, 2,2, 3,3,3,3,4,4,4,3, 4,4,4 /)
    ! Map modes
    grp_of_mode = (/ 3,4,3,4, 3,4, 2,3,4,2,2,3,4,2, 2,3,4 /)
    
    ! Default particle material densities per mode [kg/m^3]
    rho_p_defaults = (/ &
        2600.0_dp, 2600.0_dp, 2600.0_dp, 2600.0_dp, &   ! DU AS, DU CS, DU AI, DU CI
        2160.0_dp, 2160.0_dp,  &                        ! SS AS, SS CS
        1500.0_dp, 1500.0_dp, 1500.0_dp, 1500.0_dp, &   ! BC KS, BC AS, BC CS, BC KI
        1500.0_dp, 1500.0_dp, 1500.0_dp, 1500.0_dp, &   ! OC KS, OC AS, OC CS, OC KI
        1770.0_dp, 1770.0_dp, 1770.0_dp /)              ! SO4 KS, SO4 AS, SO4 CS

    pxtte = 0.0_dp

    DO m = 1, kmodes
        DO i = kidia, kfdia
            DO k = 1, klev
                SELECT CASE (grp_of_mode(m))
                    CASE (1)
                        D = MAX(pprad(1), epsD)
                    CASE (2)
                        D = MAX(pprad(2), epsD)
                    CASE (3)
                        D = MAX(pprad(3), epsD)
                    CASE (4)
                        D = MAX(pprad(4), epsD)
                    END SELECT
                    vol = four_thirds * pi * D**3
                    if (present(rhop_modes)) then
                        rho_p_mode = rhop_modes(m)
                    else
                        rho_p_mode = rho_p_defaults(m)
                    end if
                    mp = rho_p_mode * vol
                    SELECT CASE (grp_of_tracer(m))
                    CASE (1)
                        pxtte(i,k,m) = modefrac_num(i,k,m) * ptend_tot(i,k,1) * mp
                    CASE (2)
                        pxtte(i,k,m) = modefrac_num(i,k,m) * ptend_tot(i,k,2) * mp
                    CASE (3)
                        pxtte(i,k,m) = modefrac_num(i,k,m) * ptend_tot(i,k,3) * mp
                    CASE (4)
                        pxtte(i,k,m) = modefrac_num(i,k,m) * ptend_tot(i,k,4) * mp
                    END SELECT
                END DO
            END DO
        END DO

    END SUBROUTINE conc2mmr


  !--------------------------------------------------------------------
  ! Convert modal mass mixing ratios to group totals and compute
  ! number concentrations per kg of air using particle material density.
  !
  ! Units:
  !   pmmr_modes: [kg/kg]
  !   pmmr:       [kg/kg]
  !   pxtm1:      [#/kg]
  !
  ! Notes:
  !   - pxtm1 per group is computed as sum over modes in that group:
  !       N/kg = sum_m ( pmmr_mode / m_p_mode ).
  !   - Optional rhop_modes can override default particle densities.
  !--------------------------------------------------------------------
  SUBROUTINE mmr2conc(kidia, kfdia, kproma, klev, &
            kmodes, ktrac, pmmr_modes, pxtm1, prhoa, pprad, modefrac_num, rhop_modes)
    
    INTEGER, INTENT(IN) :: kidia, kfdia, kproma, klev, kmodes, ktrac
    
    REAL(dp), INTENT(INOUT) :: pxtm1(kproma,klev,ktrac)  ! [#/kg]
    REAL(wp), INTENT(INOUT) :: modefrac_num(kproma,klev,kmodes)
    
    REAL(wp), INTENT(IN) :: pmmr_modes(kproma,klev,kmodes)
    REAL(KIND=JPRB), INTENT(IN) :: prhoa(kproma,klev)    ! Air density [kg/m^3] (not used in mass; kept for interface)
    REAL(dp), INTENT(IN) :: pprad(ktrac)                 ! Representative radius (dry) [m]
    
    REAL(dp), INTENT(IN), OPTIONAL :: rhop_modes(kmodes) ! Particle densities [kg/m^3] per mode
    
    !-- locals
    INTEGER :: i, k, m
    REAL(dp), PARAMETER :: one_third = 1.0_dp/3.0_dp
    REAL(dp), PARAMETER :: four_thirds = 4.0_dp*one_third
    REAL(dp), PARAMETER :: epsD = 1.0e-20_dp
    REAL(wp), PARAMETER :: eps = 1.0e-20_wp
    REAL(dp), PARAMETER :: minfrac = 1.e-20_dp   ! Minimum modefrac
    REAL(dp) :: D, vol, mp
    REAL(dp) :: rho_p_mode
    INTEGER :: grp_of_mode(kmodes)
    INTEGER :: grp_of_tracer(kmodes)
    REAL(dp) :: rho_p_defaults(kmodes)
    REAL(dp) :: denom
    REAL(wp) :: pxtm1_mode(kproma,klev,kmodes)
    REAL(wp) :: pxtm1_group(kproma,klev,ktrac)
    
    ! Map tracers
    grp_of_tracer = (/ 1,1,1,1, 2,2, 3,3,3,3,4,4,4,3, 4,4,4 /)
    ! Map modes
    grp_of_mode = (/ 3,4,3,4, 3,4, 2,3,4,2,2,3,4,2, 2,3,4 /)
    
    ! Default particle material densities per mode [kg/m^3]
    rho_p_defaults = (/ &
        2600.0_dp, 2600.0_dp, 2600.0_dp, 2600.0_dp, &   ! DU AS, DU CS, DU AI, DU CI
        2160.0_dp, 2160.0_dp,  &                        ! SS AS, SS CS
        1500.0_dp, 1500.0_dp, 1500.0_dp, 1500.0_dp, &   ! BC KS, BC AS, BC CS, BC KI
        1500.0_dp, 1500.0_dp, 1500.0_dp, 1500.0_dp, &   ! OC KS, OC AS, OC CS, OC KI
        1770.0_dp, 1770.0_dp, 1770.0_dp /)              ! SO4 KS, SO4 AS, SO4 CS
    
    ! Compute number concentrations per kg: sum over modes of (mmr_mode / m_p_mode)
    pxtm1 = 0.0_dp
    pxtm1_mode = 0.0_dp
    pxtm1_group = 0.0_dp

    DO m = 1, kmodes
        if (present(rhop_modes)) then
            rho_p_mode = rhop_modes(m)
        else
            rho_p_mode = rho_p_defaults(m)
        end if

        SELECT CASE (grp_of_mode(m))
            CASE (1)
                D = MAX(pprad(1), epsD)
            CASE (2)
                D = MAX(pprad(2), epsD)
            CASE (3)
                D = MAX(pprad(3), epsD)
            CASE (4)
                D = MAX(pprad(4), epsD)
        END SELECT
        vol = four_thirds * pi * D**3
        mp = rho_p_mode * vol
        DO i = kidia, kfdia
            DO k = 1, klev
                !-->hhermann: Number fractions
                ! Modes separately
                pxtm1_mode(i,k,m) = pmmr_modes(i,k,m) / MAX(mp, eps)
                ! Modes lumped
                pxtm1_group(i,k,grp_of_tracer(m)) = pxtm1_group(i,k,grp_of_tracer(m)) + pxtm1_mode(i,k,m)
                !<--hhermann
            END DO
        END DO
    END DO

    !-->hhermann: Number fractions
    DO m = 1, kmodes
        DO i = kidia, kfdia
            DO k = 1, klev
                IF (pxtm1_group(i,k,grp_of_tracer(m)) > eps) THEN
                    modefrac_num(i,k,m) = pxtm1_mode(i,k,m) / pxtm1_group(i,k,grp_of_tracer(m))
                ELSE
                    modefrac_num(i,k,m) = 0.0_dp
                END IF
            END DO
        END DO
    END DO
    !<--hhermann

    ! Non-negativity guard
    pxtm1 = MAX(pxtm1_group, 0.0_dp)

    END SUBROUTINE mmr2conc
    
    
  SUBROUTINE mmr2conctend(kidia, kfdia, kproma, klev, &
            kmodes, ktrac, pmmr_modes, pxtm1, prhoa, pprad, modefrac_num, rhop_modes)
    
    INTEGER, INTENT(IN) :: kidia, kfdia, kproma, klev, kmodes, ktrac
    
    REAL(dp), INTENT(INOUT) :: pxtm1(kproma,klev,ktrac)  ! [#/kg]
    REAL(wp), INTENT(INOUT) :: modefrac_num(kproma,klev,kmodes)
    
    REAL(wp), INTENT(IN) :: pmmr_modes(kproma,klev,kmodes)
    REAL(KIND=JPRB), INTENT(IN) :: prhoa(kproma,klev)    ! Air density [kg/m^3] (not used in mass; kept for interface)
    REAL(dp), INTENT(IN) :: pprad(ktrac)                 ! Representative radius (dry) [m]
    
    REAL(dp), INTENT(IN), OPTIONAL :: rhop_modes(kmodes) ! Particle densities [kg/m^3] per mode
    
    !-- locals
    INTEGER :: i, k, m
    REAL(dp), PARAMETER :: one_third = 1.0_dp/3.0_dp
    REAL(dp), PARAMETER :: four_thirds = 4.0_dp*one_third
    REAL(dp), PARAMETER :: epsD = 1.0e-20_dp
    REAL(wp), PARAMETER :: eps = 1.0e-20_wp
    REAL(dp), PARAMETER :: minfrac = 1.e-20_dp   ! Minimum modefrac
    REAL(dp) :: D, vol, mp
    REAL(dp) :: rho_p_mode
    INTEGER :: grp_of_mode(kmodes)
    INTEGER :: grp_of_tracer(kmodes)
    REAL(dp) :: rho_p_defaults(kmodes)
    REAL(dp) :: denom
    REAL(wp) :: pxtm1_mode(kproma,klev,kmodes)
    REAL(wp) :: pxtm1_group(kproma,klev,ktrac)
    
    ! Map tracers
    grp_of_tracer = (/ 1,1,1,1, 2,2, 3,3,3,3,4,4,4,3, 4,4,4 /)
    ! Map modes
    grp_of_mode = (/ 3,4,3,4, 3,4, 2,3,4,2,2,3,4,2, 2,3,4 /)
    
    ! Default particle material densities per mode [kg/m^3]
    rho_p_defaults = (/ &
        2600.0_dp, 2600.0_dp, 2600.0_dp, 2600.0_dp, &   ! DU AS, DU CS, DU AI, DU CI
        2160.0_dp, 2160.0_dp,  &                        ! SS AS, SS CS
        1500.0_dp, 1500.0_dp, 1500.0_dp, 1500.0_dp, &   ! BC KS, BC AS, BC CS, BC KI
        1500.0_dp, 1500.0_dp, 1500.0_dp, 1500.0_dp, &   ! OC KS, OC AS, OC CS, OC KI
        1770.0_dp, 1770.0_dp, 1770.0_dp /)              ! SO4 KS, SO4 AS, SO4 CS
    
    ! Compute number concentrations per kg: sum over modes of (mmr_mode / m_p_mode)
    pxtm1 = 0.0_dp
    pxtm1_mode = 0.0_dp
    pxtm1_group = 0.0_dp

    DO m = 1, kmodes
        if (present(rhop_modes)) then
            rho_p_mode = rhop_modes(m)
        else
            rho_p_mode = rho_p_defaults(m)
        end if

        SELECT CASE (grp_of_mode(m))
            CASE (1)
                D = MAX(pprad(1), epsD)
            CASE (2)
                D = MAX(pprad(2), epsD)
            CASE (3)
                D = MAX(pprad(3), epsD)
            CASE (4)
                D = MAX(pprad(4), epsD)
        END SELECT
        vol = four_thirds * pi * D**3
        mp = rho_p_mode * vol
        DO i = kidia, kfdia
            DO k = 1, klev
                !-->hhermann: Number fractions
                ! Modes separately
                pxtm1_mode(i,k,m) = pmmr_modes(i,k,m) / MAX(mp, eps)
                ! Modes lumped
                pxtm1_group(i,k,grp_of_tracer(m)) = pxtm1_group(i,k,grp_of_tracer(m)) + pxtm1_mode(i,k,m)
                !<--hhermann
            END DO
        END DO
    END DO

    !-->hhermann: Number fractions
    !DO m = 1, kmodes
    !    DO i = kidia, kfdia
    !        DO k = 1, klev
    !            IF (pxtm1_group(i,k,grp_of_tracer(m)) > eps) THEN
    !                modefrac_num(i,k,m) = pxtm1_mode(i,k,m) / pxtm1_group(i,k,grp_of_tracer(m))
    !            ELSE
    !                modefrac_num(i,k,m) = 0.0_dp
    !            END IF
    !        END DO
    !    END DO
    !END DO
    !<--hhermann

    ! Non-negativity guard
    !pxtm1 = MAX(pxtm1_group, 0.0_dp)
    pxtm1 = pxtm1_group

    END SUBROUTINE mmr2conctend
  
END MODULE mo_ham_lite_convert_concentrations