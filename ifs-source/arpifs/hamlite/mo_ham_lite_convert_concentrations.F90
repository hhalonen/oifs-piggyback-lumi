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

	! Self-contained kind and constants to avoid external module dependency
	INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307)
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
  SUBROUTINE conc2mmr(kproma, klev, &
		 kmodes, ktrac, pxtte, ptend_tot, pxtm1, prhoa, pprad, modefrac, rhop_modes)

	 INTEGER, INTENT(in) :: kproma, klev, kmodes, ktrac

	 REAL(dp), INTENT(INOUT) :: pxtte(kproma,klev,kmodes)
	 REAL(dp), INTENT(INOUT) :: modefrac(kmodes)

	 REAL(dp), INTENT(IN) :: prhoa(kproma,klev)       ! Air density [kg/m^3] (not used in mass; kept for interface)
	 REAL(dp), INTENT(IN) :: pprad(ktrac)   ! Representative radius [m]
	 REAL(dp), INTENT(IN) :: pxtm1(kproma,klev,ktrac) ! Unused here, kept for interface
	 REAL(dp), INTENT(IN) :: ptend_tot(kproma,klev,ktrac) ! [#/kg/s]

	 REAL(dp), INTENT(IN), OPTIONAL :: rhop_modes(kmodes) ! Particle densities [kg/m^3] per mode

	 !-- locals
	 INTEGER :: i, k, m
	 REAL(dp), PARAMETER :: one_third = 1.0_dp/3.0_dp
	 REAL(dp), PARAMETER :: four_thirds = 4.0_dp*one_third
	 REAL(dp), PARAMETER :: epsD = 1.0e-30_dp
	 REAL(dp) :: D, vol, mp
	 REAL(dp) :: rho_p_mode
	 INTEGER :: grp_of_mode(11)

	 REAL(dp) :: rho_p_defaults(11)

	 ! Map modes -> groups (1: Dust, 2: SS, 3: BC, 4: SO4)
	 grp_of_mode = (/ 1,1, 2,2, 3,3,3, 4,4,4,4 /)

	 ! Default particle material densities per mode [kg/m^3]
	 !   Dust ~2600, SeaSalt ~2160, BC ~1500, Sulfate ~1770
	 rho_p_defaults = (/ &
			2600.0_dp, 2600.0_dp,  & ! DU AS, DU CS
			2160.0_dp, 2160.0_dp,  & ! SS AS, SS CS
			1500.0_dp, 1500.0_dp, 1500.0_dp, & ! BC KS, BC AS, BC CS
			1770.0_dp, 1770.0_dp, 1770.0_dp, 1770.0_dp /) ! SO4 NS, KS, AS, CS

	 pxtte = 0.0_dp

	 DO m = 1, kmodes
		 DO i = 1, kproma
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
				 !hhermann: MERGE didn't work for me
				 !rho_p_mode = MERGE(rhop_modes(m), rho_p_defaults(m), PRESENT(rhop_modes))
				 if (present(rhop_modes)) then
					 rho_p_mode = rhop_modes(m)
				 else
				     rho_p_mode = rho_p_defaults(m)
				 end if
				 mp = rho_p_mode * vol
				 SELECT CASE (grp_of_mode(m))
				 CASE (1)
					 pxtte(i,k,m) = modefrac(m) * ptend_tot(i,k,1) * mp
				 CASE (2)
					 pxtte(i,k,m) = modefrac(m) * ptend_tot(i,k,2) * mp
				 CASE (3)
					 pxtte(i,k,m) = modefrac(m) * ptend_tot(i,k,3) * mp
				 CASE (4)
					 pxtte(i,k,m) = modefrac(m) * ptend_tot(i,k,4) * mp
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
  !   - modefrac is computed from (1,1) by default to match prior behavior,
  !     with guards to avoid a divide-by-zero.
  !   - Optional rhop_modes can override default particle densities.
  !--------------------------------------------------------------------
  SUBROUTINE mmr2conc(kidia, kfdia, kproma, klev, &
		 kmodes, ktrac, pmmr_modes, pmmr, pxtm1, prhoa, pprad, modefrac, rhop_modes)

	 INTEGER, INTENT(IN) :: kidia, kfdia, kproma, klev, kmodes, ktrac

	 REAL(dp), INTENT(INOUT) :: pmmr(kproma,klev,ktrac)
	 REAL(dp), INTENT(INOUT) :: pxtm1(kproma,klev,ktrac)  ! [#/kg]
	 REAL(dp), INTENT(INOUT) :: modefrac(kmodes) ! Proportion of each tracer mode in the total tracer concentration

	 REAL(dp), INTENT(IN) :: pmmr_modes(kproma,klev,kmodes)
	 REAL(dp), INTENT(IN) :: prhoa(kproma,klev)   ! Air density [kg/m^3] (not used in mass; kept for interface)
	 REAL(dp), INTENT(IN) :: pprad(ktrac)  ! Representative radius [m]

	 REAL(dp), INTENT(IN), OPTIONAL :: rhop_modes(kmodes) ! Particle densities [kg/m^3] per mode

	!-- locals
	INTEGER :: i, k, m
	REAL(dp), PARAMETER :: one_third = 1.0_dp/3.0_dp
	REAL(dp), PARAMETER :: four_thirds = 4.0_dp*one_third
	REAL(dp), PARAMETER :: epsD = 1.0e-30_dp
	REAL(dp), PARAMETER :: eps = 1.0e-30_dp
	REAL(dp) :: D, vol, mp
	REAL(dp) :: rho_p_mode
	INTEGER :: grp_of_mode(11)
	REAL(dp) :: rho_p_defaults(11)
	REAL(dp) :: denom

	 ! Map modes -> groups (1: Dust, 2: SS, 3: BC, 4: SO4)
	grp_of_mode = (/ 1,1, 2,2, 3,3,3, 4,4,4,4 /)

	 ! Default particle material densities per mode [kg/m^3]
	rho_p_defaults = (/ &
		 2600.0_dp, 2600.0_dp,  & ! DU AS, DU CS
		 2160.0_dp, 2160.0_dp,  & ! SS AS, SS CS
		 1500.0_dp, 1500.0_dp, 1500.0_dp, & ! BC KS, BC AS, BC CS
		 1770.0_dp, 1770.0_dp, 1770.0_dp, 1770.0_dp /) ! SO4 NS, KS, AS, CS

	 ! Combine modes -> group totals (mass mixing ratios)
	pmmr(:,:,1) = pmmr_modes(kidia:kfdia, 1:klev,1) + &
		& pmmr_modes(kidia:kfdia, 1:klev,2)
	pmmr(:,:,2) = pmmr_modes(kidia:kfdia, 1:klev,3) + &
		& pmmr_modes(kidia:kfdia, 1:klev,4)
	pmmr(:,:,3) = pmmr_modes(kidia:kfdia, 1:klev,5) + &
		& pmmr_modes(kidia:kfdia, 1:klev,6) + &
		& pmmr_modes(kidia:kfdia, 1:klev,7)
	pmmr(:,:,4) = pmmr_modes(kidia:kfdia, 1:klev,8) + &
		& pmmr_modes(kidia:kfdia, 1:klev,9) + &
		& pmmr_modes(kidia:kfdia, 1:klev,10) + &
		& pmmr_modes(kidia:kfdia, 1:klev,11)

	 ! modefrac from point (1,1) with guards
	denom = MAX(pmmr(1,1,1), eps)
	modefrac(1) = pmmr_modes(1,1,1) / denom
	modefrac(2) = pmmr_modes(1,1,2) / denom

	denom = MAX(pmmr(1,1,2), eps)
	modefrac(3) = pmmr_modes(1,1,3) / denom
	modefrac(4) = pmmr_modes(1,1,4) / denom

	denom = MAX(pmmr(1,1,3), eps)
	modefrac(5) = pmmr_modes(1,1,5) / denom
	modefrac(6) = pmmr_modes(1,1,6) / denom
	modefrac(7) = pmmr_modes(1,1,7) / denom

	denom = MAX(pmmr(1,1,4), eps)
	modefrac(8)  = pmmr_modes(1,1,8)  / denom
	modefrac(9)  = pmmr_modes(1,1,9)  / denom
	modefrac(10) = pmmr_modes(1,1,10) / denom
	modefrac(11) = pmmr_modes(1,1,11) / denom

	 ! Compute number concentrations per kg: sum over modes of (mmr_mode / m_p_mode)
	pxtm1 = 0.0_dp

		DO m = 1, kmodes
			!hhermann: MERGE didn't work for me
			!rho_p_mode = MERGE(rhop_modes(m), rho_p_defaults(m), PRESENT(rhop_modes))
			if (present(rhop_modes)) then
				rho_p_mode = rhop_modes(m)
			else
				rho_p_mode = rho_p_defaults(m)
			end if
			DO i = 1, kproma
				DO k = 1, klev
				    !hhermann: fixed dimensions of pprad -> different cases for each mode
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
					SELECT CASE (grp_of_mode(m))
					CASE (1)
						pxtm1(i,k,1) = pxtm1(i,k,1) + pmmr_modes(i,k,m) / MAX(mp, eps)
					CASE (2)
						pxtm1(i,k,2) = pxtm1(i,k,2) + pmmr_modes(i,k,m) / MAX(mp, eps)
					CASE (3)
						pxtm1(i,k,3) = pxtm1(i,k,3) + pmmr_modes(i,k,m) / MAX(mp, eps)
					CASE (4)
						pxtm1(i,k,4) = pxtm1(i,k,4) + pmmr_modes(i,k,m) / MAX(mp, eps)
					END SELECT
				END DO
			END DO
		END DO

	 ! Non-negativity guard
	 pxtm1 = MAX(pxtm1, 0.0_dp)

  END SUBROUTINE mmr2conc
END MODULE mo_ham_lite_convert_concentrations
