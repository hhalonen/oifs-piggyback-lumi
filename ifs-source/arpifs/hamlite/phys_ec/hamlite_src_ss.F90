SUBROUTINE HAMLITE_SRC_SS &
  &( KIDIA, KFDIA, KLON, PCI, PCLAKE, PLSM, PSST, PWIND, &
  &  PEMISS_MASS, YDEAERSRC)

!**** *HAMLITE_SRC_SS* - TOTAL SEA SALT MASS EMISSION FOR HAMLITE
!
!     Purpose:
!     --------
!     Compute total sea salt mass emission flux (kg/m2/s) by summing
!     accumulation and coarse mode contributions from the Gong (2003)
!     parameterization.
!
!     Physics is extracted from TM5M7_SRC_SS but adapted for HAMlite:
!     - Both modes summed to produce a single total mass flux
!     - Number flux derived externally by the caller using HAMlite
!       effective particle properties (supervisor directive: "total mass
!       then conversion to number /kg for hamlite")
!
!     References:
!       Gong (2003), doi:10.1029/2003GB002079
!       Vignati et al. (2010), doi:10.1016/j.atmosenv.2009.11.013
!       Monahan et al. (1986)  - whitecap wind dependence
!       Salter et al. (2015)   - SST-dependent number flux
!
!     Interface:
!     ----------
!     Called from TM5M7_PHY2 (hamlite branch in AERINI_LAYER pathway).
!
!     Author:
!     -------
!     A. Sarkar / FMI, 2026 - Adapted from TM5M7_SRC_SS (T. van Noije)
!
!     Modifications:
!     --------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK
USE YOMCST    ,ONLY : RPI
USE YOEAERSRC ,ONLY : TEAERSRC

IMPLICIT NONE

!*       0.1   ARGUMENTS

INTEGER(KIND=JPIM),    INTENT(IN)  :: KIDIA, KFDIA, KLON
REAL(KIND=JPRB),       INTENT(IN)  :: PCI(KLON)
REAL(KIND=JPRB),       INTENT(IN)  :: PCLAKE(KLON)
REAL(KIND=JPRB),       INTENT(IN)  :: PLSM(KLON)
REAL(KIND=JPRB),       INTENT(IN)  :: PSST(KLON)
REAL(KIND=JPRB),       INTENT(IN)  :: PWIND(KLON)
REAL(KIND=JPRB),       INTENT(OUT) :: PEMISS_MASS(KLON)
TYPE(TEAERSRC),        INTENT(IN)  :: YDEAERSRC

!*       0.2   PARAMETERS
!     M7 lognormal mode parameters used for total mass integration.
!     These match TM5M7_SRC_SS values for consistency.
!     radius_ssa/ssc : number mean dry radius [m]
!     sigma          : geometric standard deviation of lognormal mode
!     SS_DENSITY     : sea salt bulk density [kg/m3]

REAL(KIND=JPRB), PARAMETER :: ZRADI_ACS   = 0.09E-6_JPRB       ! accumulation mode [m]
REAL(KIND=JPRB), PARAMETER :: ZRADI_COS   = 0.794E-6_JPRB      ! coarse mode [m]
REAL(KIND=JPRB), PARAMETER :: ZSIGMA_ACS  = 2.0_JPRB
REAL(KIND=JPRB), PARAMETER :: ZSIGMA_COS  = 2.03_JPRB
REAL(KIND=JPRB), PARAMETER :: ZSS_DENS    = 2165.0_JPRB        ! [kg/m3]

!     Gong (2003) number emission prefactors for two size ranges:
!     Obtained by fitting Gong source function to accumulation (r1: 0.05-0.5 um)
!     and coarse (r2: 0.5-5 um) dry radii (see TM5M7_SRC_SS comments).
REAL(KIND=JPRB), PARAMETER :: ZPREFAC_ACS = 9.62013E-3_JPRB
REAL(KIND=JPRB), PARAMETER :: ZPREFAC_COS = 9.05658E-4_JPRB

!*       0.5   LOCAL VARIABLES

INTEGER(KIND=JPIM) :: JL
REAL(KIND=JPRB) :: ZXSEA, ZAREA_FRAC, ZTT, ZT_SCALE
REAL(KIND=JPRB) :: ZEMIS_FAC, ZNUM_ACS, ZNUM_COS
REAL(KIND=JPRB) :: ZMASS_ACS, ZMASS_COS
REAL(KIND=JPRB) :: ZDENS_CGS, ZRG1_UM, ZRG2_UM
REAL(KIND=JPRB) :: ZSSCAL
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!-----------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('HAMLITE_SRC_SS',0,ZHOOK_HANDLE)

!     Control flag from namelist
IF (YDEAERSRC%NOSEASALT) THEN
  ZSSCAL = 0.0_JPRB
ELSE
  ZSSCAL = 1.0_JPRB
ENDIF

!     Unit conversions for mass calculation
ZDENS_CGS = ZSS_DENS * 0.001_JPRB              ! kg/m3 -> g/cm3
ZRG1_UM   = ZRADI_ACS * 1.0E6_JPRB             ! m -> um
ZRG2_UM   = ZRADI_COS * 1.0E6_JPRB             ! m -> um

PEMISS_MASS(:) = 0.0_JPRB

DO JL = KIDIA, KFDIA

  !     Ocean fraction without ice or lake
  ZXSEA = 1.0_JPRB - PLSM(JL)
  ZAREA_FRAC = ZXSEA * (1.0_JPRB - PCI(JL))
  IF (ZAREA_FRAC < 1.0E-10_JPRB .OR. PCLAKE(JL) > 0.0_JPRB) CYCLE

  !     Wind speed dependence: Monahan (1986) / Gong (2003)
  !     Conversion from #/cm2 to #/m2: norm = 1e4
  ZEMIS_FAC = 1.0E4_JPRB * ZAREA_FRAC * PWIND(JL)**3.41_JPRB

  ! --- Accumulation mode (#/m2/s) ---
  ZNUM_ACS = ZEMIS_FAC * ZPREFAC_ACS

  !     SST scaling for small particles (Salter et al. 2015, T_ref=15C)
  ZTT = MAX(-1.0_JPRB, PSST(JL) - 273.15_JPRB)
  IF (ZTT < 15.0_JPRB) THEN
    ZT_SCALE = -8.75593266E-5_JPRB*ZTT**3 + 5.56770771E-3_JPRB*ZTT**2 &
             & - 0.116695696_JPRB*ZTT + 1.79321393_JPRB
    ZNUM_ACS = ZNUM_ACS * ZT_SCALE
  ENDIF

  !     Mass flux = N * (4/3)pi * r^3 * exp(9/2*ln^2(sigma)) * rho
  !     with r in cm (um*1e-4), rho in g/cm3, result in kg/m2/s (*1e-3)
  ZMASS_ACS = ZNUM_ACS * RPI * 4.0_JPRB/3.0_JPRB &
            & * (ZRG1_UM*1.0E-4_JPRB)**3 &
            & * EXP(4.5_JPRB * LOG(ZSIGMA_ACS)**2) &
            & * ZDENS_CGS * 1.0E-3_JPRB

  ! --- Coarse mode (#/m2/s) ---
  ZNUM_COS = ZEMIS_FAC * ZPREFAC_COS

  !     SST scaling for large particles (Salter et al. 2015, linear, T_ref=15C)
  ZTT = MIN(30.0_JPRB, MAX(-1.0_JPRB, PSST(JL) - 273.15_JPRB))
  ZT_SCALE = 0.03752944_JPRB*ZTT + 0.43705846_JPRB
  ZNUM_COS = ZNUM_COS * ZT_SCALE

  ZMASS_COS = ZNUM_COS * RPI * 4.0_JPRB/3.0_JPRB &
            & * (ZRG2_UM*1.0E-4_JPRB)**3 &
            & * EXP(4.5_JPRB * LOG(ZSIGMA_COS)**2) &
            & * ZDENS_CGS * 1.0E-3_JPRB

  !     Total SS mass emission = sum of both modes
  PEMISS_MASS(JL) = (ZMASS_ACS + ZMASS_COS) * ZSSCAL

ENDDO

IF (LHOOK) CALL DR_HOOK('HAMLITE_SRC_SS',1,ZHOOK_HANDLE)
END SUBROUTINE HAMLITE_SRC_SS
