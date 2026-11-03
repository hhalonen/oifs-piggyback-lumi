SUBROUTINE HAMLITE_INTERFACE( &
 & YDMODEL,   KIDIA,     KFDIA,     KLON,        KTDIA,    KLEV,     KTILES,  &
 & KFLDX,     KLEVX,     KTRAC,     KAERO,       KCHEM,    KSTGLO,   PGEOH,   &
 & PRS1,      PRSF1,     PAEROP,    PCAERO,      PCEN,     PAPHIF,            &
 & PFPLCL,    PFPLCN,    PFPLSL,    PFPLSN,      PGELAT,   PGELAM,            &
 & PAP,       PIP,       PLP,       PRP,         PSP,      PCOVPTOT,          &
 & PLU,       PMFU,      PO3P,      PQP,         PTP,      PTHP,     PTENC,   PCFLX,  &
 & PAERDDP,   PAERSDM,   PAERSRC,   PAERWS,      PAERGUST, PAERUST,  PAERMAP, &
 & PCLAERS,   PPRAERS,   PCHEM2AER,                                           &
 & PFRTI,     PLSM,      PSNS,      PWND,        PWS1,     PAERLIF,           &
 & PAERODDF,  PTSPHY,    PGFL,                                                &
 & PODTO,     PAERO_WVL_DIAG,                                                 &
 & PAER_TAU,  PAER_SSA,  PAER_ASYM, PAER_TAU_LW,                              &
 & PTAUS_AER, PTAUA_AER, PPMAER,                                              &
 & PEXTRA,    PVERVEL,   PCCNL,     PCCNO,       PAHFSTI,  PCI,      PZ0M,    &
 & PAHFLEV,   PUP,       PVP,       PCVL,        PCVH,     PSO2DD,   PGEMU,   &
 & PBLH,      PSNOWACL,  KCTOP,     KCBOT,                                    &
 & PAEPM1,    PAEPM25,   PAEPM10,   PKH, PKM, PLAIL, PLAIH)

! -----------------------------------------------------------------------
! Purpose:
!   Interface routine between OpenIFS and HAMlite aerosol scheme.
!   Replaces HAMM7_INTERFACE when AERO_SCHEME="hamlite".
!
!   Steps:
!   1. Prepare atmospheric state (density, RH, layer thickness, etc.)
!   2. Map OpenIFS GFL tracers to HAMlite (native: 4 number + 4 mass)
!   3. Call HAMlite physics (activation, wet dep, sedimentation)
!   4. Map HAMlite tendencies back to PTENC + proportional mass tendencies
!   5. Compute optical properties for radiation scheme
!   6. Compute PM diagnostics
!
! Interface:
!   Called from AER_PHY3_LAYER (same signature as HAMM7_INTERFACE)
!
! Author: Arup Sarkar (FMI) 
! -----------------------------------------------------------------------

USE PARKIND1,     ONLY: JPIM, JPRB
USE YOMHOOK,      ONLY: LHOOK, DR_HOOK, JPHOOK
USE TYPE_MODEL,   ONLY: MODEL
USE YOMCST,       ONLY: RD, RG, RPI
USE YOMLUN,       ONLY: NULOUT

! HAMlite modules
!USE mo_kind_lite,  ONLY: JPRB
USE mo_ham_lite_main, ONLY: nclass, sizeclass

!eehol: these logicals are controlled by mo_subm_main_list_config
USE mo_subm_main_lite, ONLY: lemissions, & 
                       ldrydep, &
                       lwetdep, &
                       lsedimentation, &
                       lradiation

! OIFS to HAMlite index mapping
USE OIFS_TO_HAMLITE, ONLY: ind_oifs_hamlite, &
   & NHLGAS, NHLCLOUD

! HAMlite submodel interface (the physics entry points)
USE mo_subm_interface_lite, ONLY: &
   & cloud_mig_subm_1,  &
   & cloud_mig_subm_2,  &
   & radiation_subm_1,  &
   & radiation_subm_2,  &
   & interface_tmx_subm,&
   & aes_phy_main_subm

USE mo_ham_lite_misc, ONLY: pm6rp,prhop,prelhum,ham_lite_mprops

USE TM5_PHOTOLYSIS, ONLY: NBANDS_TROP

USE mo_xt_borrow_lite, ONLY: xt_borrow_lite

USE YOMCT0,       ONLY: LIFSMIN, LIFSTRAJ
USE mo_ham_lite_main,          ONLY: sigma_mode,sizeclass
USE mo_subm_spec_lite,             ONLY: speclist

IMPLICIT NONE

TYPE(MODEL),        INTENT(IN) :: YDMODEL
INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA, KFDIA, KLON
INTEGER(KIND=JPIM), INTENT(IN) :: KTDIA, KLEV, KFLDX, KLEVX
INTEGER(KIND=JPIM), INTENT(IN) :: KTILES
INTEGER(KIND=JPIM), INTENT(IN) :: KTRAC
INTEGER(KIND=JPIM), INTENT(IN) :: KAERO(YDMODEL%YRML_GCONF%YGFL%NAERO)
INTEGER(KIND=JPIM), INTENT(IN) :: KCHEM(YDMODEL%YRML_GCONF%YGFL%NCHEM)
INTEGER(KIND=JPIM), INTENT(IN) :: KSTGLO
INTEGER(KIND=JPIM), INTENT(IN) :: KCTOP(KLON)
INTEGER(KIND=JPIM), INTENT(IN) :: KCBOT(KLON)

REAL(KIND=JPRB),INTENT(IN)    :: PGEOH(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PRSF1(KLON,KLEV), PRS1(KLON,0:KLEV), PAPHIF(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PAP(KLON,KLEV), PIP(KLON,KLEV), PLP(KLON,KLEV), PLU(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PRP(KLON,KLEV), PSP(KLON,KLEV), PCOVPTOT(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PAEROP(KLON,KLEV,YDMODEL%YRML_GCONF%YGFL%NACTAERO)
REAL(KIND=JPRB),INTENT(IN)    :: PCAERO(KLON,KLEV,YDMODEL%YRML_GCONF%YGFL%NACTAERO)
REAL(KIND=JPRB),INTENT(IN)    :: PCEN(KLON,KLEV,KTRAC), PCFLX(KLON,KTRAC)
REAL(KIND=JPRB),INTENT(IN)    :: PO3P(KLON,KLEV), PQP(KLON,KLEV), PTP(KLON,KLEV), PTHP(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PFPLCL(KLON,0:KLEV),PFPLCN(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PFPLSL(KLON,0:KLEV),PFPLSN(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PGELAT(KLON), PGELAM(KLON)
REAL(KIND=JPRB),INTENT(IN)    :: PFRTI(KLON,KTILES)
REAL(KIND=JPRB),INTENT(IN)    :: PAERWS(KLON), PAERGUST(KLON), PAERUST(KLON), PAERMAP(KLON,5)
REAL(KIND=JPRB),INTENT(IN)    :: PAERSRC(KLON,YDMODEL%YRML_GCONF%YGFL%NACTAERO)
REAL(KIND=JPRB),INTENT(IN)    :: PCHEM2AER(KLON,KLEV,*)
REAL(KIND=JPRB),INTENT(IN)    :: PAERLIF(KLON,9), PCLAERS(KLON)
REAL(KIND=JPRB),INTENT(IN)    :: PLSM(KLON), PSNS(KLON), PWND(KLON), PWS1(KLON)
REAL(KIND=JPRB),INTENT(IN)    :: PTSPHY
REAL(KIND=JPRB),INTENT(IN)    :: PVERVEL(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PCCNL(KLON), PCCNO(KLON)
REAL(KIND=JPRB),INTENT(IN)    :: PAHFSTI(KLON,KTILES)
REAL(KIND=JPRB),INTENT(IN)    :: PCI(KLON), PZ0M(KLON), PAHFLEV(KLON)
REAL(KIND=JPRB),INTENT(IN)    :: PUP(KLON,KLEV), PVP(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PCVL(KLON), PCVH(KLON)
REAL(KIND=JPRB),INTENT(IN)    :: PGEMU(KLON), PMFU(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PSNOWACL(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PBLH(KLON)
REAL(KIND=JPRB),INTENT(IN)    :: PLAIL(KLON)       ! LOW VEGETATION LAI  
REAL(KIND=JPRB),INTENT(IN)    :: PLAIH(KLON)       ! HIGH VEGETATION LAI
REAL(KIND=JPRB),INTENT(IN)    :: PKH(KLON,KLEV)    ! Exchange coefficient for heat
REAL(KIND=JPRB),INTENT(IN)    :: PKM(KLON,KLEV)    ! Exchange coefficient for momentum

REAL(KIND=JPRB),INTENT(INOUT) :: PTENC(KLON,KLEV,KTRAC)
REAL(KIND=JPRB),INTENT(INOUT) :: PAERDDP(KLON,YDMODEL%YRML_GCONF%YGFL%NACTAERO)
REAL(KIND=JPRB),INTENT(INOUT) :: PAERSDM(KLON,YDMODEL%YRML_GCONF%YGFL%NACTAERO)
REAL(KIND=JPRB),INTENT(OUT)   :: PODTO(KLON)
REAL(KIND=JPRB),INTENT(INOUT) :: PAERO_WVL_DIAG(KLON,YDMODEL%YRML_GCONF%YGFL%NAERO_WVL_DIAG,YDMODEL%YRML_GCONF%YGFL%NAERO_WVL_DIAG_TYPES)
REAL(KIND=JPRB),INTENT(OUT)   :: PAERODDF(KLON,YDMODEL%YRML_GCONF%YGFL%NACTAERO,8)
REAL(KIND=JPRB),INTENT(INOUT) :: PEXTRA(KLON,KLEVX,KFLDX)
REAL(KIND=JPRB),INTENT(INOUT) :: PAER_TAU(KLON,KLEV,14), PAER_SSA(KLON,KLEV,14), PAER_ASYM(KLON,KLEV,14)
REAL(KIND=JPRB),INTENT(INOUT) :: PAER_TAU_LW(KLON,KLEV,16)
REAL(KIND=JPRB),INTENT(OUT)   :: PTAUS_AER(KLON,KLEV,NBANDS_TROP,2), PTAUA_AER(KLON,KLEV,NBANDS_TROP,2)
REAL(KIND=JPRB),INTENT(OUT)   :: PPMAER(KLON,KLEV,NBANDS_TROP,2)
REAL(KIND=JPRB),INTENT(INOUT) :: PGFL(KLON,KLEV,YDMODEL%YRML_GCONF%YGFL%NDIM)
REAL(KIND=JPRB),INTENT(INOUT) :: PPRAERS(KLON)
REAL(KIND=JPRB),INTENT(INOUT) :: PSO2DD(KLON)
REAL(KIND=JPRB),INTENT(OUT)   :: PAEPM1(KLON), PAEPM25(KLON), PAEPM10(KLON)

!*   LOCAL VARIABLES
INTEGER(KIND=JPIM) :: JL, JK, JM, JB, JEXT, ITRC,IW
INTEGER(KIND=JPIM) :: JO, JAER
INTEGER(KIND=JPIM) :: IFLAG, ISURF
REAL(KIND=JPRB)    :: ZTAU_TINY, ZT, ZPGFL_STATE

INTEGER, PARAMETER :: &
	   KSURF_LITE = 4, & ! number of surface types (land/water/ice/vegetation)
      kbndlw = 16, &
      kbndsw = 14

! HAMlite internal arrays (working precision = JPRB = dp)
REAL(KIND=JPRB) :: ZXTM1(KLON,KLEV,NCLASS)
REAL(KIND=JPRB) :: ZXTTE(KLON,KLEV,NCLASS)
REAL(KIND=JPRB) :: ZAOD_MODE(KLON,KLEV,NCLASS)

! Mass tracer arrays (native tracers)
REAL(KIND=JPRB) :: ZXTM1_MASS(KLON,KLEV,NCLASS)
REAL(KIND=JPRB) :: ZXTTE_MASS(KLON,KLEV,NCLASS)
REAL(KIND=JPRB) :: ZFRAC

! Derived atmospheric variables
REAL(KIND=JPRB) :: ZRHO(KLON,KLEV)
REAL(KIND=JPRB) :: ZDP(KLON,KLEV)
REAL(KIND=JPRB) :: ZDZ(KLON,KLEV)
REAL(KIND=JPRB) :: ZRH(KLON,KLEV)
REAL(KIND=JPRB) :: ZQSAT(KLON,KLEV)
REAL(KIND=JPRB) :: ZQP(KLON,KLEV)
REAL(KIND=JPRB) :: ZDPG(KLON,KLEV)
REAL(KIND=JPRB) :: ZCDNC(KLON,KLEV)
REAL(KIND=JPRB) :: ZCDNCACT(KLON,KLEV)     !number of activated particles [m-3]
REAL(KIND=JPRB) :: ZWA(KLON,KLEV)
REAL(KIND=JPRB) :: ZESW(KLON,KLEV)

REAL(KIND=JPRB) :: &
       ZLAI(KLON), &      ! total vegetation leaf area index
       KBAND_LW  (2,kbndlw),           & ! limits of longwave bands [1/cm] - RRTMGP
       KBAND_SW  (2,kbndsw)!,           & ! limits of shortwave bands [1/cm] - RRTMGP
       !aer_tau_sw_vr (KLON,KLEV,kbndsw), & ! SW aerosol optical depth
       !aer_ssa_sw_vr (KLON,KLEV,kbndsw), & ! SW aerosol single scattering albedo
       !aer_asy_sw_vr (KLON,KLEV,kbndsw), & ! SW aerosol asymmetry factor
       !aer_tau_lw_vr (KLON,KLEV,kbndlw)   ! LW aerosol optical depth

REAL(KIND=JPRB) :: PAOD(KLON,kbndsw), PSSA(KLON,kbndsw), PASY(KLON,kbndsw)
REAL(KIND=JPRB) :: PAOD_LW(KLON,16)

REAL(KIND=JPRB) :: &
       ZMAIR         (KLON,KLEV),      & ! air mass [kg/m2] 
       ZXLQG         (KLON,KLEV),      & ! graupel [kg/kg]
       ZXPRI         (KLON),           & ! precip rate ice [kg/m2/s] 
       ZXPRG         (KLON),           & ! precip rate graupel [kg/m2/s] 
       ZGLAC         (KLON),           & ! fraction of land covered by glaciers
       ZALAKE        (KLON),           & ! lake fraction
       ZSFCWIND      (KLON),           & ! 10m windspeed
       ZFRC_TILE     (KLON,KSURF_LITE),& ! area fraction of each surface type (land/water/ice)
       ZZ0M_TILE     (KLON,KSURF_LITE),& ! surface roughness length
       ZTSFC_TILE    (KLON,KSURF_LITE),& ! surface temperature  
       ZSFCWIND_TILE (KLON,KSURF_LITE),& ! 10m windspeed          
       ZKH_TILE      (KLON,KSURF_LITE),& ! exchange coefficient for scalar 
       ZKM_TILE      (KLON,KSURF_LITE),& ! exchange coefficient for momentum 
       ZZF           (KLON,KLEV),      & ! geometric height above sea level, full level [m] 
       ZZH           (KLON,KLEV+1)       ! geometric height above sea level, half level [m]

! Tendencies
REAL(KIND=JPRB) :: ZFLUX_WDEP(KLON,NCLASS)
REAL(KIND=JPRB) :: ZFLUX_SEDI(KLON,NCLASS)
REAL(KIND=JPRB) :: ZFLUX_DDEP(KLON,NCLASS)
REAL(KIND=JPRB) :: ZTEND_TOT(KLON,KLEV,NCLASS)
REAL(KIND=JPRB) :: ZTEND(KLON,KLEV,NCLASS)
REAL(KIND=JPRB) :: ZXTM0(KLON,KLEV,NCLASS)

! Dummy arrays for HAMlite
REAL(KIND=JPRB) :: ZRWET(KLON,KLEV,1,NCLASS)
REAL(KIND=JPRB) :: ZRHOP(KLON,KLEV,1,NCLASS)

! Full-profile precipitation rates for wet deposition
REAL(KIND=JPRB) :: ZPRECIP_RAIN(KLON)
REAL(KIND=JPRB) :: ZPRECIP_ICE(KLON)
REAL(KIND=JPRB) :: ZPRECIP_SNOW(KLON)

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
REAL(KIND=JPRB) :: ZTMP(KLON)
#include "abor1.intfb.h"
#include "satur.intfb.h"
#include "aer_negat.intfb.h"

IF (LHOOK) CALL DR_HOOK('HAMLITE_INTERFACE',0,ZHOOK_HANDLE)

ASSOCIATE( &
         & YGFL         => YDMODEL%YRML_GCONF%YGFL,        &
         & YDPHYRAD     => YDMODEL%YRML_PHY_RAD,           &
         & YDPHYAER     => YDMODEL%YRML_PHY_AER,           &
         & YRERAD       => YDMODEL%YRML_PHY_RAD%YRERAD,    &
         & YREAERATM    => YDMODEL%YRML_PHY_RAD%YREAERATM, &
         & YDAERM7      => YDMODEL%YRML_PHY_AER%YREAEROPT, &              
         & TIME_STEP_LEN => YDMODEL%YRML_GCONF%YRRIP%TSTEP )

ASSOCIATE( &
         & NACTAERO      => YGFL%NACTAERO, &
         & YAEROUT       => YGFL%YAEROUT,  &
         & NAERO         => YGFL%NAERO,    &
         & NGHG          => YGFL%NGHG,     &
         & NCHEM         => YGFL%NCHEM,    &
         & NDIM          => YGFL%NDIM,     &
         & YAERO         => YGFL%YAERO,    &
         & YCHEM         => YGFL%YCHEM,    &
         & YGHG          => YGFL%YGHG,     &
         & YCDNC         => YGFL%YCDNC,    &
         & YICNC         => YGFL%YICNC,    &
         & LAERSCAV      => YREAERATM%LAERSCAV, &
         & LAERSEDIM     => YREAERATM%LAERSEDIM, &
         & LAERNGAT      => YREAERATM%LAERNGAT,  &
         & LAERRRTM      => YREAERATM%LAERRRTM,  &
         & NTSW          => YRERAD%NTSW           )

! ======================================================================
! 0. INITIALIZATION
! ======================================================================
PODTO(KIDIA:KFDIA) = 0._JPRB
PAEPM1(KIDIA:KFDIA) = 0._JPRB
PAEPM25(KIDIA:KFDIA) = 0._JPRB
PAEPM10(KIDIA:KFDIA) = 0._JPRB
PAERODDF(KIDIA:KFDIA,:,:) = 0._JPRB

DO JB = 1, NBANDS_TROP
   PTAUS_AER(KIDIA:KFDIA,:,JB,:) = 0._JPRB
   PTAUA_AER(KIDIA:KFDIA,:,JB,:) = 0._JPRB
   PPMAER   (KIDIA:KFDIA,:,JB,:) = 0._JPRB
END DO

! ======================================================================
! 1. COMPUTE ATMOSPHERIC STATE VARIABLES
! ======================================================================

!---Allocate
IF (.NOT. ALLOCATED(pm6rp)) ALLOCATE(pm6rp(KLON,KLEV,nclass))
IF (.NOT. ALLOCATED(prhop)) ALLOCATE(prhop(KLON,KLEV,nclass))
IF (.NOT. ALLOCATED(prelhum)) ALLOCATE(prelhum(KLON,KLEV))
pm6rp(:,:,:) = 0.0_JPRB
prhop(:,:,:) = 0.0_JPRB
prelhum(:,:) = 0.0_JPRB

! Air density, layer thickness, dp/g
DO JK = 1, KLEV
   DO JL = KIDIA, KFDIA
      ZRHO(JL,JK) = PRSF1(JL,JK) / (RD * PTP(JL,JK))
      ZDP(JL,JK)  = PRS1(JL,JK) - PRS1(JL,JK-1)
      ZDPG(JL,JK) = ZDP(JL,JK) / RG
      ZDZ(JL,JK)  = ZDP(JL,JK) / (ZRHO(JL,JK) * RG)
      ZMAIR(JL,JK) = ZRHO(JL,JK)*ZDZ(JL,JK)
   END DO
END DO

! initialize
ZFLUX_DDEP(:,:) = 0._JPRB
ZFLUX_WDEP(:,:) = 0._JPRB
ZFLUX_SEDI(:,:) = 0._JPRB
ZTEND_TOT(:,:,:)  = 0._JPRB
ZTEND(:,:,:) = 0._JPRB
ZCDNCACT(:,:) = 0._JPRB

ZXLQG(:,:) = 0._JPRB
ZGLAC(:) = 0._JPRB
ZALAKE(:) = 0._JPRB
ZXPRG(:) = 0._JPRB

! Half level height
ZZH(KIDIA:KFDIA, KLEV+1) = 0._JPRB
DO JK = KLEV, 1, -1
   ZZH(KIDIA:KFDIA, JK) = ZZH(KIDIA:KFDIA, JK+1) + ZDZ(KIDIA:KFDIA, JK)
END DO
! full level height
DO JK = 1, KLEV
   ZZF(KIDIA:KFDIA, JK) = 0.5_JPRB * (ZZH(KIDIA:KFDIA, JK) + ZZH(KIDIA:KFDIA, JK+1))
END DO

! Relative humidity
IFLAG = 2
CALL SATUR(KIDIA, KFDIA, KLON, KTDIA, KLEV, &
   & YDMODEL%YRML_PHY_SLIN%YREPHLI%LPHYLIN, &
   & PRSF1, PTP, ZQSAT, IFLAG)

DO JK = 1, KLEV
   DO JL = KIDIA, KFDIA
      ZQP(JL,JK) = MAX(0._JPRB, PQP(JL,JK))
      ZRH(JL,JK)=ZQP(JL,JK)/(MAX(1.E-30_JPRB,ZQSAT(JL,JK)))
      ZRH(JL,JK)=MIN(1.0_JPRB,MAX(0.0_JPRB,ZRH(JL,JK)))
      !ZRH(JL,JK) = MIN(1._JPRB, MAX(0._JPRB, ZQP(JL,JK) / MAX(1.E-30_JPRB, ZQSAT(JL,JK))))
   END DO
END DO

! Vertical velocity: convert from Pa/s to m/s (positive upward)
DO JK = 1, KLEV
   DO JL = KIDIA, KFDIA
      ZWA(JL,JK) = -PVERVEL(JL,JK) / (ZRHO(JL,JK) * RG)
   END DO
END DO

! Saturation vapour pressure (for activation)
DO JK = 1, KLEV
   DO JL = KIDIA, KFDIA
      ZESW(JL,JK) = ZQSAT(JL,JK) * PRSF1(JL,JK) / 0.622_JPRB
   END DO
END DO

! CDNC from GFL (convert #/kg to #/m3)
ZCDNC(KIDIA:KFDIA,1:KLEV) = 0._JPRB
IF (YCDNC%LACTIVE) THEN
   DO JK = 1, KLEV
      DO JL = KIDIA, KFDIA
         ZCDNC(JL,JK) = MAX(0._JPRB, PGFL(JL,JK,YCDNC%MP9_PH)) * ZRHO(JL,JK) * 1.0E6_JPRB
      END DO
   END DO
END IF
! ======================================================================
! 2. MAP OPENIFS TRACERS TO HAMLITE (native class tracers, no lumping)
! ======================================================================
ZXTM1(KIDIA:KFDIA,1:KLEV,:) = 0._JPRB
ZXTTE(KIDIA:KFDIA,1:KLEV,:) = 0._JPRB
DO JM = 1, NCLASS
   JO = ind_oifs_hamlite%ind_class_OIFS(JM)
   IF (JO > 0) THEN
      DO JK = 1, KLEV
         DO JL = KIDIA, KFDIA
            ZPGFL_STATE = PGFL(JL,JK,YAERO(JO)%MP9_PH) !arups changes (state of the tracer num concentration)
            PTENC(JL,JK,KAERO(JO)) = MAX(PTENC(JL,JK,KAERO(JO)), &
               & -ZPGFL_STATE / TIME_STEP_LEN) !arups changes (threshold tendency so num concentration can not be negative)
            ZXTM1(JL,JK,JM) = MAX(0.0_JPRB,PCEN(JL,JK,KAERO(JO)))
            ZXTTE(JL,JK,JM) = PTENC(JL,JK,KAERO(JO))
         END DO
      END DO
   END IF
END DO

! --- Mass tracers (DU_M, SS_M, CA_M, SU_M) ---
ZXTM1_MASS(:,:,:) = 0._JPRB
DO JM = 1, NCLASS
   JO = ind_oifs_hamlite%ind_mass_OIFS(JM)
   IF (JO > 0) THEN
      DO JK = 1, KLEV
         DO JL = KIDIA, KFDIA
            ZPGFL_STATE = PGFL(JL,JK,YAERO(JO)%MP9_PH) !arups changes (mass)
            PTENC(JL,JK,KAERO(JO)) = MAX(PTENC(JL,JK,KAERO(JO)), &
               & -ZPGFL_STATE / TIME_STEP_LEN) !arups changes (mass)
            ZXTM1_MASS(JL,JK,JM) = MAX(0.0_JPRB,PCEN(JL,JK,KAERO(JO)))
            ZXTTE_MASS(JL,JK,JM) = PTENC(JL,JK,KAERO(JO)) !arups changes (mass)
         END DO
      END DO
   END IF
END DO

! ======================================================================
! 3. CALL HAMLITE PHYSICS
! ======================================================================

CALL ham_lite_mprops(1, 1, KIDIA, KFDIA, KLON, KLEV, PTP, PRSF1, ZQP) !eehol: aerosol rwet and density calculations (relhum also)

ZRWET(KIDIA:KFDIA,1:KLEV,1,1:NCLASS) = pm6rp(KIDIA:KFDIA,1:KLEV,1:NCLASS)
ZRHOP(KIDIA:KFDIA,1:KLEV,1,1:NCLASS) = prhop(KIDIA:KFDIA,1:KLEV,1:NCLASS)
prelhum(KIDIA:KFDIA,1:KLEV) = ZRH(KIDIA:KFDIA,1:KLEV) !eehol: add rel hum from OIFS

! --- 3a. Activation (Abdul-Razzak & Ghan) ---
CALL cloud_mig_subm_1( &
   & 1, 1, KIDIA, KFDIA, 1, KLEV, KLON, NCLASS, &
   & ZDZ, ZRHO, ZWA, &
   & PTP, PRSF1, ZQP, &
   & PLP, PIP, &
   & ZXTM1, ZCDNCACT, ZESW )

! --- 3b. Wet deposition ---
IF ( lwetdep ) THEN

   ! Expand half-level precip fluxes to full-level 2D arrays
   ! PFPLSL/PFPLSN are at half-levels (0:KLEV); use value at bottom of each layer
   ! precip fluxes from surface
   !ZPRECIP_RAIN(KIDIA:KFDIA) = MAX(0._JPRB, PFPLSL(KIDIA:KFDIA,KLEV))
   !ZPRECIP_ICE(KIDIA:KFDIA)  = 0._JPRB
   !ZPRECIP_SNOW(KIDIA:KFDIA) = MAX(0._JPRB, PFPLSN(KIDIA:KFDIA,KLEV))
   
   ZPRECIP_RAIN(KIDIA:KFDIA) = 0._JPRB
   ZPRECIP_ICE(KIDIA:KFDIA)  = 0._JPRB
   ZPRECIP_SNOW(KIDIA:KFDIA) = 0._JPRB
   ! precip fluxes column integrated
	!DO JK = 1, KLEV
	!  ZPRECIP_RAIN(KIDIA:KFDIA) = ZPRECIP_RAIN(KIDIA:KFDIA) + PFPLSL(KIDIA:KFDIA,JK)
    !  ZPRECIP_ICE(KIDIA:KFDIA)  = ZPRECIP_ICE(KIDIA:KFDIA)  + PFPLSN(KIDIA:KFDIA,JK)
    !  ZPRECIP_SNOW(KIDIA:KFDIA) = ZPRECIP_SNOW(KIDIA:KFDIA) + PFPLSN(KIDIA:KFDIA,JK)
	!END DO
	    
	ZPRECIP_RAIN(KIDIA:KFDIA) = ZPRECIP_RAIN(KIDIA:KFDIA) + PFPLSL(KIDIA:KFDIA,KLEV)
    ZPRECIP_ICE(KIDIA:KFDIA)  = ZPRECIP_ICE(KIDIA:KFDIA)  + PFPLSN(KIDIA:KFDIA,KLEV)
    ZPRECIP_SNOW(KIDIA:KFDIA) = ZPRECIP_SNOW(KIDIA:KFDIA) + PFPLSN(KIDIA:KFDIA,KLEV)

   CALL cloud_mig_subm_2( &
      & 1, 1, KIDIA, KFDIA, 1, KLEV, KLON, NCLASS, &
      & TIME_STEP_LEN, &
      & ZRHO, PTP, ZDPG, &
      & PLP, PIP, PRP, &
      & PSP, ZXLQG, &
      & ZPRECIP_RAIN, ZPRECIP_ICE, ZPRECIP_SNOW, ZXPRG, &
      & ZXTM1, ZTEND, ZFLUX_WDEP )

   ZTEND_TOT(KIDIA:KFDIA,1:KLEV,:) = ZTEND_TOT(KIDIA:KFDIA,1:KLEV,:) + &
      & ZTEND(KIDIA:KFDIA,1:KLEV,:)

   CALL xt_borrow_lite(KFDIA, KLON,  KLEV, KLEV+1, NCLASS, &
        PRSF1, PRS1, ZXTM1, ZTEND_TOT, TIME_STEP_LEN)

END IF

! --- 3c. Sedimentation ---
IF ( lsedimentation ) THEN

   CALL aes_phy_main_subm( &
      & 1, 1, KIDIA, KFDIA, KLON, KLEV, KLEV+1, NCLASS, &
      & TIME_STEP_LEN, &
      & ZDZ, PTP, &
      & ZDPG, ZRHO, &
      & PRSF1, PRS1(KIDIA:KFDIA,0:KLEV), &
      & ZQP, ZXTM1, ZTEND, ZRWET, ZRHOP, ZFLUX_SEDI )

   ZTEND_TOT(KIDIA:KFDIA,1:KLEV,:) = ZTEND_TOT(KIDIA:KFDIA,1:KLEV,:) + &
      & ZTEND(KIDIA:KFDIA,1:KLEV,:)

   CALL xt_borrow_lite(KFDIA, KLON,  KLEV, KLEV+1, NCLASS, &
        PRSF1, PRS1, ZXTM1, ZTEND_TOT, TIME_STEP_LEN)

END IF

!-------Start dry deposition------------------------
!---Interface with TMX (emissions and drydep but emissions come from host model!)

IF ( ldrydep ) THEN
 
   ! Meteorology and other values for dry deposition
   DO JL=KIDIA,KFDIA
      ZFRC_TILE(JL,1) = PLSM(JL)!PCVL(JL)                 ! Land area fraction
      ZFRC_TILE(JL,2) = MAX(0.0_JPRB,1.0_JPRB-PLSM(JL)-PCI(JL)) ! water fraction = 1 - land mask - sea ice fraction
      ZFRC_TILE(JL,3) = PCI(JL)                  ! Ice area fraction
      ZFRC_TILE(JL,4) = PCVH(JL)+PCVL(JL)                 ! Vegetation area fraction

      ZSFCWIND(JL) = PWND(JL)                              ! Surface wind

      DO ISURF = 1,KSURF_LITE
         ZZ0M_TILE(JL, ISURF) = MAX(1.0E-5_JPRB,PZ0M(JL))!*ZFRC_TILE(JL, ISURF))! Surface roughness length [m]
         ZSFCWIND_TILE(JL, ISURF) = PWND(JL)!*ZFRC_TILE(JL, ISURF)
         ZKH_TILE(JL, ISURF) = PKH(JL, KLEV)/PWND(JL)!*ZFRC_TILE(JL, ISURF)! Horizontal exchange coefficient for momentum in 3D turb.
         ZKM_TILE(JL, ISURF) = ((PAERUST(JL)/PWND(JL))**(2._JPRB))!*ZFRC_TILE(JL,ISURF) !inverse operation done in mo_subm_interface_lite to get ustar
         !zkm_tile(JL, ISURF) = PKM(JL, KLEV)!*zfrc_tile(JL, ISURF)! Horizontal exchange coefficient for heat in 3D turb.
         ZTSFC_TILE(JL, ISURF) = PTP(JL, KLEV) ! surface temperature (type)
      END DO
   END DO

   ZLAI(KIDIA:KFDIA) = PLAIH(KIDIA:KFDIA)*PCVH(KIDIA:KFDIA) + &! Leaf area index zlai(KIDIA:KFDIA) = 
         PLAIL(KIDIA:KFDIA)*PCVL(KIDIA:KFDIA)                  ! zlaih(KIDIA:KFDIA)*zfrc_tile(KIDIA:KFDIA,4) + 
                                                               ! zlail(KIDIA:KFDIA)*(1-zfrc_tile(KIDIA:KFDIA,4))
                                                               ! Weighted sum of low and high vegetation values
                                                               ! Weighted sum of low and high vegetation values

   CALL interface_tmx_subm(1, 1, KIDIA, KFDIA,         &
                       KLON, KLEV, KLEV+1, NCLASS,       &
                       KSURF_LITE, 2, 3, 1, &
                       TIME_STEP_LEN,         &
                       PLSM, zglac, zalake,        &
                       PCI, zsfcWind,              &
                       PGELAM, PGELAT,   &
                       ZFRC_TILE, ZZ0M_TILE,                 &
                       ZTSFC_TILE, ZSFCWIND_TILE,            &
                       ZKH_TILE, ZKM_TILE, PAERWS, &
                       ZZF, ZZH, ZDZ,                        &
                       ZMAIR, PRSF1, PRS1(KIDIA:KFDIA,0:KLEV),               &
                       PTP, ZRHO, ZQP,                       &
                       ZXTM1,                             &
                       ZTEND,ZRWET,ZRHOP,PCVH,ZLAI,ZFLUX_DDEP)                    
   
   ZTEND_TOT(KIDIA:KFDIA,1:KLEV,:) = ZTEND_TOT(KIDIA:KFDIA,1:KLEV,:) + &
      ZTEND(KIDIA:KFDIA,1:KLEV,:) !eehol: add tendency to total tendency

   CALL xt_borrow_lite(KFDIA, KLON,  KLEV, KLEV+1, NCLASS, &
        PRSF1, PRS1, ZXTM1, ZTEND_TOT, TIME_STEP_LEN)
 
END IF
!-------End dry deposition------------------------


!-------Start radiation calculations------------------------
!---Radiation
IF ( lradiation ) THEN
   ZXTM0(KIDIA:KFDIA,1:KLEV,:) = ZXTM1(KIDIA:KFDIA,1:KLEV,:) + ZTEND_TOT(KIDIA:KFDIA,1:KLEV,:)*TIME_STEP_LEN
   CALL radiation_subm_1(1, 1, KIDIA, KFDIA,                                           &
                     KLON, KLEV, NCLASS, NCLASS, ZQP, 1,                     &
                     kbndlw, kbndsw, kband_lw, kband_sw,                         &
                     ZXTM0, zmair, PTP, PRSF1,                               &                                                         
                     PAER_TAU, PAER_SSA, PAER_ASYM, PAER_TAU_LW)
                     
  !eehol: radiation_subm_2 is basically just diagnostics but includes cache cleanup so I included it here but removed the diagnostics
   CALL radiation_subm_2(1, 1, KIDIA, KFDIA,                                                 &
                     KLON, KLEV, kbndlw, kbndsw,                                     & 
                     ZDZ, PAER_TAU, PAER_SSA, PAER_ASYM, PAER_TAU_LW)

                  

END IF
!-------End radiation calculations------------------------

! ======================================================================
! 4. MAP HAMLITE TENDENCIES BACK TO OPENIFS PTENC (native classes)
! ======================================================================

ZXTTE(KIDIA:KFDIA,1:KLEV,:) = ZXTTE(KIDIA:KFDIA,1:KLEV,:) + &
   & ZTEND_TOT(KIDIA:KFDIA,1:KLEV,:)

CALL xt_borrow_lite(KFDIA, KLON,  KLEV, KLEV+1, NCLASS, &
     PRSF1, PRS1, ZXTM1, ZXTTE, TIME_STEP_LEN)

!arups add to tendency
DO JM = 1, NCLASS
   JO = ind_oifs_hamlite%ind_class_OIFS(JM)
   IF (JO > 0) THEN
      DO JK = 1, KLEV
         DO JL = KIDIA, KFDIA
            ZPGFL_STATE = PGFL(JL,JK,YAERO(JO)%MP9_PH)
            PTENC(JL,JK,KAERO(JO)) = MAX(ZXTTE(JL,JK,JM), &
               & -ZPGFL_STATE / TIME_STEP_LEN)
         END DO
      END DO
   END IF
END DO

DO JM = 1, NCLASS
   JO = ind_oifs_hamlite%ind_mass_OIFS(JM)
   IF (JO > 0) THEN
      DO JK = 1, KLEV
         DO JL = KIDIA, KFDIA
            ZPGFL_STATE = PGFL(JL,JK,YAERO(JO)%MP9_PH)
            PTENC(JL,JK,KAERO(JO)) = MAX(PTENC(JL,JK,KAERO(JO)), &
               & -ZPGFL_STATE / TIME_STEP_LEN)
         END DO
      END DO
   END IF
END DO

! ======================================================================
! 6. OPTICAL PROPERTIES -diagnostic (placeholder - full rad coupling later - testing)
! ======================================================================

PAOD (KIDIA:KFDIA,:)=0._JPRB
PSSA (KIDIA:KFDIA,:)=0._JPRB
PASY (KIDIA:KFDIA,:)=0._JPRB
PAOD_LW(KIDIA:KFDIA,:)=0._JPRB

DO JK = 1, KLEV
  DO JL = KIDIA,KFDIA
    DO IW=1,kbndsw
      PAOD(JL,IW)=PAER_TAU(JL,JK,IW)+PAOD(JL,IW)
      PSSA(JL,IW)=PAER_SSA(JL,JK,IW)*PAER_TAU(JL,JK,IW)+PSSA(JL,IW)
      PASY(JL,IW)=PAER_ASYM(JL,JK,IW)*PAER_TAU(JL,JK,IW)*PAER_SSA(JL,JK,IW)+PASY(JL,IW)
    END DO

    DO IW=1,16
      PAOD_LW(JL,IW)=PAOD_LW(JL,IW)+PAER_TAU_LW(JL,JK,IW)
    END DO

  END DO
END DO

DO JL = KIDIA,KFDIA
  DO IW=1,kbndsw
    IF(PSSA(JL,IW)>0._JPRB) THEN
      PASY(JL,IW) = PASY(JL,IW)/PSSA(JL,IW) ! AVERAGE
    ENDIF
    IF(PAOD(JL,IW)>0._JPRB) THEN
      PSSA(JL,IW) = PSSA(JL,IW)/PAOD(JL,IW) ! AOD AVERAGE
    ENDIF
  END DO
END DO

! =======================================================================
! 8. Outputs
! =======================================================================

!DO JL = 1,NCLASS !output removal fluxes for tracer numbers
!   PGFL(KIDIA:KFDIA,JL,YAEROUT(1)%MP)=ZFLUX_DDEP(KIDIA:KFDIA,JL)
!   PGFL(KIDIA:KFDIA,JL,YAEROUT(2)%MP)=ZFLUX_WDEP(KIDIA:KFDIA,JL)
!   PGFL(KIDIA:KFDIA,JL,YAEROUT(3)%MP)=ZFLUX_SEDI(KIDIA:KFDIA,JL)
!END DO
!
!IF(.NOT.LIFSMIN  .AND. .NOT.LIFSTRAJ) THEN
!  !** YAEROUT(1) : RADIATIVE PROPERTIES
!  
!  ! AOD/SSA/ASY of one internal (short) wavelengths - 533 nm
!  PGFL(KIDIA:KFDIA,1,YAEROUT(4)%MP)=PAOD(KIDIA:KFDIA,10)
!  PGFL(KIDIA:KFDIA,2,YAEROUT(4)%MP)=PSSA(KIDIA:KFDIA,10)
!  PGFL(KIDIA:KFDIA,3,YAEROUT(4)%MP)=PASY(KIDIA:KFDIA,10)
! 
!  !** YAEROUT(5-6) : Total column number concentration and burdens
!  DO JM=1,NCLASS
!   
!    ZTMP(KIDIA:KFDIA)=0.0_JPRB
!    DO JK=1,KLEV
!      ZTMP(KIDIA:KFDIA)= ZTMP(KIDIA:KFDIA) + (ZXTM1(KIDIA:KFDIA,JK,JM)+(ZXTTE(KIDIA:KFDIA,JK,JM)*TIME_STEP_LEN)) * ZDPG(KIDIA:KFDIA,JK)
!    END DO
!    !Total column number concentration
!    PGFL(KIDIA:KFDIA,JM,YAEROUT(5)%MP) = ZTMP(KIDIA:KFDIA)
!
!    !Burden mass
!    PGFL(KIDIA:KFDIA,JM,YAEROUT(6)%MP) = ZTMP(KIDIA:KFDIA) * speclist(JM)%density&
!    & * (RPI / 6.0_JPRB) * (sizeclass(JM)%lite_radi*2.0_JPRB)**3 * exp(4.5_JPRB * log(sigma_mode(JM))**2)
!
!  END DO
!  PGFL(KIDIA:KFDIA,1:KLEV,YAEROUT(7)%MP)=PAER_TAU(KIDIA:KFDIA,1:KLEV,10)
!ENDIF

IF(.NOT.LIFSMIN  .AND. .NOT.LIFSTRAJ) THEN
  !---------------------------------------
  !-->hhermann: ML data

  ! HAM-Lite tracer number concentrations
  PGFL(KIDIA:KFDIA,1:KLEV,YAEROUT(1)%MP) = ZXTM1(KIDIA:KFDIA,1:KLEV,1)
  PGFL(KIDIA:KFDIA,1:KLEV,YAEROUT(2)%MP) = ZXTM1(KIDIA:KFDIA,1:KLEV,2)
  PGFL(KIDIA:KFDIA,1:KLEV,YAEROUT(3)%MP) = ZXTM1(KIDIA:KFDIA,1:KLEV,3)
  PGFL(KIDIA:KFDIA,1:KLEV,YAEROUT(4)%MP) = ZXTM1(KIDIA:KFDIA,1:KLEV,4)
  
  ! HAM-Lite 550nm extinction coefficient
  PGFL(KIDIA:KFDIA,1:KLEV,YAEROUT(5)%MP) = PAER_TAU(KIDIA:KFDIA,1:KLEV,10)
  
  ! HAM-Lite SSA
  PGFL(KIDIA:KFDIA,1:KLEV,YAEROUT(6)%MP) = PAER_SSA(KIDIA:KFDIA,1:KLEV,10)
  
  ! HAM-Lite asymmetry factor
  PGFL(KIDIA:KFDIA,1:KLEV,YAEROUT(7)%MP) = PAER_ASYM(KIDIA:KFDIA,1:KLEV,11)
  
  !<--hhermann: ML data
  !---------------------------------------
ENDIF

END ASSOCIATE
END ASSOCIATE

IF (LHOOK) CALL DR_HOOK('HAMLITE_INTERFACE',1,ZHOOK_HANDLE)

END SUBROUTINE HAMLITE_INTERFACE
