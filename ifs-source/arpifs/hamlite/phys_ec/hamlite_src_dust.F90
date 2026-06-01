SUBROUTINE HAMLITE_SRC_DUST &
  &( KIDIA, KFDIA, KLON, KTILES, KSW, &
  &  PGELAM, PGELAT, PLSM, PSNS, &
  &  PWIND, PWS1, PALB, PALBD, PFRTI, PHSDFOR, &
  &  PEMISS_MASS )

!**** *HAMLITE_SRC_DUST* - TOTAL DUST MASS EMISSION FOR HAMLITE
!
!     Purpose:
!     --------
!     Compute total mineral dust mass emission flux (kg/m2/s) for the HAMlite
!     bulk dust mode using the operational ECMWF NDDUST=3 climatology +
!     emission formulation (Morcrette et al., MACC) collapsed to one bin.
!
!     This routine is fully self-contained: the regional atlas (37 IFF zones),
!     per-region emission-potential factors, per-source-type reference threshold
!     speed and reference radius, and the wilting-point / saturation soil
!     moisture constants are all hardcoded here. No M7 module data is touched.
!
!     Interface:
!     ----------
!     Called from TM5M7_PHY2 (hamlite branch in AERINI_LAYER pathway).
!
!     Inputs:
!     -------
!     PGELAM(KLON), PGELAT(KLON) : longitude, latitude  (radians)
!     PLSM(KLON)                 : land-sea mask
!     PSNS(KLON)                 : snow depth          (m)
!     PWIND(KLON)                : 10-m wind speed     (m/s)
!     PWS1(KLON)                 : top soil moisture   (m3/m3)
!     PALB(KLON)                 : total surface albedo
!     PALBD(KLON,KSW)            : MODIS UV-VIS diffuse albedo per SW band
!     PFRTI(KLON,KTILES)         : tile fractions
!     PHSDFOR(KLON)              : st. dev. of filtered sub-grid orography (m)
!
!     Output:
!     -------
!     PEMISS_MASS(KLON)          : bulk dust mass emission flux (kg/m2/s)
!
!     Method (NDDUST=3 / ECMWF):
!     --------------------------
!     1. Map each grid point to one of 37 IFF regions via lat/lon atlas.
!     2. Look up per-region scaling RDDUAER(IFF), reference threshold speed
!        and reference particle radius from RDUSRCP(NDUSRCP(IFF),:).
!     3. Build dust emission potential ZDUEMPOT = RDDUAER(IFF)*RDDUBULK*PLSM.
!     4. Apply source mask (snow-free, low orography, low albedo, bare-soil
!        tile present, no permanent ice, no wet skin, etc.).
!     5. Compute soil-wetness-shifted lifting threshold ZLTS2.
!     6. Saltation flux  PEMISS_MASS = RAERDUB * ZDUEMPOT * Albedo_factor *
!                                       MAX(0,(U-U_t))*U^2.
!
!     Author: A. Sarkar / FMI, 2026
!     Modifications:
!       Apr 2026 - Self-contained port of full ECMWF NDDUST=3 atlas + flux. Similar to TM5M7_SRC_DUST but collapsed to one bin for HAMlite. 

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK
USE YOMCST    ,ONLY : RPI

IMPLICIT NONE

!*       0.1   ARGUMENTS

INTEGER(KIND=JPIM),    INTENT(IN)  :: KIDIA, KFDIA, KLON, KTILES, KSW
REAL(KIND=JPRB),       INTENT(IN)  :: PGELAM(KLON), PGELAT(KLON)
REAL(KIND=JPRB),       INTENT(IN)  :: PLSM(KLON), PSNS(KLON)
REAL(KIND=JPRB),       INTENT(IN)  :: PWIND(KLON), PWS1(KLON)
REAL(KIND=JPRB),       INTENT(IN)  :: PALB(KLON), PALBD(KLON,KSW)
REAL(KIND=JPRB),       INTENT(IN)  :: PFRTI(KLON,KTILES), PHSDFOR(KLON)
REAL(KIND=JPRB),       INTENT(OUT) :: PEMISS_MASS(KLON)

!*       0.2   PARAMETERS  (ECMWF NDDUST=3 defaults, hardcoded here so that
!              this routine is self-contained and decoupled from YDEAERMAP.)

!     - per-bin scale factor for mass emission potential, summed over the
!       3 ECMWF dust bins (0.3 + 0.8 + 5.5) for HAMlite bulk mode.
REAL(KIND=JPRB), PARAMETER :: RDDUBULK = 6.6_JPRB

!     - global emission coefficient (matches RAERDUB in tm5m7_src_dust.F90)
REAL(KIND=JPRB), PARAMETER :: RAERDUB  = 1.E-11_JPRB

!     - soil hydrology constants (ECland defaults, surf_inq.F90)
REAL(KIND=JPRB), PARAMETER :: ZRWPWP   = 0.171_JPRB
REAL(KIND=JPRB), PARAMETER :: ZRWSAT   = 0.323_JPRB

!     - source-mask thresholds (identical to NDDUST=3)
REAL(KIND=JPRB), PARAMETER :: ZEPSSNO  = 1.0E-03_JPRB
REAL(KIND=JPRB), PARAMETER :: ZEPSARE  = 1.0E-03_JPRB
REAL(KIND=JPRB), PARAMETER :: ZALBMAX  = 0.52_JPRB
REAL(KIND=JPRB), PARAMETER :: ZOROGMAX = 50.0_JPRB
REAL(KIND=JPRB), PARAMETER :: ZBAREMIN = 0.1_JPRB
REAL(KIND=JPRB), PARAMETER :: ZLOWVEGMAX = 0.5_JPRB

!     - MODIS UV-VIS albedo gates (NDDUST=3 line ~1697)
REAL(KIND=JPRB), PARAMETER :: ZALBDMIN = 0.08_JPRB
REAL(KIND=JPRB), PARAMETER :: ZALBDMAX = 0.55_JPRB

!     - dummy KBINDD kept so the atlas snippet from M7 compiles unchanged
INTEGER(KIND=JPIM), PARAMETER :: KBINDD = 3

!*       0.3   LOCAL TABLES  (NDDUST=3 hardcoded from yoeaermap defaults)

REAL(KIND=JPRB)    :: RDDUAER(50)
INTEGER(KIND=JPIM) :: NDUSRCP(50)
REAL(KIND=JPRB)    :: RDUSRCP(10,2)
REAL(KIND=JPRB)    :: RDDUSRC(9)

!*       0.4   LOCAL VARIABLES (atlas + flux core)

INTEGER(KIND=JPIM) :: JL, JAER, IFF, ITYPDU
REAL(KIND=JPRB)    :: ZLAT, ZLON, ZRAD2DEG
REAL(KIND=JPRB)    :: ZBNDA, ZBNDB, ZBNDC, ZBNDD, ZBNDE, ZBNDF, ZBNDG, ZBNDH
REAL(KIND=JPRB)    :: ZBNDI, ZBNDJ, ZBNDK, ZBNDL, ZBNDM, ZLONGB
REAL(KIND=JPRB)    :: ZINCLAT, ZLONW, ZLONE
REAL(KIND=JPRB)    :: PAERMAP(KLON,5)
REAL(KIND=JPRB)    :: ZDUEMPOT(KLON,3)
REAL(KIND=JPRB)    :: ZREFSPD, ZREFRAD, ZRADREF
REAL(KIND=JPRB)    :: ZSWETN, ZFSWET, ZLTS2, ZLTSMIN, ZLTSMAX
REAL(KIND=JPRB)    :: ZWND3, ZDEP, ZALBFAC
LOGICAL            :: LLPDUSTS
REAL(KIND=JPHOOK)  :: ZHOOK_HANDLE

!-----------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('HAMLITE_SRC_DUST',0,ZHOOK_HANDLE)

PEMISS_MASS(:) = 0.0_JPRB
PAERMAP(:,:)   = 0.0_JPRB
ZDUEMPOT(:,:)  = 0.0_JPRB

!*       1.    HARDCODED NDDUST=3 TABLES
!              -------------------------

RDDUAER(:)   = 0.0_JPRB
NDUSRCP(:)   = 1
RDUSRCP(:,:) = 0.0_JPRB
RDDUSRC(:)   = 0.0_JPRB

! Per-region emission-potential scaling factors (IFF=1..37)
RDDUAER( 1)=1.0_JPRB ; RDDUAER( 2)=1.0_JPRB ; RDDUAER( 3)=0.5_JPRB
RDDUAER( 4)=0.6_JPRB ; RDDUAER( 5)=0.6_JPRB ; RDDUAER( 6)=1.0_JPRB
RDDUAER( 7)=1.0_JPRB ; RDDUAER( 8)=1.0_JPRB ; RDDUAER( 9)=1.0_JPRB
RDDUAER(10)=1.0_JPRB ; RDDUAER(11)=1.0_JPRB ; RDDUAER(12)=1.0_JPRB
RDDUAER(13)=0.0_JPRB ; RDDUAER(14)=0.2_JPRB ; RDDUAER(15)=0.5_JPRB
RDDUAER(16)=1.0_JPRB ; RDDUAER(17)=1.0_JPRB ; RDDUAER(18)=0.5_JPRB
RDDUAER(19)=0.5_JPRB ; RDDUAER(20)=0.8_JPRB ; RDDUAER(21)=1.2_JPRB
RDDUAER(22)=1.5_JPRB ; RDDUAER(23)=1.5_JPRB ; RDDUAER(24)=0.5_JPRB
RDDUAER(25)=1.0_JPRB ; RDDUAER(26)=0.5_JPRB ; RDDUAER(27)=1.0_JPRB
RDDUAER(28)=0.5_JPRB ; RDDUAER(29)=1.0_JPRB ; RDDUAER(30)=1.0_JPRB
RDDUAER(31)=0.1_JPRB ; RDDUAER(32)=0.1_JPRB ; RDDUAER(33)=0.3_JPRB
RDDUAER(34)=0.8_JPRB ; RDDUAER(35)=0.5_JPRB ; RDDUAER(36)=0.4_JPRB
RDDUAER(37)=0.6_JPRB

! Per-source-type reference threshold speed (col 1) and reference radius (col 2)
RDUSRCP(1,1)=6.0_JPRB ; RDUSRCP(1,2)=5.0_JPRB    ! N & S America, Europe
RDUSRCP(2,1)=6.0_JPRB ; RDUSRCP(2,2)=5.0_JPRB    ! Russia, Urals
RDUSRCP(3,1)=6.0_JPRB ; RDUSRCP(3,2)=5.0_JPRB    ! Africa, Sahara, S. Africa
RDUSRCP(4,1)=4.0_JPRB ; RDUSRCP(4,2)=5.0_JPRB    ! Australasia
RDUSRCP(5,1)=3.5_JPRB ; RDUSRCP(5,2)=5.0_JPRB    ! Asian deserts
RDUSRCP(6,1)=4.0_JPRB ; RDUSRCP(6,2)=5.0_JPRB    ! Dry lands of S. America
RDUSRCP(7,1)=4.0_JPRB ; RDUSRCP(7,2)=5.0_JPRB    ! Rest (Japan, Greenland, Antarctica)

! Per-bin emission-potential weights (kept here so atlas snippet compiles
! unchanged; for HAMlite we only use the bulk sum RDDUBULK = sum of these).
RDDUSRC(1)=0.3_JPRB ; RDDUSRC(2)=0.8_JPRB ; RDDUSRC(3)=5.5_JPRB

!*       2.    REGIONAL ATLAS  (verbatim port of NDDUST=3, lat/lon in degrees)
!              -------------------------------------------------------------

ZRAD2DEG = 180.0_JPRB / RPI

!PAERMAP(KIDIA:KFDIA,1:5) = 0._JPRB
DO JL=KIDIA,KFDIA
  ZLAT = PGELAT(JL) * ZRAD2DEG
  ZLON = PGELAM(JL) * ZRAD2DEG
  IF (ZLON < 0.0_JPRB) ZLON = ZLON + 360.0_JPRB
  ZBNDA= 30._JPRB+(36._JPRB -ZLAT)*14._JPRB/24._JPRB
  ZBNDB= 30._JPRB+(36._JPRB -ZLAT)*40._JPRB/16._JPRB
  ZBNDC= 38._JPRB+(ZLON-124._JPRB)*12._JPRB/29._JPRB
  ZBNDD= 32._JPRB-(ZLON-243._JPRB)* 6._JPRB/21._JPRB

!-- Eastern border Canada/USA
  ZBNDE= 49._JPRB
  IF (ZLON > 268._JPRB .AND. ZLON < 277._JPRB) THEN
    ZBNDE= 49._JPRB-(ZLON-268._JPRB)*7._JPRB/9._JPRB
  ELSEIF (ZLON >= 277._JPRB .AND. ZLON < 285._JPRB) THEN
    ZBNDE= 42._JPRB+(ZLON-277._JPRB)*2._JPRB/8._JPRB
  ELSEIF (ZLON >= 285._JPRB .AND. ZLON < 310._JPRB) THEN
    ZBNDE= 44._JPRB+(ZLON-285._JPRB)*3._JPRB/25._JPRB
  ENDIF

!-- limits Britain
  ZLONGB=-9999._JPRB
  IF (ZLON > 354._JPRB .AND. ZLON < 360._JPRB) THEN
    ZLONGB=ZLON
  ELSEIF (ZLON >= 0._JPRB .AND. ZLON < 3._JPRB) THEN
    ZLONGB=ZLON+360._JPRB
  ENDIF
  ZBNDF= 47._JPRB+(ZLONGB-349._JPRB)*4.5_JPRB/14._JPRB

!-- limits Ireland
  ZBNDG= 61._JPRB-(ZLON-349._JPRB)*7._JPRB/6._JPRB
  ZBNDH= 45._JPRB+(ZLON-349._JPRB)*9._JPRB/6._JPRB

!-- Western border Brazil
  IF (ZLAT <= 4._JPRB .AND. ZLAT > 2._JPRB) THEN
    ZBNDI= 296._JPRB
  ELSEIF (ZLAT <= 2._JPRB .AND. ZLAT > -4._JPRB) THEN
    ZBNDI= 290._JPRB
  ELSEIF (ZLAT <= -4._JPRB .AND. ZLAT > -7._JPRB) THEN
    ZBNDI= 290._JPRB-(-4._JPRB-ZLAT)*4._JPRB/3._JPRB
  ELSEIF (ZLAT <= -7._JPRB .AND. ZLAT > -11._JPRB) THEN
    ZBNDI= 286._JPRB+(-7._JPRB-ZLAT)*4._JPRB/4._JPRB
  ENDIF

  IF (ZLAT <= -11._JPRB .AND. ZLAT > -18._JPRB) THEN
    ZBNDJ= 294._JPRB+(-11._JPRB-ZLAT)*8._JPRB/7._JPRB
  ELSEIF (ZLAT <= -18._JPRB .AND. ZLAT > -27._JPRB) THEN
    ZBNDJ= 302._JPRB+(-18._JPRB-ZLAT)*4._JPRB/9._JPRB
  ELSEIF (ZLAT <= -27._JPRB .AND. ZLAT > -30._JPRB) THEN
    ZBNDJ= 306._JPRB-(-27._JPRB-ZLAT)*3._JPRB/3._JPRB
  ELSEIF (ZLAT <= -30._JPRB .AND. ZLAT >= -34._JPRB) THEN
    ZBNDJ= 303._JPRB+(-30._JPRB-ZLAT)*4._JPRB/4._JPRB
  ENDIF

!-- Northern border India
  IF (ZLON > 70._JPRB .AND. ZLON <= 90._JPRB) THEN
    ZBNDK= 35._JPRB-(ZLON-70._JPRB)*0.5_JPRB
  ENDIF

!-- South border of Asian deserts
  IF (ZLON > 90._JPRB .AND. ZLON <= 135._JPRB) THEN
    ZBNDL= 25._JPRB+(ZLON-90._JPRB)*15._JPRB/45._JPRB
  ENDIF

!-- North limit of the Argentinian pampas
  IF (ZLON > 285._JPRB .AND. ZLON <= 297._JPRB) THEN
    ZBNDM= -42._JPRB+(ZLON-285._JPRB)*6._JPRB/12._JPRB 
  ENDIF

  IFF=0
  ITYPDU=0
 
!-- North America
!  ITYPDU=1
!----- Canada
  IF ( ZLAT >= ZBNDE .AND.&
    &      (ZLON > 190._JPRB .AND. ZLON < 330._JPRB) ) THEN
    IFF=1
!----- USA
  ELSEIF ( (ZLAT >= ZBNDD .AND. ZLAT < ZBNDE )&
    & .AND. (ZLON > 190._JPRB .AND. ZLON < 330._JPRB) ) THEN
    IFF=3
  ENDIF
!-- Alaska
  IF ( (ZLAT < 72._JPRB .AND. ZLAT > 52._JPRB)&
    & .AND. (ZLON > 190._JPRB .AND. ZLON <= 219._JPRB) ) THEN
    IFF=2
  ENDIF

!-- Central America
  IF (ZLAT < ZBNDD .AND.&
    &     (ZLON > 190._JPRB .AND. ZLON < 330._JPRB) ) THEN
    IFF=4
  ENDIF

!-- South America
  IF ( ZLAT < 12._JPRB .AND.&
    &      (ZLON > 190._JPRB .AND. ZLON < 330._JPRB) ) THEN
    IFF=5
  ENDIF
!-- Brazil
  IF ( (ZLAT <= 4._JPRB .AND. ZLAT > 2._JPRB)&
    &      .AND. (ZLON >= 296._JPRB .AND. ZLON <= 300._JPRB) ) THEN
    IFF=6 
  ENDIF
  IF (ZLAT <= 2._JPRB .AND. ZLAT > -11._JPRB) THEN
    IF (ZLON >= ZBNDI .AND. ZLON < 330._JPRB) THEN
      IFF=6
    ENDIF
  ENDIF
  IF (ZLAT <= -11._JPRB .AND. ZLAT >= -34._JPRB) THEN
    IF (ZLON >= ZBNDJ .AND. ZLON < 330._JPRB) THEN
      IFF=6
    ENDIF
  ENDIF

!-- Western Europe
  IF ( ZLAT > 36._JPRB .AND. ( ZLON >= 330._JPRB .OR. ZLON <= 30._JPRB) ) THEN
    IFF=10
  ENDIF

!----- Iceland
  IF ( (ZLAT < 67._JPRB .AND. ZLAT > 63._JPRB)&
    &      .AND. ( ZLON > 335._JPRB .AND. ZLON < 353._JPRB) ) THEN
    IFF=7
  ENDIF
!----- Britain  
  IF ( (ZLAT < 63._JPRB .AND. ZLAT > ZBNDF)&
    &      .AND. ( ZLON > 354._JPRB .OR. ZLON < 3._JPRB) ) THEN
    IFF=9
  ENDIF
!----- Ireland
  IF ( (ZLAT < ZBNDG .AND. ZLAT > ZBNDH)&
    &      .AND. ( ZLON > 349._JPRB .AND. ZLON < 355._JPRB) ) THEN
    IFF=8
  ENDIF
  ITYPDU=1
  IF ( (IFF >= 1 .AND. IFF <= 10) .OR. IFF == 16) THEN
    NDUSRCP(IFF)=ITYPDU
  ENDIF        

! ITYPDU=2 
!-- Russia to Urals
  IF ( ZLON > 30._JPRB .AND. ZLON <= 70._JPRB ) THEN
    IF ( ZLAT > 51._JPRB ) THEN
      IFF=11
    ELSEIF ( ZLAT > 36._JPRB ) THEN
      IFF=12
    ENDIF
  ENDIF
  ITYPDU=2
  IF ( IFF >= 11 .AND. IFF <= 12 ) THEN
    NDUSRCP(IFF)=ITYPDU
  ENDIF        

! ITYPDU=3 
!-- Northern Sahara
!  if ( ( zlat <= 36._JPRB .and. zlat >= 21._JPRB) &
!    & .and. ( zlon >= 330._JPRB .or. zlon <= zbnda) ) then
!    iff=13
!-- Northern Sahara (West)
  IF ( ( ZLAT <= 36._JPRB .AND. ZLAT >= 21._JPRB)&
    & .AND. ( ZLON >= 330._JPRB .OR. ZLON < 2._JPRB) ) THEN
    IFF=36
!-- Northern Sahara (East)
  ELSEIF ( ( ZLAT <= 36._JPRB .AND. ZLAT >= 21._JPRB)&
    & .AND. ( ZLON >= 2._JPRB .OR. ZLON <= ZBNDA) ) THEN
    IFF=37
!-- Southern Sahara (West)
  ELSEIF ( ( ZLAT < 21._JPRB .AND. ZLAT >= 12._JPRB)&
    & .AND. ( ZLON >= 330._JPRB .OR. ZLON < 8._JPRB) ) THEN
    IFF=34
!-- Southern Sahara (East)
  ELSEIF ( ( ZLAT < 21._JPRB .AND. ZLAT >= 12._JPRB)&
    & .AND. ( ZLON >= 8._JPRB .AND. ZLON <= ZBNDA) ) THEN
    IFF=35
!-- Central Africa
  ELSEIF ( ( ZLAT < 12._JPRB .AND. ZLAT >= -12._JPRB)&
    &      .AND. ( ZLON >= 330._JPRB .OR. ZLON <= 60._JPRB) ) THEN
    IFF=14
!-- Southern Africa
  ELSEIF ( ZLAT < -12._JPRB .AND. ZLAT >= -60._JPRB&
    &      .AND. ( ZLON >= 330._JPRB .OR. ZLON <= 60._JPRB) ) THEN
    IFF=15
  ENDIF
  ITYPDU=3
  IF ( (IFF >= 13 .AND. IFF <= 15) .OR. (IFF >= 34 .AND. IFF <= 37) ) THEN
    NDUSRCP(IFF)=ITYPDU
  ENDIF        

!  ITYPDU=4
!-- Australasia
  IF (ZLON > 70._JPRB .AND. ZLON <= 190._JPRB) THEN
    IFF=26

!-- Siberia
    IF (ZLAT <= 90._JPRB .AND. ZLAT > 51._JPRB) THEN
      IFF=16

!-- South Australasia
!---- Tropical Pacific Islands
    ELSEIF ( ZLAT > -10.5_JPRB) THEN
      IFF=27
!---- Australia
    ELSEIF ( ZLAT <= -10.5_JPRB .AND. ZLAT >= -60._JPRB) THEN
      IFF=28
    ENDIF
  ENDIF
  ITYPDU=4
  IF ( IFF >= 26 .AND. IFF <= 28 ) THEN
    NDUSRCP(IFF)=ITYPDU
  ENDIF    

! ITYPDU=5
!-- Asian deserts
  IF (ZLON > 90._JPRB .AND. ZLON <= 135._JPRB) THEN
    IF (ZLAT <= 51._JPRB .AND. ZLAT > ZBNDL) THEN
      IFF=17
    ENDIF
  ENDIF

!-- Saudi Arabia
  IF ((ZLAT <= 36._JPRB .AND. ZLAT >= 12._JPRB)&
    & .AND.( ZLON > ZBNDA .AND. ZLON < ZBNDB) ) THEN
    IFF=18
  ENDIF
!-- Irak, Iran, Pakistan
  IF ((ZLAT <= 36._JPRB   .AND. ZLAT >= 20._JPRB)&
    & .AND.( ZLON > ZBNDB .AND. ZLON < 70._JPRB) ) THEN
    IFF=19
  ENDIF

!-- Central Asia and India
  IF ( ZLON > 70._JPRB .AND. ZLON <= 90._JPRB) THEN
!----- Central Asia: Taklamakan
    IF (ZLAT <= 43._JPRB .AND. ZLAT >= ZBNDK) THEN
      IFF=20
!----- India
    ELSEIF (ZLAT <= ZBNDK .AND. ZLAT > 7._JPRB) THEN
      IFF=21
    ENDIF
  ENDIF
!-- other Gobi(s) in South Mongolia and Central China
  IF ( ZLAT <= 49._JPRB .AND. ZLAT > 35._JPRB) THEN
    IF (ZLON > 90._JPRB .AND. ZLON <= 110._JPRB)  THEN
      IFF=22
    ELSEIF (ZLON > 110._JPRB .AND. ZLON <= 125._JPRB) THEN
      IFF=23
    ENDIF
  ENDIF

!-- South China  
  IF (ZLON > 90._JPRB .AND. ZLON <= 135._JPRB) THEN
    IF (ZLAT <= ZBNDL .AND. ZLAT > 7._JPRB) THEN
      IFF=24
    ENDIF
  ENDIF
  ITYPDU=5
  IF ( IFF >= 17 .AND. IFF <= 24 ) THEN
    NDUSRCP(IFF)=ITYPDU
  ENDIF    


! ITYPDU=7
!-- Japan and S.Korea
  IF ( (ZLON > 124._JPRB .AND. ZLON < 153._JPRB)&
    & .AND. (ZLAT > 24._JPRB .AND. ZLAT < ZBNDC) ) THEN
    IFF=25
  ENDIF

!-- Greenland
  IF (ZLAT > 50._JPRB) THEN
    ZINCLAT=(90._JPRB-ZLAT)/40._JPRB*45._JPRB
    ZLONW=270._JPRB +ZINCLAT
    ZLONE=360._JPRB -ZINCLAT
    IF ( ZLON > ZLONW .AND. ZLON <  ZLONE ) THEN
      IFF=29
    ENDIF
  ENDIF

!-- Antarctica
  IF (ZLAT < -60._JPRB) THEN
    IFF=30
  ENDIF
  ITYPDU=7
  IF ( (IFF >= 29 .AND. IFF <= 30) .OR. IFF == 25 ) THEN
    NDUSRCP(IFF)=ITYPDU
  ENDIF

!-- awaiting a proper recoding, new areas are set between iff=31 and 35
! ITYPDU=6 
  IF ( ZLON > 285._JPRB .AND. ZLON < 295._JPRB) THEN
!- Atacama desert and Salar de Uyuni
    IF ( ZLAT < -16._JPRB  .AND. ZLAT > -28._JPRB) THEN
      IFF=31
    ENDIF
!- Salar de Pipanaco and other small ones
    IF ( ZLAT <= -28._JPRB .AND. ZLAT > ZBNDM) THEN
      IFF=32
    ENDIF
  ENDIF
!- Argentinian pampas
  IF (ZLON > 285._JPRB .AND. ZLON < 297._JPRB) THEN
    IF ( ZLAT <= ZBNDM ) THEN
      IFF=33
    ENDIF
  ENDIF
  ITYPDU=6
  IF ( IFF >= 31 .AND. IFF <= 33 ) THEN
    NDUSRCP(IFF)=ITYPDU
  ENDIF
       
  IF (IFF /= 0) THEN
    PAERMAP(JL,1)=IFF*PLSM(JL)                                   ! area index
    PAERMAP(JL,3)=RDUSRCP(NDUSRCP(IFF),1)                        ! reference speed
    PAERMAP(JL,4)=RDUSRCP(NDUSRCP(IFF),2)                        ! reference particule radius
    DO JAER=1,KBINDD
       !ZDUEMPOT(JL,JAER)=RDDUAER(IFF)*RDDUSRC(IFF,JAER)*PLSM(JL)  ! dust emission potential factor (including land-sea mask)
       ZDUEMPOT(JL,JAER)=RDDUAER(IFF)*RDDUSRC(JAER)*PLSM(JL)  ! dust emission potential factor (including land-sea mask)
    ENDDO
    PAERMAP(JL,2)=ZDUEMPOT(JL,1)                                 ! for diagnostics only
  ELSE
    !WRITE(NULOUT,FMT='(''aer_src: Unassigned grid for Lat,Lon='',2F8.2)') ZLAT,ZLON
    PAERMAP(JL,:)=0._JPRB
  ENDIF
ENDDO


!*       3.    SOURCE MASK + FLUX CORE  (verbatim NDDUST=3, collapsed to bulk)
!              ---------------------------------------------------------------

DO JL = KIDIA, KFDIA

  LLPDUSTS = .FALSE.
  ZLTS2    = 0.0_JPRB

  IF (PLSM(JL) < 0.99_JPRB) CYCLE

  ! Reference threshold speed and radius for this region
  ZREFSPD = PAERMAP(JL,3)
  ZREFRAD = PAERMAP(JL,4)
  IF (ZREFSPD <= 0.0_JPRB .OR. ZREFRAD <= 0.0_JPRB) CYCLE
  ZRADREF = ZREFSPD * ZREFRAD**0.25_JPRB

  ! Soil-moisture-dependent lifting threshold (Ginoux-style)
  ZLTSMIN = 0.6_JPRB * ZRADREF
  ZLTSMAX = 1.2_JPRB * ZRADREF
  ZSWETN  = MIN(1.0_JPRB, MAX(0.001_JPRB, &
            & (PWS1(JL) - ZRWPWP) / (ZRWSAT - ZRWPWP)))
  ZFSWET  = 1.2_JPRB + 0.2_JPRB * LOG10(ZSWETN)
  ZLTS2   = MIN(ZLTSMAX, MAX(ZLTSMIN, ZFSWET * ZRADREF))

  ! Source mask: snow-free, flatish, low albedo, bare-soil tile, no ice/wet
  IF (PSNS(JL)    >= ZEPSSNO ) CYCLE
  IF (PHSDFOR(JL) >  ZOROGMAX) CYCLE
  IF (PALB(JL)    >= ZALBMAX ) CYCLE
  IF (KTILES >= 8) THEN
    IF (PFRTI(JL,2) >= ZEPSARE   ) CYCLE
    IF (PFRTI(JL,3) >= ZEPSARE   ) CYCLE
    IF (PFRTI(JL,5) >= ZEPSARE   ) CYCLE
    IF (PFRTI(JL,6) >= ZEPSARE   ) CYCLE
    IF (PFRTI(JL,7) >= ZEPSARE   ) CYCLE
    IF (PFRTI(JL,8) <= ZBAREMIN  ) CYCLE
    IF (PFRTI(JL,4) >= ZLOWVEGMAX) CYCLE
  END IF

  LLPDUSTS = .TRUE.

  ! Bulk emission potential = sum of per-bin ZDUEMPOT already weighted
  ! by RDDUAER(IFF), RDDUSRC(JAER) and PLSM in the atlas pass.
  ZDEP = RAERDUB * (ZDUEMPOT(JL,1) + ZDUEMPOT(JL,2) + ZDUEMPOT(JL,3))

  ! Wind-cube saltation
  ZWND3 = MAX(0.0_JPRB, (PWIND(JL) - ZLTS2) * PWIND(JL) * PWIND(JL))

  ! Albedo brightness factor: prefer MODIS UV-VIS (PALBD band 1), else PALB
  IF (KSW >= 1 .AND. PALBD(JL,1) >= ZALBDMIN .AND. PALBD(JL,1) < ZALBDMAX) THEN
    ZALBFAC = PALBD(JL,1)
  ELSE
    ZALBFAC = PALB(JL)
  END IF

  PEMISS_MASS(JL) = ZDEP * ZALBFAC * ZWND3

END DO

IF (LHOOK) CALL DR_HOOK('HAMLITE_SRC_DUST',1,ZHOOK_HANDLE)
END SUBROUTINE HAMLITE_SRC_DUST
