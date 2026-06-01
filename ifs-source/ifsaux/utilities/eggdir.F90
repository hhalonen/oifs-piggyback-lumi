SUBROUTINE EGGDIR (PRPI, PRA, PDELX, PDELY, KPROF,&
 & KBEG, KEND, KULOUT, PGX, PGY)  
!****
!---------------------------------------------------------------------

!     GEOGRAPHY OF GRID-POINTS, ROTATION/PROJECTION FROM GEOGRAPHICAL
!     SPHERE TO ARPEGE-ALADIN
!     --------------------------------------------------------------------

!       ---------------------------------------------------
!     PURPOSE
!     -------
!      KNOWING THE PRECISE GEOGRAPHICAL TRANSFORMATION FROM
!      ARGUMENTS AND COMMON /YEMGGCM/, COMPUTES THE LOCATION
!      OF THE GEOGRAPHICAL POINTS GIVEN IN INPUT ON
!      ON THE ARPEGE-ALADIN GRID

!      MUST BE USED IN CONNECTION WITH SUBROUTINE EGGX
!      AFTER IT HAS INITIALIZED /YEMGGCM/

!     INPUT PARAMETERS
!     ----------------
!      PRPI : PI (3.14ETC)
!      PRA  : A, RADIUS OF PLANET
!      PDELX, PDELY : GRID SIZE IN M IF PROJECTION, OR IN RADIANS
!      KPROF : SIZE OF (1D) ARRAYS
!      KBEG, KEND : BEGINNING AND END POINTS OF CALCULATIONS
!      KULOUT : LOGICAL UNIT OF LISTING
!      PGX(KPROF) : INPUT: TRUE GEOGRAPHICAL LONGITUDE IN RADIANS
!                          (SEE EGGX FOR CONVENTIONS)
!      PGY(KPROF) : INPUT : TRUE GEOGRAPHICAL LATITUDE IN RADIANS

!            IN SHORT, THE OUTPUT IS SUITABLE FOR USE IN EGGRVS

!     IMPLICIT INPUT
!     --------------
!      COMMON /YEMGGCM/ MUST HAVE BEEN INITIALIZED

!     OUTPUT PARAMETERS
!     -----------------
!      PGX (KPROF):
!                   OUTPUT: X LOCATION OF POINTS, DISTANCE UNDER PROJECTION,
!                            RELATIVE ROTATED LONGITUDE UNDER ROTATION,
!                            ALWAYS DEFINED AS (JLON-KDLUN)*PDELX
!      PGY (KPROF):
!                   OUTPUT : Y LOCATION OF POINTS, DISTANCE UNDER PROJECTION,
!                            RELATIVE ROTATED LATITUDE UNDER ROTATION,
!                            ALWAYS DEFINED AS (JLAT-KDGUN)*PDELY
!            UNDER ROTATION, THE POSITION OF THE ORIGIN (XLAT1R,XLON1U)
!            IS HANDLED BY THIS SUBROUTINE : ONLY RELATIVE LOCATION
!            ARE GIVEN ON OUTPUT.

!     WRITTEN BY
!     ---------- ALAIN JOLY

!      ORIGINAL NORTHERN HEMISPHERE VERSION : 10/5/92
!      SOUTHERN HEMISPHERE VERSION : 27/1/93

!-------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

USE YEMGGCM  , ONLY : NYMGGI   ,NYMGGR   ,NYMGGWH  ,XLATR    ,&
 & XLONR    ,XGGPK    ,XLAT0R   ,XLON0U   ,XIPORE   ,&
 & XJPORE   ,XLON1R   ,XLON1U   ,XLAT1R   ,XLON2R   ,&
 & HSUD     ,XBETA  

!-------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KPROF 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRPI 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRA 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDELX 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDELY 
INTEGER(KIND=JPIM),INTENT(IN)    :: KBEG 
INTEGER(KIND=JPIM),INTENT(IN)    :: KEND 
INTEGER(KIND=JPIM),INTENT(IN)    :: KULOUT 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGX(KPROF) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGY(KPROF) 

!-------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JJ

LOGICAL :: LLGWH

REAL(KIND=JPRB) :: Z2PI, Z2PIPK, ZCOBETA, ZCOS, ZCOSO, ZGAM,&
 & ZLAT, ZLIMIT, ZLON, ZPIS2, ZPIS4, ZRC, ZRR, &
 & ZSECAN, ZSECUR, ZSIBETA, ZSIN, ZSINO, ZXE, &
 & ZXP, ZYE, ZYP  
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!-------------------------------------------------------------------

#include "abor1.intfb.h"

!-------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('EGGDIR',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------

ZRC=0.0_JPRB

IF ( NYMGGI /= 10 ) THEN
  WRITE (KULOUT,*) '*** EGGDIR *** UNINITIALISED MODULE '
  CALL ABOR1(' EGGDIR: NYMGGI /= 10 ')
ENDIF

IF ( HSUD < 0.0_JPRB ) THEN
  DO JJ = KBEG, KEND
    PGY(JJ) = ABS( PGY(JJ) )
  ENDDO
ENDIF

!     INITIALISE ROTATION ANGLE AND OTHER CONSTANTS
!     ---------------------------------------------
ZPIS2 = PRPI*0.5_JPRB
ZPIS4 = PRPI*0.25_JPRB
ZSECUR = 1.E-12_JPRB
ZSECAN = 1.E-05_JPRB
LLGWH = .FALSE.
IF ( NYMGGWH == 1 ) LLGWH = .TRUE.
!     CHECK LONGITUDES
DO JJ = KBEG, KEND
  IF ( PGX(JJ) < 0.0_JPRB ) THEN
    PGX(JJ) = PGX(JJ) + 2.0_JPRB*PRPI
  ENDIF
ENDDO

!*
!----------------------------------------------------------------------
!     1.- DIRECT ROTATION
!     --------------------
IF ( NYMGGR /= 0 ) THEN
  DO JJ = KBEG, KEND
    ZSIN = COS( XLATR )*SIN( PGY(JJ) ) -&
     & SIN( XLATR )*COS( PGY(JJ) )*COS( PGX(JJ)-XLONR )  
    ZLAT = ASIN( ZSIN )
    IF ( ABS(ZLAT) >= ZPIS2 ) THEN
      ZLON = 0.0_JPRB
    ELSE
      ZCOS = COS( ZLAT )
      ZCOSO = ( SIN( XLATR )*SIN( PGY(JJ) ) +&
       & COS( XLATR )*COS( PGY(JJ) )*COS( PGX(JJ)-XLONR ) )/ZCOS  
      ZCOSO = MIN(1.0_JPRB,MAX(-1.0_JPRB,ZCOSO))
      ZSINO = ( COS( PGY(JJ) )*SIN( PGX(JJ)-XLONR ) )/ZCOS
      ZSINO = MIN(1.0_JPRB,MAX(-1.0_JPRB,ZSINO))
      ZLON = ACOS( ZCOSO )
      IF ( ASIN( ZSINO ) < 0.0_JPRB ) ZLON = 2.0_JPRB*PRPI - ZLON
    ENDIF
    PGX(JJ) = ZLON
    PGY(JJ) = ZLAT
  ENDDO
ENDIF

!     CORRECTION OF POSITION IF DOMAIN ASTRIDE GREENWICH
!     --------------------------------------------------
IF ( LLGWH ) THEN
  DO JJ = KBEG, KEND
    ZLIMIT=0.5_JPRB*(XLON2R+XLON1R-ZPIS2)
    IF ( PGX(JJ) < XLON1R .AND. PGX(JJ) < ZLIMIT )&
     & PGX(JJ) = PGX(JJ) + 2.0_JPRB*PRPI  
  ENDDO
ENDIF

!     FINAL RESULT IN THE ABSENCE OF PROJECTION
!     -----------------------------------------
IF ( XGGPK < 0.0_JPRB ) THEN
  DO JJ = KBEG, KEND
    PGX(JJ) = PGX(JJ) - XLON1U
    PGY(JJ) = PGY(JJ) - XLAT1R
  ENDDO
ENDIF

!*
!------------------------------------------------------------------
!     2.- DIRECT STEREO-LAMBERT PROJECTION
!     ------------------------------------
IF ( XGGPK > 0.0_JPRB ) THEN
  IF ( XGGPK < 1.0_JPRB ) THEN
    ZRC = PRA*( COS( XLAT0R )**(1.0_JPRB-XGGPK) )*&
     & ( (1.0_JPRB+SIN( XLAT0R ))**XGGPK )/XGGPK  
    Z2PI = 2.0_JPRB*PRPI
    Z2PIPK = 2.0_JPRB*PRPI*XGGPK
    DO JJ = KBEG, KEND
      IF ( (PGX(JJ)-XLON0U)  >  Z2PIPK ) THEN
        PGX(JJ) = PGX(JJ) - Z2PI
      ENDIF
    ENDDO
  ELSEIF ( XGGPK == 1.0_JPRB ) THEN
    ZRC = PRA*( 1.0_JPRB + SIN( XLAT0R ) )
  ELSE
    WRITE (KULOUT,*) ' *** EGGDIR *** UNKNOWN PROJECTION '
  ENDIF
  ZXP = XIPORE*PDELX
  ZYP = XJPORE*PDELY

  DO JJ = KBEG, KEND
    ZRR = ZRC*( COS(PGY(JJ)) /&
     & ( 1.0_JPRB+ SQRT(1.0_JPRB-COS(PGY(JJ))**2 ))  )**XGGPK      
    ZGAM = XGGPK*( PGX(JJ)-XLON0U ) - XBETA
    PGX(JJ) = ZXP + ZRR*SIN( ZGAM )
    PGY(JJ) = ZYP - HSUD*ZRR*COS( ZGAM )
  ENDDO
ENDIF

!*
!----------------------------------------------------------------------
!     3.- DIRECT MERCATOR PROJECTION
!     ------------------------------
IF ( XGGPK == 0.0_JPRB ) THEN
  ZRC = PRA*COS( XLAT0R )
  ZXE = XIPORE*PDELX
  ZYE = XJPORE*PDELY
  ZSIBETA = SIN( XBETA )
  ZCOBETA = COS( XBETA )
  DO JJ = KBEG, KEND
    ZXP = ZXE + ZRC*( PGX(JJ)-XLON0U )
    ZYP = ZYE - ZRC*LOG( TAN(ZPIS4 - 0.5_JPRB*PGY(JJ)) )
    PGX(JJ) = ZXP*ZCOBETA + ZYP*ZSIBETA
    PGY(JJ) = -ZXP*ZSIBETA + ZYP*ZCOBETA
  ENDDO
ENDIF

!-------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('EGGDIR',1,ZHOOK_HANDLE)
END SUBROUTINE EGGDIR
