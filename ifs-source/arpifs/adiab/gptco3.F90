SUBROUTINE GPTCO3(KPROMA,KSTART,KPROF,KFLEV,PDELP,PO3,PDTCO3)

!**** *GPTCO3* - COMPUTES TOTAL COLUMN OZONE

!     Purpose.
!     --------
!           Computes total column ozone in kg/m**2.

!**   Interface.
!     ----------
!        *CALL* *GPTCO3(KPROMA,KSTART,KPROF,KFLEV,PDELP,PO3,PDTCO3)

!        Explicit arguments :
!        --------------------
!        KPROMA                     - HORIZ. DIMENSIONING      (INPUT)
!        KSTART                     - START OF WORK            (INPUT)
!        KPROF                      - DEPTH OF WORK            (INPUT)
!        KFLEV                      - NUMBER OF LEVELS         (INPUT)
!        PDELP(KPROMA,KFLEV)        - PRESSURE ACROSS LAYERS   (INPUT)
!        PO3  (KPROMA,KFLEV)        - OZONE MIXING RATIO       (INPUT)
!        PRXP(KPROMA)               - TOTAL COLUMN OZONE       (OUTPUT)

!        Implicit arguments :    None.
!        --------------------

!     Method.
!     -------
!        See documentation.

!     Externals.   None.
!     ----------

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!      Agathe Untch, based on PP-routines by
!      Mats Hamrud and Philippe Courtier  *ECMWF*
!      Original : 96-05-22

!     Modifications.
!     --------------
!      M.Hamrud      01-Oct-2003 CY28 Cleaning
!      K. Yessad (Jan 2011): remove useless overdimension.
!     ------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM     ,JPRB
USE YOMHOOK  , ONLY : LHOOK,   DR_HOOK, JPHOOK

USE YOMCST   , ONLY : RG

!     ------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KPROMA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTART 
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROF 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDELP(KPROMA,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PO3(KPROMA,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDTCO3(KPROMA) 

!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JL, JLEV

REAL(KIND=JPRB) :: ZRGI
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GPTCO3',0,ZHOOK_HANDLE)
!     ------------------------------------------------------------------

!*       1.    INTEGRATES THE OZONE MIXING RATIO IN THE VERTICAL (kg/m**2)
!              -----------------------------------------------------------

ZRGI=1.0_JPRB/RG
DO JL=KSTART,KPROF
  PDTCO3(JL)=0.0_JPRB
ENDDO
DO JLEV=1,KFLEV
  DO JL=KSTART,KPROF
    PDTCO3(JL)=PDTCO3(JL)+PO3(JL,JLEV)*PDELP(JL,JLEV)*ZRGI
  ENDDO
ENDDO

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('GPTCO3',1,ZHOOK_HANDLE)
END SUBROUTINE GPTCO3
