SUBROUTINE GPTET(KPROMA,KSTART,KPROF,KFLEV,PRESF,PT,PKAP,PTETA)

!**** *GPTET* - COMPUTES POTENTIAL TEMPERATURE

!     PURPOSE.
!     --------
!           COMPUTES POTENTIAL TEMPERATURE

!**   INTERFACE.
!     ----------
!        *CALL* *GPTET(...)*

!        EXPLICIT ARGUMENTS :
!        --------------------

!         KPROMA                - HORIZONTAL DIMENSIONS          (INPUT)
!         KSTART to KPROF       - DEPTH OF WORK                  (INPUT)
!         KFLEV                 - NUMBER OF MODEL LEVELS         (INPUT)
!         PRESF(KPROMA,KFLEV)   - FULL LEVEL PRESSURE            (INPUT)
!         PT(KPROMA,KFLEV)      - FULL LEVEL TEMPERATURE         (INPUT)
!         PKAP(KPROMA,KFLEV)    - FULL LEVEL KAPPA (GPRCP)       (INPUT)

!         PTETA(KPROMA,KFLEV)   - POTENTIAL TEMPERATURE          (OUTPUT)

!        IMPLICIT ARGUMENTS : CONSTANTS FROM YOMCST.
!        --------------------

!     METHOD.
!     -------
!        SEE DOCUMENTATION

!     EXTERNALS.
!     ----------
!        None.
!        Called by CPG.

!     REFERENCE.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     AUTHOR.
!     -------
!      MATS HAMRUD AND PHILIPPE COURTIER  *ECMWF*
!      ORIGINAL : 88-02-04

!     MODIFICATIONS.
!     --------------
!      M.Hamrud      01-Oct-2003 CY28 Cleaning
!     ------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM     ,JPRB
USE YOMHOOK  , ONLY : LHOOK,   DR_HOOK, JPHOOK

USE YOMCST   , ONLY : RATM

!     ------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KPROMA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTART 
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROF 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRESF(KPROMA,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT(KPROMA,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKAP(KPROMA,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PTETA(KPROMA,KFLEV) 

!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JLEV, JROF

REAL(KIND=JPRB) :: ZUSRATM
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GPTET',0,ZHOOK_HANDLE)
!     ------------------------------------------------------------------

!*       1.    COMPUTES POTENTIAL TEMPERATURE.
!              -------------------------------

ZUSRATM=1.0_JPRB/RATM
DO JLEV=1,KFLEV
  DO JROF=KSTART,KPROF
    PTETA(JROF,JLEV)=&
     & +PT(JROF,JLEV)*(PRESF(JROF,JLEV)*ZUSRATM)**(-PKAP(JROF,JLEV))  
  ENDDO
ENDDO

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('GPTET',1,ZHOOK_HANDLE)
END SUBROUTINE GPTET
