SUBROUTINE SUCST_IFSAUX

!**** *SUCST_IFSAUX * - Routine to initialize the constants of the model.

!     Purpose.
!     --------
!           Initialize and print the common YOMCST_IFSAUX
!           Should be consistent with the content of arp/setup/sucst.F90
!           Do not use currently with LDYNCORE=T, RPLRADI /=1

!**   Interface.
!     ----------
!        *CALL* *SUCST_IFSAUX (..)

!        Explicit arguments :
!        --------------------
!        none

!        Implicit arguments :
!        --------------------
!        COMMON YOMCST_IFSAUX

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!        Mats Hamrud and Philippe Courtier  *ECMWF*

!     Modifications.
!     --------------
!        Original : 87-10-15
!        K. Yessad (Jun 2009): IFSAUX version
!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

USE YOMCST_IFSAUX, ONLY : XRPI     ,XRA

!      -----------------------------------------------------------------

IMPLICIT NONE

REAL(KIND=JPRB) :: ZXRPLRADI
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!      -----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SUCST_IFSAUX',0,ZHOOK_HANDLE)
!      -----------------------------------------------------------------

!*       1.    DEFINE FUNDAMENTAL CONSTANTS.
!              -----------------------------

XRPI=2.0_JPRB*ASIN(1.0_JPRB)

!     ------------------------------------------------------------------

!*       2.    DEFINE GEOIDE.
!              --------------

ZXRPLRADI=1.0_JPRB
XRA=6371229._JPRB*ZXRPLRADI

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('SUCST_IFSAUX',1,ZHOOK_HANDLE)
END SUBROUTINE SUCST_IFSAUX
