SUBROUTINE SU2YOM(YDRIP)

!**** *SU2YOM*  - Initialize level 2 commons and some higher

!     Purpose.
!     --------
!           Initialize level 2 commons

!**   Interface.
!     ----------
!        *CALL* *SU2YOM

!        Explicit arguments :
!        --------------------
!        None

!        Implicit arguments :  Common YOMCT2
!        --------------------

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
!      Mats Hamrud and Philippe Courtier  *ECMWF*
!      Original : 87-10-15

!     Modifications.
!     --------------
!      M.Hamrud      01-Oct-2003 CY28 Cleaning
!      M.Hamrud      01-Dec-2003 CY28R1 Cleaning
!      K. Yessad (July 2014): Move some variables.
!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK
USE YOMRIP   , ONLY : TRIP
USE YOMLUN   , ONLY : NULOUT
USE YOMCT0   , ONLY : LELAM
USE YOMCT2   , ONLY : NSTAR2   ,NSTOP2   ,NSTP   ,NSTAR2CPL

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(TRIP),INTENT(INOUT):: YDRIP
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SU2YOM',0,ZHOOK_HANDLE)
ASSOCIATE(NSTART=>YDRIP%NSTART, NSTOP=>YDRIP%NSTOP)
!     ------------------------------------------------------------------

!*       1.    Initialize YOMCT2, set default values.
!              --------------------------------------

NSTAR2=NSTART
NSTOP2=NSTOP
NSTP=1
IF (LELAM) THEN
  NSTAR2CPL=NSTAR2
  WRITE(NULOUT,*) ' NSTAR2CPL = ',NSTAR2CPL
ENDIF

!     ------------------------------------------------------------------
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SU2YOM',1,ZHOOK_HANDLE)
END SUBROUTINE SU2YOM
