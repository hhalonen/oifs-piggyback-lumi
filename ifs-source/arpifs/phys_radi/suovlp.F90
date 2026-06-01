SUBROUTINE SUOVLP(YDSTA,YDEOVLP,YDERAD,KLEV)
  
!***** *SUOVLP*   -INITIALIZE PROFILE OF ALPHA1

!**   INTERFACE.
!     ----------
!        CALL *SUOVLP* FROM *SUECRAD*
!              ------        -------

!     REFERENCE.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     AUTHOR.
!     -------
!        JEAN-JACQUES MORCRETTE  *ECMWF*

!     MODIFICATIONS.
!     --------------
!        ORIGINAL : 01-02-16
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!        T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!     ------------------------------------------------------------------

USE YOMSTA   , ONLY : TSTA
USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOERAD   , ONLY : TERAD
USE YOEOVLP  , ONLY : TEOVLP

IMPLICIT NONE

!     ------------------------------------------------------------------

!     DUMMY PARAMETERS

TYPE(TSTA)        ,INTENT(IN) :: YDSTA
TYPE(TEOVLP)      ,INTENT(INOUT):: YDEOVLP
TYPE(TERAD)       ,INTENT(INOUT):: YDERAD
INTEGER(KIND=JPIM),INTENT(IN) :: KLEV 

INTEGER(KIND=JPIM) :: JK
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('SUOVLP',0,ZHOOK_HANDLE)
ASSOCIATE(RA1OVLP=>YDEOVLP%RA1OVLP, &
 & RAOVLP=>YDERAD%RAOVLP, RBOVLP=>YDERAD%RBOVLP, &
 & STZ=>YDSTA%STZ)
DO JK=1,KLEV
  RA1OVLP(JK)=RAOVLP*STZ(JK)+RBOVLP
ENDDO  

!     ------------------------------------------------------------------
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SUOVLP',1,ZHOOK_HANDLE)
END SUBROUTINE SUOVLP
