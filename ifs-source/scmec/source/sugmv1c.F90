SUBROUTINE SUGMV1C(YDGMV)

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

USE YOMGMV   , ONLY : TGMV

!**** *SUGFL1C*  - Initialize definition of GMV model fields

!     Purpose.
!     --------
!           Hacked version doing the least necessary work to please
!           phys dyn interface. 

!     Author.
!     -------
!        Filip Vana      *ECMWF*

!     Modifications.
!     --------------
!        Original    2014-01-16

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(TGMV) ,     INTENT(INOUT) :: YDGMV

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!-------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('SUGMV1C',0,ZHOOK_HANDLE)
ASSOCIATE(YT1=>YDGMV%YT1)

!-------------------------------------------------------------------------

YT1%NDIM=3
YT1%MU=1
YT1%MV=2
YT1%MT=3

YT1%NDIMS=1
YT1%MSP=1


END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SUGMV1C',1,ZHOOK_HANDLE)

!-------------------------------------------------------------------------

END SUBROUTINE SUGMV1C
