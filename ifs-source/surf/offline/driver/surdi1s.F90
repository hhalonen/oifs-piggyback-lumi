SUBROUTINE SURDI1S
USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOERDI1S , ONLY : REMISS

#ifdef DOC

!**** *SURDI1s* - Initialize common YOERDI1S
!     ------------------------------------------------------------------
#endif
IMPLICIT NONE

REMISS = 0.99_JPRB

RETURN
END SUBROUTINE SURDI1S
