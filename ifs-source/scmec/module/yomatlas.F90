MODULE YOMATLAS

USE PARKIND1, ONLY : JPIM, JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!*    * ATLAS

TYPE TATLAS
END TYPE TATLAS

TYPE(TATLAS), POINTER :: YRATLAS => NULL()


END MODULE YOMATLAS
