MODULE YOMGPPB

USE PARKIND1, ONLY : JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------


! GPARBUF               : buffer for AROME physics

REAL (KIND=JPRB), ALLOCATABLE :: GPARBUF (:,:,:)

!      containing pointers

!     ------------------------------------------------------------------
END MODULE YOMGPPB
