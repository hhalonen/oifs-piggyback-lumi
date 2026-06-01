MODULE YOE_AERVOLE

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*    ** *YOE_AERVOLE* - CONTROL PARAMETERS FOR ERUPTING VOLCANO
!     ------------------------------------------------------------------

REAL(KIND=JPRB) :: RVEMHGHT(19)
REAL(KIND=JPRB) :: RVEMMAS(19,160)
REAL(KIND=JPRB) :: RVMASSEM(19)

REAL(KIND=JPRB) :: RVMASSVI(0:19)
REAL(KIND=JPRB) :: RVHGHTEM(0:19)

INTEGER(KIND=JPIM) :: JVEMTIM(160)
INTEGER(KIND=JPIM) :: NVDATES
!------------------------------------------------------------------------------
END MODULE YOE_AERVOLE
