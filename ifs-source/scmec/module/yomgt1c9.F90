MODULE YOMGT1C9

!     Grid point array at time t-dt in the atmospheric physics and dynamics

!     ------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM     ,JPRB
USE PARDIM1C , ONLY : JPFLEV

IMPLICIT NONE

SAVE

!  === Momentum variables ===

REAL(KIND=JPRB) :: UT9(JPFLEV)
REAL(KIND=JPRB) :: VT9(JPFLEV)

!  === Thermodynamical variables ===

REAL(KIND=JPRB) :: TT9(JPFLEV)
REAL(KIND=JPRB) :: QT9(JPFLEV)

!  === cloud variables ===

REAL(KIND=JPRB) :: WT9(JPFLEV)
REAL(KIND=JPRB) :: ST9(JPFLEV)
REAL(KIND=JPRB) :: AT9(JPFLEV)
REAL(KIND=JPRB) :: RNT9(JPFLEV)
REAL(KIND=JPRB) :: SNT9(JPFLEV)

! === surface pressure ===

REAL(KIND=JPRB) :: SPT9

!     ------------------------------------------------------------------

END MODULE YOMGT1C9
