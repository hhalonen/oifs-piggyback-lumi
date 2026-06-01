MODULE YOMGT1C1

!     Grid point array at time t+dt in the atmospheric physics and dynamics

!     ------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM     ,JPRB
USE PARDIM1C , ONLY : JPFLEV

IMPLICIT NONE

SAVE

!  === Momentum variables ===

REAL(KIND=JPRB) :: UT1(JPFLEV)
REAL(KIND=JPRB) :: VT1(JPFLEV)

!  === Thermodynamical variables ===

REAL(KIND=JPRB) :: TT1(JPFLEV)
REAL(KIND=JPRB) :: QT1(JPFLEV)

!  === cloud variables ===

REAL(KIND=JPRB) :: WT1(JPFLEV)
REAL(KIND=JPRB) :: ST1(JPFLEV)
REAL(KIND=JPRB) :: AT1(JPFLEV)
REAL(KIND=JPRB) :: RNT1(JPFLEV)
REAL(KIND=JPRB) :: SNT1(JPFLEV)

! === surface pressure ===

REAL(KIND=JPRB) :: SPT1

!     ------------------------------------------------------------------

END MODULE YOMGT1C1
