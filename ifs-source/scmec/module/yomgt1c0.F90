MODULE YOMGT1C0

!     Grid point array at time t in the atmospheric physics and dynamics

!     ------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM     ,JPRB
USE PARDIM1C , ONLY : JPFLEV

IMPLICIT NONE

SAVE

!  === Momentum variables ===

REAL(KIND=JPRB) :: UT0(JPFLEV)
REAL(KIND=JPRB) :: VT0(JPFLEV)

!  === Thermodynamical variables ===

REAL(KIND=JPRB) :: TT0(JPFLEV)
REAL(KIND=JPRB) :: QT0(JPFLEV)

!  === cloud variables ===

REAL(KIND=JPRB) :: WT0(JPFLEV)
REAL(KIND=JPRB) :: ST0(JPFLEV)
REAL(KIND=JPRB) :: AT0(JPFLEV)
REAL(KIND=JPRB) :: RNT0(JPFLEV)
REAL(KIND=JPRB) :: SNT0(JPFLEV)

! === surface pressure ===

REAL(KIND=JPRB) :: SPT0

! === relaxation variables ===

REAL(KIND=JPRB) :: UOBS(JPFLEV)
REAL(KIND=JPRB) :: VOBS(JPFLEV)

REAL(KIND=JPRB) :: TOBS(JPFLEV)
REAL(KIND=JPRB) :: QOBS(JPFLEV)

!     ------------------------------------------------------------------

END MODULE YOMGT1C0
