MODULE YOMGF1C

!*    grid point array for large scale forcing of the one column model

!     ------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM     ,JPRB
USE PARDIM1C , ONLY : JPFLEV

IMPLICIT NONE

SAVE

!  === Geostrophic Wind ===

REAL(KIND=JPRB) :: UG0(JPFLEV)
REAL(KIND=JPRB) :: VG0(JPFLEV)

!  === Vertical Velocity ===

REAL(KIND=JPRB), target :: VVEL0(1,JPFLEV)

!  === horizontal advection ===

REAL(KIND=JPRB) :: UADV(JPFLEV)
REAL(KIND=JPRB) :: VADV(JPFLEV)
REAL(KIND=JPRB) :: TADV(JPFLEV)
REAL(KIND=JPRB) :: QADV(JPFLEV)

REAL(KIND=JPRB) :: etadotdpdeta(0:JPFLEV)

!     ------------------------------------------------------------------

END MODULE YOMGF1C
