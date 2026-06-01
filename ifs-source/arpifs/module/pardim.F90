MODULE PARDIM

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!     JPMXLE : MAXIMUM NUMBER OF LEVELS
!     JPMXGL : MAXIMUM NUMBER OF GAUSSIAN LATITUDES 
!     JPSLWIDE: maximum allowed number of rows for interpolation halos.
!     JPFPPYX : Maximum number of fields in catalogue cilipdy
!     JPNPPM  : Number of interpolation methods in post-processing

INTEGER(KIND=JPIM), PARAMETER :: JPMXLE=200
INTEGER(KIND=JPIM), PARAMETER :: JPMXGL=5120
INTEGER(KIND=JPIM), PARAMETER :: JPSLWIDE=48
INTEGER(KIND=JPIM), PARAMETER :: JPFPPYX=17
INTEGER(KIND=JPIM), PARAMETER :: JPNULNAM=4
INTEGER(KIND=JPIM), PARAMETER :: JPNPPM=4

!     ------------------------------------------------------------------
END MODULE PARDIM
