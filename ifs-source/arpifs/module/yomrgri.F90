MODULE YOMRGRI

USE PARKIND1, ONLY : JPIM
USE PARDIM,   ONLY : JPMXGL

IMPLICIT NONE

SAVE

! NRGRI: for each latitude, number of longitudes of the Gaussian grid;
!        has to be specified if NHTYP=2.
INTEGER(KIND=JPIM) :: NRGRI(JPMXGL)

!-----------------------------------------------------------------------------
END MODULE YOMRGRI
