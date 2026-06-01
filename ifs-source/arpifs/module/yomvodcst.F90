MODULE YOMVODCST

USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

! Temporally constant values for shallow-water model with N2DINI=11
REAL(KIND=JPRB),ALLOCATABLE:: CSTVO(:,:)  ! for vorticity
REAL(KIND=JPRB),ALLOCATABLE:: CSTDIV(:,:) ! for divergence

!     ------------------------------------------------------------------
END MODULE YOMVODCST
