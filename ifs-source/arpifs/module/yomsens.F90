MODULE YOMSENS

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

!     --------------------------------------------------

!*    Control for SENSitivity job

!     LGRVOL = .T.  : ACTIVATE GRADIENT NORMALIZATION BY SIDELP TO WRITE
!                        FILES OF 3D DENSITY OF GRADIENT
!     NJROPT        : TYPE OF COST FUNCTION:
!                             1 - QUADRATIC DISTANCE TO A REFERENCE (DEFAULT)
!                             2 - LINEAR INTEGRAL OF PARAMETERS
!     LBSENS        : USE B-matrix in the initial norm

LOGICAL :: LGRVOL
INTEGER(KIND=JPIM) :: NJROPT
LOGICAL :: LBSENS

!     --------------------------------------------------
END MODULE YOMSENS
