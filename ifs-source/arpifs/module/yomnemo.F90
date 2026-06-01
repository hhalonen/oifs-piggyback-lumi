MODULE YOMNEMO

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!*    Control variables for the NEMO model

! NEMOCSTEP : Current NEMO time step
! NEMONSTEP : Number of NEMO steps between each coupling

INTEGER(KIND=JPIM) :: NEMOCSTEP
INTEGER(KIND=JPIM) :: NEMONSTEP

!     ------------------------------------------------------------------
END MODULE YOMNEMO

