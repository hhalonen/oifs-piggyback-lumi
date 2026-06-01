MODULE YOMVRTLX

USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!*    Switches for variational assimilation: use of tangent linear model
! L801TL  : .T. = the sensitivity  is to be run with the tangent model.
!                 (otherwise a finite-difference approximation is used)
LOGICAL :: L801TL=.TRUE.
LOGICAL :: LMINI=.TRUE.

!     ------------------------------------------------------------------
END MODULE YOMVRTLX
