MODULE YOMCT2

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!*    Control variables for the model - changed at level 2 during ex.

! NSTAR2  : first timestep of model
! NSTOP2  : last timestep of model
! NSTP    : length of an elementary covariance transport (Kalman filter)
! NSTAR2CPL : First step for coupling; usually 0 (regular forecast, can be > 0 on restart)

INTEGER(KIND=JPIM) :: NSTAR2
INTEGER(KIND=JPIM) :: NSTOP2
INTEGER(KIND=JPIM) :: NSTP
INTEGER(KIND=JPIM) :: NSTAR2CPL

!     ------------------------------------------------------------------
END MODULE YOMCT2
