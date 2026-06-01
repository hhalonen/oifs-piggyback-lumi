MODULE YOS_THF
 
USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

REAL(KIND=JPRB) :: RHOH2O    ! DENSITY OF LIQUID WATER.   (RATM/100.)
REAL(KIND=JPRB) :: RVTMP2    ! RVTMP2=RCPV/RCPD-1.
REAL(KIND=JPRB) :: R2ES      ! Constants for computation of esat
REAL(KIND=JPRB) :: R3LES     ! Constants for computation of esat
REAL(KIND=JPRB) :: R3IES     ! Constants for computation of esat
REAL(KIND=JPRB) :: R4LES     ! Constants for computation of esat
REAL(KIND=JPRB) :: R4IES     ! Constants for computation of esat
REAL(KIND=JPRB) :: R5LES     ! Constants for computation of esat
REAL(KIND=JPRB) :: R5IES     ! Constants for computation of esat

END MODULE YOS_THF
