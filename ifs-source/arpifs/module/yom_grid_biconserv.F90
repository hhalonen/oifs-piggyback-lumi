MODULE YOM_GRID_BICONSERV

USE PARKIND1 , ONLY : JPRB

IMPLICIT NONE

SAVE
! ----------------------------------------------------------------------------

! Global surface ln pressure fields for conserving interpolation of trajectory
! and increments. 
! * RGPPRS_HR    : grid-point high resolution (outer loop) lnps
! * RGPPRS_LR    : grid-point low resolution (inner loop) lnps

REAL(KIND=JPRB),ALLOCATABLE :: RGPPRS_HR (:)
REAL(KIND=JPRB),ALLOCATABLE :: RGPPRS_LR (:)

! ----------------------------------------------------------------------------
END MODULE YOM_GRID_BICONSERV
