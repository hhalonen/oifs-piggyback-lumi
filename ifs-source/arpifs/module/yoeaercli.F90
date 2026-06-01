MODULE YOEAERCLI

USE PARKIND1  ,ONLY : JPIM, JPRB

IMPLICIT NONE

SAVE

!     -------------------------------------------------------------------------
!*    ** *YOEAERCLI* - CONTROL OPTIONS TO ADJUST MACC-DERIVED AEROSOL CLIMATOLOGY
!     -------------------------------------------------------------------------

INTEGER(KIND=JPIM) :: NAERADJDU

REAL(KIND=JPRB) :: RDUMULF
REAL(KIND=JPRB) :: RWGHTDU1, RWGHTDU2, RWGHTDU3

!     -------------------------------------------------------------------------
! NAERADJU  : CONFIGURATION INDEX (0 NO CHANGE, 1 OVERALL WEIGHTING BY RFCTDU)
! RDUMULF   : MULTIPLICATION FACTOR
! RWGHTDUn  : REBALANCE FRACTIONS WITHIN BINS
!     -------------------------------------------------------------------------
END MODULE YOEAERCLI
