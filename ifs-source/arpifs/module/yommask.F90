MODULE YOMMASK

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

! ----------------------------------------------------------------------

! Module for On demand Semi-Lagrangian Masks

! * NFIXSFLD: description of fixed fields (fields not sent/received)
!   in on-demand communications.
INTEGER(KIND=JPIM) :: NFIXSFLD(2) 

! ----------------------------------------------------------------------

END MODULE YOMMASK

