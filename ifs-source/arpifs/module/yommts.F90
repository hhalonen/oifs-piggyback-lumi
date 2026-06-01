MODULE YOMMTS

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

! LMTS : Switch d'activation de la production de temperatures de brillance
! IF LMTS and LMTSCL production of clear sky TB
! NHLIM : max hour  (production for hour of forecast < -NHLIM)
! LUBIQUITAIRE : To have the satellite at the vetical of all the grid points
! LISP_HYBRID : MSG simulated data in 'real' conditions over the MSG domain, 
!               ubiquitaire computation elsewhere

INTEGER(KIND=JPIM) :: NTYPE

LOGICAL :: LMTS,LMTSCL,LUBIQUITAIRE,LISP_HYBRID


END MODULE YOMMTS
