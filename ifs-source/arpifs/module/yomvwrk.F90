MODULE YOMVWRK

USE PARKIND1, ONLY : JPIM

IMPLICIT NONE

SAVE

!   -----------------------------------------------------------------

!    NTRSLTYPE       : trajectory storage method -
!                       =0 no storage (only works for LELTRA=T)
!                       =1 minimum storage
!                       =2 more storage, less computation
!       ** N.B. Only NTRSLTYPE=2 is supported in the 3-d model **

INTEGER(KIND=JPIM) :: NTRSLTYPE

!   ----------------------------------------------------------------
END MODULE YOMVWRK
