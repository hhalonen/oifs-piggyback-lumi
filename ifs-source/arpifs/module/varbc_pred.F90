MODULE VARBC_PRED
  ! Header and Type definition removed for OpenIFS and forecast only
  ! Retain declaration of JPREDNAME since required for CLASS_VARBC Type
  ! declaration
  USE PARKIND1   , ONLY : JPIM

  IMPLICIT NONE
  SAVE

  PRIVATE
  PUBLIC JPREDNAME

  INTEGER(KIND=JPIM), PARAMETER :: JPREDNAME = 21
!-----------------------------------------------------------------------
END MODULE VARBC_PRED
