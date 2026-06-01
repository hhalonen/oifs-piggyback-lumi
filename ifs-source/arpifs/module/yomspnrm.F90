MODULE YOMSPNRM

USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

! ----------------------------------------------------------------------

! * AVNRM[X]: Spectral norms computed under SPNORM
!   [X]=DIV for divergence
!   [X]=PD pressure departure
!   [X]=VD vertical divergence

REAL(KIND=JPRB) :: AVNRMDIV, AVNRMPD, AVNRMVD

! ----------------------------------------------------------------------

END MODULE YOMSPNRM
