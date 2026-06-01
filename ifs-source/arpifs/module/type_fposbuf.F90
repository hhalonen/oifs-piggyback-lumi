MODULE TYPE_FPOSBUF

USE PARKIND1  ,ONLY : JPRB
USE YOMFP4L , ONLY : TRQFP

IMPLICIT NONE

SAVE

! Post-prpcessing data derived type :
! =================================

! YRQPHY: fields request
! FPBUF : cache-blocked data array

TYPE FPOSBUF


TYPE(TRQFP)               :: YRQPHY
REAL(KIND=JPRB)    , ALLOCATABLE :: FPBUF(:,:,:)

END TYPE FPOSBUF

END MODULE TYPE_FPOSBUF
