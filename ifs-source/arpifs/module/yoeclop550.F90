MODULE YOECLOP550

USE PARKIND1, ONLY : JPIM, JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!     * YOECLOP550* PARAMETERS FOR CLOUD OPTICAL PROPERTIES
!     ------------------------------------------------------------------

REAL(KIND=JPRB) :: RSA55, RSB55,  RSC55, RSD55

REAL(KIND=JPRB) :: RFA55(2)

!*    SLINGO (1989) WATER CLOUD OPTICAL PROPERTIES

! RSA :  REAL   : C1 IN OPTICAL THICKNESS FORMULA
! RSB :  REAL   : C2 IN OPTICAL THICKNESS FORMULA

!*    ICE CLOUD OPTICAL PROPERTIES DERIVED FROM FU (1996)

! RFA :  REAL   : COEFFICIENTS IN EXPRESSION FOR SW EXTINCTION COEFF.

!     -----------------------------------------------------------------
END MODULE YOECLOP550
