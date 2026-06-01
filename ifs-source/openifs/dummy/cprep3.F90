SUBROUTINE CPREP3(KITER)
use parkind1 , only:&
 & jpim
INTEGER(KIND=JPIM),INTENT(OUT) :: KITER
call abor1("cprep3.F90 should never be called with OpenIFS - EXITING")
END SUBROUTINE CPREP3
