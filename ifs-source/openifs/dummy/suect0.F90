SUBROUTINE SUECT0(KULOUT)
use parkind1 , only:&
 & jpim
INTEGER(KIND=JPIM),INTENT(IN) :: KULOUT
call abor1("suect0.F90 should never be called with OpenIFS - EXITING")
END SUBROUTINE SUECT0
