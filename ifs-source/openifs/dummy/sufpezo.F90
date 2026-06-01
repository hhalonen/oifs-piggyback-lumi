SUBROUTINE SUFPEZO(PCO_EZO)
use parkind1 , only:&
 & jprb
REAL(KIND=JPRB), INTENT(OUT) :: PCO_EZO
call abor1("sufpezo.F90 should never be called with OpenIFS - EXITING")
END SUBROUTINE SUFPEZO
