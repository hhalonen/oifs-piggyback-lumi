!     ------------------------------------------------------------------

!     EFFICENT WAVE NUMBER FOR EACH WAVE NUMBER COUPLE (N,M)

REAL(KIND=JPRB) :: FKSTAR
INTEGER(KIND=JPIM) :: KN, KM, KNSMAX, KNMSMAX

FKSTAR(KN,KM,KNSMAX,KNMSMAX)=INT(KNSMAX*SQRT((REAL(KN,JPRB)&
                               &/REAL(KNSMAX,JPRB))**2 &
                               &+(REAL(KM,JPRB)&
                               &/REAL(KNMSMAX,JPRB))**2)&
                               &+ 0.49_JPRB)
!     ------------------------------------------------------------------



