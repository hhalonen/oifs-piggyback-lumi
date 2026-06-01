! ---------------------------------------------------------------

! These functions are used to compute the relaxation coefficient:

! FEZBP and FEZBM are the profiles of the relaxation coefficient

! FEZE is the exponent e of z**e=x**e+y**e

REAL(KIND=JPRB) :: FEZBP, FEZBM, FEZE
REAL(KIND=JPRB) :: PZ, PEPA, PALP
INTEGER(KIND=JPIM) :: KNA, KMA

FEZBP(PZ,PEPA)=(PEPA+1.0_JPRB)*PZ**PEPA-PEPA*PZ**(PEPA+1.0_JPRB)

FEZBM(PZ,PEPA)=1.0_JPRB-(PEPA+1.0_JPRB)*(1.0_JPRB-PZ)**PEPA &
              &+PEPA*(1.0_JPRB-PZ)**(PEPA+1.0_JPRB)

FEZE(PZ,PALP,KNA,KMA)=1.0_JPRB/(PALP*(PZ**KNA)*((1.0_JPRB-PZ)**KMA))
! ------------------------------------------------------------------

