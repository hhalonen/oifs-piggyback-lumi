MODULE YOMIAU

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

! Logical to activate IAU
LOGICAL :: LIAU

! Integer
INTEGER(KIND=JPIM) :: NSTARTIAU ! fist time step of the IAU
INTEGER(KIND=JPIM) :: NSTOPIAU  ! last time step of the IAU

! Real
REAL(KIND=JPRB) :: TSTARTIAU ! fist time (sec.) of the IAU
REAL(KIND=JPRB) :: TSTOPIAU  ! last time (sec.) of the IAU
REAL(KIND=JPRB) :: ALPHAIAU  ! part of the total increment added during the execution
                             ! then increment added at each time step is ALPHAIAU*(xa-xb)/(NSTARTIAU-NSTOPIAU) 

END MODULE YOMIAU
