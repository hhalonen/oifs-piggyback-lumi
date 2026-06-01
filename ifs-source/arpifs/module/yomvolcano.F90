MODULE YOMVOLCANO

! volcano emissions, dates and source strength

USE PARKIND1  ,ONLY : JPRB, JPIM

IMPLICIT NONE
SAVE
! volcano emissions
INTEGER(KIND=JPIM),PARAMETER :: JVOCDAT=200
INTEGER(KIND=JPIM)           ::  NVOCDATES
INTEGER(KIND=JPIM), DIMENSION(JVOCDAT) :: IVOCSTART ! start end date for specific source strength&height
LOGICAL            :: LVOCENS   ! tracer ensemble

REAL(KIND=JPRB),DIMENSION(JVOCDAT) :: SLVOC1, SLVOC2 ! injection height range in km  ! namelist input 
REAL(KIND=JPRB),DIMENSION(10) :: SLVOCES1, SLVOCES2 ! injection height range in km for 10 tracers ensemble
REAL(KIND=JPRB) , DIMENSION(JVOCDAT) :: SEMIVOC   ! kg/s
REAL(KIND=JPRB) , DIMENSION(JVOCDAT) :: SEMIVOCFLX   ! kg/m**2
INTEGER(KIND=JPIM),DIMENSION(JVOCDAT) :: ILVOCJB ! JB injection height model levels 
REAL(KIND=JPRB)  :: SEMIVOCENS   ! kg/s
REAL(KIND=JPRB)   :: SEMIVOCFLXENS   ! kg/m**2s
! volcano location
LOGICAL            :: LVOCMP   ! volcano on this mpi task
INTEGER(KIND=JPIM) :: IVOCGP   ! index of volcano location betwen 1 NPROMA
INTEGER(KIND=JPIM) :: IVOCBLK  ! block position of volcanoe in NGPTOT  
REAL(KIND=JPRB)    :: SVOCLAT, SVOCLON ! lat lon of volcano

!     -------------------------------------------------------------------------
END MODULE YOMVOLCANO
