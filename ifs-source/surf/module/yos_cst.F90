MODULE YOS_CST

USE PARKIND1  ,ONLY : JPIM     ,JPRB
  
IMPLICIT NONE

SAVE

TYPE :: TCST 
REAL(KIND=JPRB) :: RTT     ! RT0=273.16= TRIPLE POINT TEMPERATURE
REAL(KIND=JPRB) :: RPI     ! PI=3.14...
REAL(KIND=JPRB) :: RDAY    ! Astronomical constants
REAL(KIND=JPRB) :: R       ! Thermodynamic gas phase
REAL(KIND=JPRB) :: RD      ! Thermodynamic gas phase
REAL(KIND=JPRB) :: RV      ! Thermodynamic gas phase
REAL(KIND=JPRB) :: RETV    ! Thermodynamic gas phase
REAL(KIND=JPRB) :: RLSTT   ! Thermodynamic transition of phase
REAL(KIND=JPRB) :: RLMLT   ! Thermodynamic transition of phase
REAL(KIND=JPRB) :: RLVTT   ! Thermodynamic transition of phase
REAL(KIND=JPRB) :: RCPD    ! Thermodynamic gas phase
REAL(KIND=JPRB) :: RCPV    ! Thermodynamic gas phase
REAL(KIND=JPRB) :: RSIGMA  ! Radiation
REAL(KIND=JPRB) :: RG      ! Geoide
REAL(KIND=JPRB) :: RATM    ! 
REAL(KIND=JPRB) :: ROMEGA
END TYPE TCST 

END MODULE YOS_CST
