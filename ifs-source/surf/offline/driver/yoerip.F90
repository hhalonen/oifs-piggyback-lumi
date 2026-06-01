MODULE YOERIP

USE PARKIND1  ,ONLY : JPIM     ,JPRB,JPRD

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!*    Real time related variables for ECMWF radiation: updated UPDTIER

!     RIP0M  : I0 WEIGHTED BY THE DISTANCE EARTH-SUN

!     RCODECM: COSINE OF THE DECLINATION
!     RSIDECM:   SINE OF THE DECLINATION

!     RCOVSRM: COSINE OF TRUE SOLAR TIME
!     RSIVSRM:   SINE OF TRUE SOLAR TIME

REAL(KIND=JPRB) :: RIP0M
REAL(KIND=JPRD) :: RCODECM
REAL(KIND=JPRD) :: RSIDECM
REAL(KIND=JPRD) :: RCOVSRM
REAL(KIND=JPRD) :: RSIVSRM
!     ------------------------------------------------------------------
END MODULE YOERIP
