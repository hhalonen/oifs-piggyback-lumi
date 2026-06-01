MODULE YOMCST_IFSAUX

USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!*    Module of physical constants: XRD/IFSAUX version.

! A1.0 Fundamental constants
! * XRPI         : number Pi
REAL(KIND=JPRB) :: XRPI

! A1.2 Geoide
! * XRA          : Earth radius
REAL(KIND=JPRB) :: XRA

!    ------------------------------------------------------------------
END MODULE YOMCST_IFSAUX
