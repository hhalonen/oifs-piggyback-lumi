MODULE YOEVDF

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*    ** *YOEVDF* CONTAINS CONSTANTS NEEDED BY *VDF....*
!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) :: NVTYPES
REAL(KIND=JPRB) :: RLAM
REAL(KIND=JPRB) :: RKAP
REAL(KIND=JPRB) :: RVDIFTS
REAL(KIND=JPRB) :: REPDU2
REAL(KIND=JPRB) :: RENTR
REAL(KIND=JPRB) :: RPAR
REAL(KIND=JPRB) :: RPAR1
REAL(KIND=JPRB) :: RPARSRF

!*     *YOEVDF* CONTAINS CONSTANTS NEEDED BY *VDF....*
!     FOR THE COMPUTATION OF VERTICAL DIFFUSION

!     A.C.M. BELJAARS      E.C.M.W.F.    14/12/89

!     OBUKHOV-L UPDATE     ACMB          26/03/90.   

!     NAME        TYPE     DESCRIPTION
!     ----        ----     -----------

!     *NVTYPES*   INTEGER  Number of vegetation (surface cover) types
!     *RLAM*      REAL     *ASYMPTOTIC MIXING LENGTH FOR MOMENTUM
!     *RKAP*      REAL     *VONKARMAN CONSTANT
!     *RVDIFTS*   REAL     *FACTOR FOR TIME STEP WEIGHTING IN *VDF....*
!     *REPDU2*    REAL     *MINIMUM VELOCITY DIFFERENCE IN RI-NUMBER
!     *RENTR*     REAL     *ENTRAINMENT CONSTANT          
!     *RPAR*      REAL     *PARAMETER FOR TEMPERATURE EXCESS IN THERMAL 
!                           AT BOUNDARY LAYER TOP      
!     *RPAR1*     REAL     *COEFFICIENT OF (W*)**3 IN WS         
!     *RPARSRF*   REAL     *DEPTH OF SURFACE LAYER AS FRACTION OF PBL-H 
!     ------------------------------------------------------------------
END MODULE YOEVDF
