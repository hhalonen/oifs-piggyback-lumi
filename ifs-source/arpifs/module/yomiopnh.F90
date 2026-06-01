MODULE YOMIOPNH

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

! -------- TRAJECTORY FOR ADJOINT OF NONHYDROSTATIC DYN. --------

! NG3NH95   : NUMBER OF 3D FIELDS - (NFLEVG)
! LTRAJNH   : .T. IF NHS TRAJECTORY IS WRITTEN INTO BUFFER
!             NEEDED FOR TL AND AD MODELS (TRAJNH AND TRAJIT)

INTEGER(KIND=JPIM) :: NG3NH95

LOGICAL :: LTRAJNH

!     ------------------------------------------------------------------
END MODULE YOMIOPNH
