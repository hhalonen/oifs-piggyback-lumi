MODULE YOMIOS

IMPLICIT NONE

SAVE

! -------- RESTART FILES -------------------
! CFRCF    : pathname for restart control file
! CIOSPRF  : PATHNAME FOR SPECTRAL RESTART FILE
! LRCFTIME : ALSO WRITE A RESTART CONTROL FILE WITH TIME

CHARACTER (LEN = 120) ::  CIOSPRF
CHARACTER (LEN = 120) ::  CFRCF
LOGICAL :: LRCFTIME
!     ------------------------------------------------------------------
END MODULE YOMIOS
