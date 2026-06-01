MODULE YOMFPIOS

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

! General post-processing I/O parameters

! NFPGRIB   : level of GRIB encoding
! NFPWRITE  : write (1) or not (0) output files. If (-1) then even do not write output norms
! NFPXFLD   : maximum number of fields to be extracted from a pp buffer at a time
! NFPDIGITS : number of digits for time stamp extension
!             If negative the NFPDIGITS is used as abs(NFPDIGITS) without
!             the character "+" preceeding it. This facility enables to
!             make file names like boundary files in some sense.
! LPGDFWR   : write Surfex PGD file or not
! LHISFWR   : write SURFEX historic data or not

TYPE TNAMFPIOS

INTEGER(KIND=JPIM) :: NFPGRIB
INTEGER(KIND=JPIM) :: NFPWRITE = 1
INTEGER(KIND=JPIM) :: NFPXFLD = -999
INTEGER(KIND=JPIM) :: NFPDIGITS
LOGICAL            :: LFPPGDFWR = .FALSE.
LOGICAL            :: LFPHISFWR = .FALSE.

END TYPE TNAMFPIOS

!     ------------------------------------------------------------------
END MODULE YOMFPIOS
