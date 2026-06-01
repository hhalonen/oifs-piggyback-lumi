MODULE YOMFPFILTERS

USE PARKIND1, ONLY : JPIM, JPRB

IMPLICIT NONE

SAVE

PRIVATE
PUBLIC TFPFILTERS

!     ------------------------------------------------------------------

!*    Full-POS spectral filter

! LFPFIL   : .TRUE. if the filter for dilated fields is active (arpege only) for
!            the corresponding (sub)domain
! RFPFIL   : value of the filter for each zonal wavenumber and each subdomain

! LFPMAT   : .TRUE. if filtering matrix in the homogenous resolution space must be computed
! RFPMAT   : Filtering matrix in the homogenous resolution space (ARPEGE only)

TYPE TFPFILTERS

LOGICAL, ALLOCATABLE :: LFPFIL(:)
REAL(KIND=JPRB), ALLOCATABLE :: RFPFIL(:,:)

LOGICAL :: LFPMAT
REAL(KIND=JPRB), ALLOCATABLE :: RFPMAT(:,:)

END TYPE TFPFILTERS

!     ------------------------------------------------------------------
END MODULE YOMFPFILTERS
