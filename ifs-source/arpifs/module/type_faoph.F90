MODULE TYPE_FAOPH

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!========== HANDLING OF OUTPUT FIELDS IN FA FILES  ======

! CFPCA : names of output FA frames
! NHEADMAX : maximum size of the header of a record
! NGPSIZPK : size of a gridpoint field in FA file
! NSPSIZPK : size of a spectral field in FA file

TYPE TFAOPH

CHARACTER(LEN=16)  :: CFPCA
INTEGER(KIND=JPIM) :: NHEADMAX
INTEGER(KIND=JPIM) :: NGPSIZPK
INTEGER(KIND=JPIM) :: NSPSIZPK

END TYPE TFAOPH

!     ------------------------------------------------------------------
END MODULE TYPE_FAOPH
