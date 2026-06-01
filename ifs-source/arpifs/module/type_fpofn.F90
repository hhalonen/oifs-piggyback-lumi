MODULE TYPE_FPOFN

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

! CSH : output file name containing spectral (hybrid) data
! CGG : output file name containing gridpoint (surface) data
! CSX : output file name containing gridpoint SURFEX data
! CUA : output file name containing upper air gridpoint data

! CLI : input file name containing surface climatology data on target geometry
! CSU : input file name containing SURFEX climatology data on target geometry

!====== FULLPOS INPUT/OUTPUT FILES  ======

TYPE TFPOFN

CHARACTER(LEN=256)  :: CSH
CHARACTER(LEN=256)  :: CGG
CHARACTER(LEN=256)  :: CSX
CHARACTER(LEN=256)  :: CUA
CHARACTER(LEN=256)  :: CLI
CHARACTER(LEN=256)  :: CSU

END TYPE TFPOFN

!     ------------------------------------------------------------------
END MODULE TYPE_FPOFN
