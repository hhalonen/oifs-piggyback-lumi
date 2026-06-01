MODULE FULLPOS

USE YOMFPCNT     , ONLY : TFPCNT
USE YOMFPGEOMETRY, ONLY : TFPGEOMETRY
USE YOMVERT      , ONLY : TVAB
USE YOMFPFILTERS , ONLY : TFPFILTERS
USE EINT_MOD     , ONLY : SL_STRUCT
USE YOMWFPB      , ONLY : TFPWSTD, TFPSUW
USE YOMAFN       , ONLY : TAFN
USE YOMFPOP      , ONLY : TFPIOH
USE YOMFPC       , ONLY : TNAMFPSCI, TNAMFPINT
USE TYPE_FPOSBUF , ONLY : FPOSBUF


IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

! FULLPOS DATA OBJECT

TYPE TFPOS

! Controler
TYPE(TFPCNT) :: YFPCNT

! Horizontal geometry
TYPE(TFPGEOMETRY) :: YFPGEOMETRY

! Vertical geometry
TYPE(TVAB) :: YFPVAB

! Spectral filters
TYPE(TFPFILTERS)  :: YFPFILTERS

! Horizontal halos management
TYPE(SL_STRUCT) :: YFPSTRUCT

! Interpolator
TYPE(TFPWSTD) :: YFPWSTD

! I/O handling
TYPE(TFPIOH) :: YFPIOH

! Fields descriptors
TYPE(TAFN) :: YAFN

! Scientific parameters
TYPE(TNAMFPSCI) :: YNAMFPSCI

! Parameters driving the horizontal interpolations
! Special structure (to be re-worked)
TYPE(TNAMFPINT) :: YNAMFPINT

END TYPE TFPOS


! FULLPOS FIELDS-DEPENDENT OR TIME-DEPENDENT AUXILARY DATA OBJECT

TYPE TFPDATA

! Surface-dependent interpolation weights : logical key to re-initialize and data structure
LOGICAL :: LFPUPDSUW = .TRUE. ! always true at first call
TYPE(TFPSUW) :: YFPSUW

! Output climatology : logical key to re-initialize and data structure
LOGICAL :: LFPUPDCLI = .TRUE. ! always true at first call
TYPE(FPOSBUF) :: YFPCLIMO

TYPE(TFPOS), POINTER :: YFPOS => NULL()

END TYPE TFPDATA

!     ------------------------------------------------------------------      
END MODULE FULLPOS
