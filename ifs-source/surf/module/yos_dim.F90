MODULE YOS_DIM
 
USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

INTEGER(KIND=JPIM), PARAMETER :: JPSOILTY=11  ! number of soil types (CH)
INTEGER(KIND=JPIM), PARAMETER :: JPTEXT=3     ! number of macro-types (TESSEL->2only)

TYPE :: TDIM
INTEGER(KIND=JPIM) :: NCSS    ! number of levels in SOILB
INTEGER(KIND=JPIM) :: NTILES  ! number of surface tiles
INTEGER(KIND=JPIM) :: NMONTH  ! number of months im a year
INTEGER(KIND=JPIM) :: NCSNEC  ! number of snow levels 
END TYPE TDIM

END MODULE YOS_DIM
