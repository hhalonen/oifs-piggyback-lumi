MODULE BASCOE_LBC_MODULE


USE PARKIND1, ONLY : JPIM, JPRB

IMPLICIT NONE

SAVE

  ! Boundary conditions at surface for stratospheric species
  ! Which currently don't feature emissions (!)
  ! array of monthly varying, latitude band dependent, values
  ! allocated and loaded loaded from external file
  ! in routine BASCOE_LBC_INI
  INTEGER(KIND=JPIM)    :: NMONTH_LBC, NLATBOUND_LBC
  INTEGER(KIND=JPIM) , DIMENSION(:), ALLOCATABLE    :: MONTH_LBC
  REAL(KIND=JPRB) , DIMENSION(:), ALLOCATABLE       :: XLATBOUND_LBC
  REAL(KIND=JPRB) , DIMENSION(:,:,:), ALLOCATABLE   :: VALUES_LBC


END MODULE BASCOE_LBC_MODULE
