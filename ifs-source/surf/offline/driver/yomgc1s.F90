MODULE YOMGC1S
USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE
SAVE

!     ------------------------------------------------------------------
!*    Grid point boundaries

! -   VARIABLES

!       RCORI   -  Coriolis parameter 
!       GEMU    -  SIN of latitude on the real earth
!       GSQM2   -  COS of latitude on the real earth
!       GELAM   -  longitude on the real earth
!       GELAT   -  latitude on the real earth
!       GECLO   -  COS of longitude on the real earth
!       GESLO   -  SIN of longitude on the real earth
!       LMASK   -  Logical Mask Array of original input field

REAL(KIND=JPRB),ALLOCATABLE :: RCORI(:)
REAL(KIND=JPRB),ALLOCATABLE :: GEMU(:)
REAL(KIND=JPRB),ALLOCATABLE :: GSQM2(:)
REAL(KIND=JPRB),ALLOCATABLE :: GELAM(:)
REAL(KIND=JPRB),ALLOCATABLE :: GELAT(:)
REAL(KIND=JPRB),ALLOCATABLE :: GECLO(:)
REAL(KIND=JPRB),ALLOCATABLE :: GESLO(:)
LOGICAL,ALLOCATABLE :: LMASK(:)

!      ----------------------------------------------------------------
END MODULE YOMGC1S
