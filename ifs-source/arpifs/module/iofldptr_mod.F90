MODULE IOFLDPTR_MOD

!**** *IOFLDPTR_MOD*  - Field descriptor and pointer for IO

!     Author. 
!     ------- 
!      Philippe Marguinaud *METEO-FRANCE*
!      Original : 15-04-2016


USE PARKIND1, ONLY : JPRB
USE IOFLDDESC_MOD, ONLY : IOFLDDESC

IMPLICIT NONE

! Field descriptor for IO

TYPE IOFLDPTR
  TYPE (IOFLDDESC) :: YFLDDSC
  REAL (KIND=JPRB), POINTER :: ZFLDGP (:,:) => NULL () ! NPROMA, NBLOCKS
  REAL (KIND=JPRB), POINTER :: ZFLDSP (:)   => NULL () ! NSPEC2
  LOGICAL                   :: LDRDGP       = .FALSE.
  LOGICAL                   :: LSKIPF       = .FALSE.
END TYPE IOFLDPTR

SAVE

END MODULE IOFLDPTR_MOD

