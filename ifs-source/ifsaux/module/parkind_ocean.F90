MODULE PARKIND_OCEAN
!
!     *** Define usual kinds ***
!
  
IMPLICIT NONE
SAVE
!
!     Integer Kinds
!     -------------
!


!
!     Real Kinds
!     ----------
!
#ifdef PARKIND1_SINGLE_NEMO
INTEGER, PARAMETER :: JPRO = SELECTED_REAL_KIND(6,37)
#else
INTEGER, PARAMETER :: JPRO = SELECTED_REAL_KIND(13,300)
#endif
!

END MODULE PARKIND_OCEAN
