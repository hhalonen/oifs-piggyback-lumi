MODULE YOMINI

IMPLICIT NONE

SAVE

!**************  COMDECK YOMINI  ************

!     LDFI :     TRUE if digital filter initialization

!     LBIAS :    COMPUTING INITIALIZATION INCREMENT (Xi-Xa)
!     LINCR :    INCREMENTAL INITIALIZATION ( -> Xi-Z , Z=Yi-Ya )

!     LINITER :  TRUE during initialization, FALSE else (internal switch)
!     LSCRINI :  TRUE if screening prepared in digital filter finalisation

LOGICAL :: LDFI
LOGICAL :: LBIAS
LOGICAL :: LINCR
LOGICAL :: LINITER
LOGICAL :: LSCRINI

END MODULE YOMINI
