MODULE YOMGPPCB

USE PARKIND1, ONLY : JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!*    Buffer for gridpoint workfile needed for PC schemes 
!      This is an obsolescent buffer, which was used for NH dynamics
!      in the past, and which is now used also in the hydrostatic
!      dynamics: its purpose is to pass some quantities from predictor
!      to corrector step if one of the PC schemes is switched on.
!      For this reason it has been renamed in January 2008.
!     This buffer should later disappear, its content should be
!      moved in the buffers GMV, GMVS and GFL (new attributes to create).

! GPPCBUF  : buffer

REAL (KIND=JPRB), POINTER :: GPPCBUF (:,:,:) => NULL ()

!      containing pointers

!     ------------------------------------------------------------------
END MODULE YOMGPPCB

