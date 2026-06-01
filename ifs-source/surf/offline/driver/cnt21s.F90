SUBROUTINE CNT21S

!**** *CNT21S*  -  Controls integration job at level 2.

!     Purpose.
!     --------
!           Controls integration job at level 2.

!**   Interface.
!     ----------
!        *CALL* *CNT21S

!        Explicit arguments :
!        --------------------
!        None

!        Implicit arguments :
!        --------------------
!        None

!     Method.
!     -------
!        See documentation

!     Externals.   CNT31S - controls integration on level 3
!     ---------

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the 1D surface model

!     Author.
!     -------
!        Jean-Francois Mahfouf  *ECMWF*

!     Modifications.
!     --------------
!        Original : 95-03-01

!     ------------------------------------------------------------------

IMPLICIT NONE

#include "cnt31s.intfb.h"

!*       1.    CALL LEVEL 3 CONTROL ROUTINE.
!              -----------------------------

CALL CNT31S

!     ------------------------------------------------------------------

RETURN
END SUBROUTINE CNT21S


