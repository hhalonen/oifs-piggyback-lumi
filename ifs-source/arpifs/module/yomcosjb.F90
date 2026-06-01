MODULE YOMCOSJB

USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!*    Cost function management of first guess constraint
!*    (for data assimilation)

! FJBCOST  :  VALUE OF THE COST-FUNCTION

! The following allows more control than LGUESS which
! switches off all of the following and Jq.
! LJBZERO : Runs 4D-Var without Jb
! LJPZERO : Runs 4D-Var without background term for parameters
!           (this is here because it can contain more than VARBC)
! LJHZERO : Runs 4D-Var without background term for parameters
! LJTZERO : Runs 4D-Var without background term for TOVS cv
! LJLZERO : Runs 4D-Var without background term for LELAM cv

REAL(KIND=JPRB) :: FJBCOST, FJECOST
LOGICAL :: LJBZERO, LJPZERO, LJHZERO, LJLZERO, LJTZERO

!     ------------------------------------------------------------------
END MODULE YOMCOSJB
