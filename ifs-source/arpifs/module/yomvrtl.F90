MODULE YOMVRTL

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!*    Switches for variational assimilation: use of tangent linear model
! L131TL  : .T. = the incremental is to be run with the tangent model.
! LTLINT  : .T. = Flag telling if we are in the tangent integration
!                 of the model (incremental)
! LOBSTL  : .T. = we use the tangent linear of the observation operators
!                 (otherwise a finite-difference approximation is used)
LOGICAL :: L131TL
LOGICAL :: LTLINT
LOGICAL :: LOBSTL

!*    Alterations of the TL and AD models
LOGICAL :: LDRYTL       ! .T. = remove the coupling of q and T in the dynamics

!*    Global arrays and variables for the truncated Newton algorithm

LOGICAL :: LIDMODEL     ! .T. = Replace tl-model by identity operator

!     ------------------------------------------------------------------
END MODULE YOMVRTL
