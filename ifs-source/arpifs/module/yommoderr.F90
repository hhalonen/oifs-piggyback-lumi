MODULE YOMMODERR

!     Purpose.
!     --------
!       Data and controls for model error in 4D-Var.

!     Author.
!     -------
!       Y. Tremolet

!     Modifications.
!     --------------
!       Original    18-Mar-2004
!       M.Jidane 09-04-2006 Cy31 Phasing
!       M. Chrust   3-Jan-2020 OOPS cleaning
! ------------------------------------------------------------------

USE PARKIND1, ONLY: JPIM, JPRB
USE GRIDPOINT_FIELDS_MIX, ONLY: ASSIGNMENT(=), GRIDPOINT_FIELD
USE SPECTRAL_FIELDS_MOD, ONLY: ASSIGNMENT(=), SPECTRAL_FIELD
USE YOMMODERRCONF, ONLY : TMODERR_CONF

IMPLICIT NONE
SAVE

TYPE(TMODERR_CONF)  :: YGMODERRCONF     ! Configuration of model error

LOGICAL             :: LFGMODERR         ! Use first guess for model error
INTEGER(KIND=JPIM)  :: N_COUPLED_WINDOWS ! Number of coupled (sub)windows
REAL(KIND=JPRB)     :: WEAK4D_INTERV     ! Time interval between components (hours)

TYPE(GRIDPOINT_FIELD), ALLOCATABLE :: GPMODERR(:)   ! Gridpoint model error
TYPE(GRIDPOINT_FIELD), ALLOCATABLE :: GPFGMODERR(:) ! Grid point ME first guess
TYPE(SPECTRAL_FIELD), ALLOCATABLE  :: SPMODERR(:)   ! Spectral model error
TYPE(SPECTRAL_FIELD), ALLOCATABLE  :: SPFGMODERR(:) ! Spectral   ME first guess
TYPE(SPECTRAL_FIELD)               :: SPCTLMODERR   ! Spectral model error CV
TYPE(SPECTRAL_FIELD)               :: SPGPMODERR    ! Spectral ME for GP trans

END MODULE YOMMODERR
