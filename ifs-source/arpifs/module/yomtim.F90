MODULE YOMTIM

USE PARKIND1  ,ONLY : JPRD

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!*    Time and date of the real world (both official and computer)

! RSTART : CPU time between the start of the job and last call to the timer
! RVSTART : vector CPU time between the start of the job and last call to the
! RTIMEF : wallclock time between the last two calls to the timer

REAL(KIND=JPRD) :: RSTART
REAL(KIND=JPRD) :: RVSTART
REAL(KIND=JPRD) :: RTIMEF

!     ------------------------------------------------------------------
END MODULE YOMTIM
