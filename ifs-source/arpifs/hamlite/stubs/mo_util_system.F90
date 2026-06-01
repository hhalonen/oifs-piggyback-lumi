MODULE mo_util_system
  IMPLICIT NONE
  PUBLIC :: util_exit
CONTAINS
  SUBROUTINE util_exit(iexit)
    INTEGER, INTENT(IN) :: iexit
    STOP 1
  END SUBROUTINE util_exit
END MODULE mo_util_system
