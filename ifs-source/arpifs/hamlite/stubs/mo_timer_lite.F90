MODULE mo_timer_lite
  IMPLICIT NONE
  PUBLIC :: new_timer
CONTAINS
  FUNCTION new_timer(name) RESULT(itimer)
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER :: itimer
    itimer = 0  ! stub: timers disabled in OIFS
  END FUNCTION new_timer
END MODULE mo_timer_lite
