MODULE yaxt
  IMPLICIT NONE
  PUBLIC :: xt_initialize, xt_initialized
CONTAINS
  SUBROUTINE xt_initialize(comm)
    INTEGER, INTENT(IN) :: comm
    ! stub
  END SUBROUTINE xt_initialize
  FUNCTION xt_initialized() RESULT(linit)
    LOGICAL :: linit
    linit = .TRUE.
  END FUNCTION xt_initialized
END MODULE yaxt
