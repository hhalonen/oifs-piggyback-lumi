MODULE comin_host_interface
  IMPLICIT NONE
  PUBLIC
  INTEGER, PARAMETER :: EP_FINISH = 0
  INTEGER, PARAMETER :: COMIN_DOMAIN_OUTSIDE_LOOP = 0
CONTAINS
  SUBROUTINE comin_callback_context_call(i1, i2, lacc)
    INTEGER, INTENT(IN) :: i1, i2
    LOGICAL, INTENT(IN), OPTIONAL :: lacc
    ! stub: no ICON plugins in OIFS
  END SUBROUTINE comin_callback_context_call
  SUBROUTINE mpi_handshake(comm, group_names, group_comms)
    INTEGER, INTENT(INOUT) :: comm
    CHARACTER(LEN=*), INTENT(IN) :: group_names(:)
    INTEGER, INTENT(OUT) :: group_comms(:)
    group_comms = comm
  END SUBROUTINE mpi_handshake
END MODULE comin_host_interface
