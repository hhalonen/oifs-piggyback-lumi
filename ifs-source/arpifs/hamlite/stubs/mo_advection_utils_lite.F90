MODULE mo_advection_utils_lite
  IMPLICIT NONE
  PUBLIC :: add_tracer_ref
CONTAINS
  SUBROUTINE add_tracer_ref(p_prog_list, vname, idx)
    INTEGER, INTENT(IN) :: p_prog_list
    CHARACTER(LEN=*), INTENT(IN) :: vname
    INTEGER, INTENT(OUT) :: idx
    idx = 0  ! stub
  END SUBROUTINE add_tracer_ref
END MODULE mo_advection_utils_lite
