MODULE mo_tracer_metadata_lite
  IMPLICIT NONE
  PUBLIC :: create_tracer_metadata
CONTAINS
  FUNCTION create_tracer_metadata(lis_tracer, name, lfeedback, ihadv_tracer, ivadv_tracer) RESULT(metadata)
    LOGICAL, INTENT(IN), OPTIONAL :: lis_tracer, lfeedback
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: name
    INTEGER, INTENT(IN), OPTIONAL :: ihadv_tracer, ivadv_tracer
    INTEGER :: metadata
    metadata = 0  ! stub
  END FUNCTION create_tracer_metadata
END MODULE mo_tracer_metadata_lite
