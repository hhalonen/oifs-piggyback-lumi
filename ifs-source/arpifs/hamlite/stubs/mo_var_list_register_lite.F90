MODULE mo_var_list_register_lite
  IMPLICIT NONE
  PUBLIC :: vlr_add
CONTAINS
  SUBROUTINE vlr_add(list, name, patch_id, vlc, lrestart, loutput, model_type)
    INTEGER, INTENT(OUT) :: list
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(IN), OPTIONAL :: patch_id, vlc, model_type
    LOGICAL, INTENT(IN), OPTIONAL :: lrestart, loutput
    list = 0  ! stub
  END SUBROUTINE vlr_add
END MODULE mo_var_list_register_lite
