MODULE mo_cf_convention_lite
  IMPLICIT NONE
  PUBLIC :: t_cf_var
  TYPE :: t_cf_var
    CHARACTER(LEN=256) :: standard_name = ''
    CHARACTER(LEN=256) :: long_name = ''
    CHARACTER(LEN=64)  :: units = ''
    INTEGER :: datatype = 0
  END TYPE t_cf_var
END MODULE mo_cf_convention_lite
