MODULE mo_var_lite
  USE mo_kind_lite, ONLY: wp
  IMPLICIT NONE
  PUBLIC :: level_type_ml, t_var, t_var_ptr
  INTEGER, PARAMETER :: level_type_ml = 1

  TYPE :: t_var
    REAL(wp), POINTER :: r_ptr(:,:,:,:,:) => NULL()
  END TYPE t_var

  TYPE :: t_var_ptr
    TYPE(t_var), POINTER :: p => NULL()
  END TYPE t_var_ptr
END MODULE mo_var_lite
