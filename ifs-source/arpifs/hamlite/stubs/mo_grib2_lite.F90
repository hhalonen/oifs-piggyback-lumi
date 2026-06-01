MODULE mo_grib2_lite
  IMPLICIT NONE
  PUBLIC :: t_grib2_var, grib2_var
  TYPE :: t_grib2_var
    INTEGER :: discipline = 0
    INTEGER :: category = 0
    INTEGER :: number = 0
  END TYPE t_grib2_var
CONTAINS
  FUNCTION grib2_var(disc, cat, num, tabtype, tabver) RESULT(g)
    INTEGER, INTENT(IN) :: disc, cat, num, tabtype, tabver
    TYPE(t_grib2_var) :: g
    g%discipline = disc; g%category = cat; g%number = num
  END FUNCTION grib2_var
END MODULE mo_grib2_lite
