MODULE mo_nwp_tuning_config_lite
  USE mo_kind_lite, ONLY: wp
  IMPLICIT NONE
  PUBLIC :: tune_supsat_limfac
  REAL(wp) :: tune_supsat_limfac = 2.0_wp
END MODULE mo_nwp_tuning_config_lite
