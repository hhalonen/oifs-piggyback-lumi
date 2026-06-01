MODULE mo_advection_config_lite
  IMPLICIT NONE
  PUBLIC :: t_advection_config, advection_config
  TYPE :: t_advection_config
    INTEGER :: npassive_tracer = 0
  END TYPE t_advection_config
  TYPE(t_advection_config), TARGET :: advection_config(1)
END MODULE mo_advection_config_lite
