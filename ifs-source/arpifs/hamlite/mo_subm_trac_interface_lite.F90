! HAMlite tracer interface - OIFS version
! In ICON, this module registers HAMlite tracers in the ICON variable system.
! In OIFS, tracer registration is handled by hamlite_init.F90 via GFL/YCHEM.
! These routines are kept as no-ops to satisfy USE dependencies.
MODULE mo_subm_trac_interface_lite

  USE mo_exception_lite,  ONLY: message, message_text
  USE mo_subm_trac_lite,  ONLY: trlist, ntrac

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: init_tracer_subm, new_prog_list_subm, construct_memory_subm

CONTAINS

  SUBROUTINE init_tracer_subm( jg, ntracer )
    INTEGER, INTENT(IN)    :: jg
    INTEGER, INTENT(INOUT) :: ntracer
    ! In OIFS, tracers are registered via GFL in hamlite_init.F90
    ntracer = ntracer + ntrac
    WRITE(message_text,'(a,i3,a,i3)') 'init_tracer_subm(OIFS): ntracer increased by ', ntrac, ' to ', ntracer
    CALL message('init_tracer_subm', message_text)
  END SUBROUTINE init_tracer_subm

  SUBROUTINE new_prog_list_subm(jg)
    INTEGER, INTENT(IN) :: jg
    ! No-op in OIFS - tracers managed by GFL
  END SUBROUTINE new_prog_list_subm

  SUBROUTINE construct_memory_subm( ntracer )
    INTEGER, INTENT(IN) :: ntracer
    ! No-op in OIFS - memory managed by GFL
  END SUBROUTINE construct_memory_subm

END MODULE mo_subm_trac_interface_lite
