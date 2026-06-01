!> Interface to run JSBACH for one time step
!> eehol: SOME DEFAULT VALUES REPRESENTING JSBACH for BOXMODEL!

MODULE mo_jsb_interface_lite

  USE mo_kind_lite,                ONLY: wp

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: jsbach_get_var_1d, jsbach_get_var_2d

  CHARACTER(len=*), PARAMETER :: modname = 'mo_jsb_interface'

CONTAINS

  ! ====================================================================================================== !
  !
  !> Routine to be called from host model in order to inquire a variable (section) on a specified tile
  !
  SUBROUTINE jsbach_get_var_1d(var_name, model_id,  &
                 ics, ice, iblk, kproma, ptr1d)                         

    INTEGER, INTENT(IN) :: ics, ice, iblk, kproma, model_id
    CHARACTER(len=*), INTENT(IN) :: var_name
    REAL(wp), POINTER, INTENT(INOUT) :: ptr1d(:)
    !REAL(wp), TARGET :: value

    IF (.NOT. ASSOCIATED(ptr1d)) THEN
      WRITE(*,*) 'ERROR: ptr1d is not allocated before call!'
      RETURN
    END IF

    IF (var_name == "hydro_fract_snow") THEN
        ptr1d(:) = 0.01_wp
    END IF

  END SUBROUTINE jsbach_get_var_1d

!--------------------------------

  SUBROUTINE jsbach_get_var_2d(var_name, model_id,  &
             ics, ice, iblk, kproma, ptr2d)                         

    INTEGER, INTENT(IN) :: ics, ice, iblk, kproma, model_id
    CHARACTER(len=*), INTENT(IN) :: var_name
    REAL(wp), POINTER, INTENT(INOUT) :: ptr2d(:,:)

    IF (.NOT. ASSOCIATED(ptr2d)) THEN
      WRITE(*,*) 'ERROR: ptr2d is not allocated before call!'
      RETURN
    END IF

    IF (var_name == "hydro_wtr_soil_sl") THEN
        ptr2d(:,:) = 0.0025_wp
    ELSEIF (var_name == "hydro_wtr_soil_fc_sl") THEN
        ptr2d(:,:) = 0.0125_wp
    END IF

  END SUBROUTINE jsbach_get_var_2d
  
END MODULE mo_jsb_interface_lite