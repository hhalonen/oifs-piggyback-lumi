! 
! based on mo_config_util.f90 
! in https://git.iac.ethz.ch/hammoz/icon-hammoz.git (hammoz/gpu/master)
!
! Philipp Weiss, University of Oxford  
!
! ICON
!
! ---------------------------------------------------------------
! Copyright (C) 2004-2024, DWD, MPI-M, DKRZ, KIT, ETH, MeteoSwiss
! Contact information: icon-model.org
!
! See AUTHORS.TXT for a list of authors
! See LICENSES/ for license information
! SPDX-License-Identifier: BSD-3-Clause
! ---------------------------------------------------------------

MODULE mo_util_config_lite

  USE mo_kind_lite, ONLY: dp
  USE mo_exception_lite,           ONLY: finish

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: set_config, set_to_missing

  INTEGER  :: missing_val_int = -999
  REAL(dp) :: missing_val_dp  = -999_dp  

  INTERFACE set_config
    MODULE PROCEDURE set_config_logical
    MODULE PROCEDURE set_config_string
    MODULE PROCEDURE set_config_int
    MODULE PROCEDURE set_config_dp
    MODULE PROCEDURE set_config_int_1d
    MODULE PROCEDURE set_config_dp_1d
    MODULE PROCEDURE set_config_logical_1d
    MODULE PROCEDURE set_config_int_2d
    MODULE PROCEDURE set_config_dp_2d
    MODULE PROCEDURE set_config_string_1d
  END INTERFACE 
 
  INTERFACE ismissing
    MODULE PROCEDURE ismissing_int
    MODULE PROCEDURE ismissing_dp
    MODULE PROCEDURE ismissing_string
  END INTERFACE 

  INTERFACE set_to_missing
    MODULE PROCEDURE set_to_missing_int
    MODULE PROCEDURE set_to_missing_dp
    MODULE PROCEDURE set_to_missing_string
    MODULE PROCEDURE set_to_missing_int_1d
    MODULE PROCEDURE set_to_missing_dp_1d
    MODULE PROCEDURE set_to_missing_int_2d
    MODULE PROCEDURE set_to_missing_dp_2d
  END INTERFACE 

  CONTAINS
 
!---------------------------------------------------------------------------------------------------

  SUBROUTINE set_config_logical( var, cvar)
    LOGICAL, INTENT(OUT) :: var
    LOGICAL, INTENT(IN)  :: cvar
    var=cvar
  END SUBROUTINE set_config_logical

!---------------------------------------------------------------------------------------------------

  SUBROUTINE set_config_string( var, cvar)
    CHARACTER(LEN=*), INTENT(INOUT) :: var, cvar
    IF ( .NOT. ismissing(cvar) ) THEN
      var=cvar
    ELSE
      cvar=var
    END IF
  END SUBROUTINE set_config_string

!---------------------------------------------------------------------------------------------------

  SUBROUTINE set_config_int( var, cvar)
    INTEGER, INTENT(INOUT) :: var, cvar
    IF ( .NOT. ismissing(cvar) ) THEN
      var=cvar
    ELSE
      cvar=var
    END IF
  END SUBROUTINE set_config_int

!---------------------------------------------------------------------------------------------------

  SUBROUTINE set_config_dp( var, cvar)
    REAL(dp), INTENT(INOUT) :: var, cvar
    IF ( .NOT. ismissing(cvar) ) THEN
      var=cvar
    ELSE
      cvar=var
    END IF
  END SUBROUTINE set_config_dp

!---------------------------------------------------------------------------------------------------

  SUBROUTINE set_config_logical_1d( var, cvar)
    LOGICAL, DIMENSION(:), INTENT(INOUT) :: var, cvar
    IF ( SIZE(var) .NE. SIZE(cvar) ) THEN
      CALL finish('set_config (logical, 1d)', 'size mismatch')
    END IF
    var=cvar
  END SUBROUTINE set_config_logical_1d

!---------------------------------------------------------------------------------------------------

  SUBROUTINE set_config_string_1d( var, cvar )
    CHARACTER(LEN=*), DIMENSION(:), INTENT(INOUT) :: var, cvar
    INTEGER :: i
    IF ( SIZE(var) .NE. SIZE(cvar) ) THEN
      CALL finish('set_config (string, 1d)', 'size mismatch')
    END IF
    DO i=1,SIZE(var)
      IF ( .NOT. ismissing(cvar(i)) ) THEN
        var(i)=cvar(i)
      ELSE
        cvar(i)=var(i)
      END IF
    END DO
  END SUBROUTINE set_config_string_1d

!---------------------------------------------------------------------------------------------------

  SUBROUTINE set_config_int_1d( var, cvar)
    INTEGER, DIMENSION(:), INTENT(INOUT) :: var, cvar
    INTEGER :: i
    IF ( SIZE(var) .NE. SIZE(cvar) ) THEN
      CALL finish('set_config (int, 1d)', 'size mismatch')
    END IF
    DO i=1,SIZE(var)
      IF ( .NOT. ismissing(cvar(i)) ) THEN
        var(i)=cvar(i)
      ELSE
        cvar(i)=var(i)
      END IF
    END DO
  END SUBROUTINE set_config_int_1d

!---------------------------------------------------------------------------------------------------

  SUBROUTINE set_config_int_2d( var, cvar)
    INTEGER, DIMENSION(:,:), INTENT(INOUT) :: var, cvar
    INTEGER :: i,j
    IF ( SIZE(var,1) .NE. SIZE(cvar,1) .OR. SIZE(var,2) .NE. SIZE(cvar,2) ) THEN
      CALL finish('set_config (int, 2d)', 'size mismatch')
    END IF
    DO j=1,SIZE(var,2)
      DO i=1,SIZE(var,1)
        IF ( .NOT. ismissing(cvar(i,j)) ) THEN
          var(i,j)=cvar(i,j)
        ELSE
          cvar(i,j)=var(i,j)
        END IF
      END DO
    END DO
  END SUBROUTINE set_config_int_2d

!---------------------------------------------------------------------------------------------------

  SUBROUTINE set_config_dp_1d( var, cvar)
    REAL(dp), DIMENSION(:), INTENT(INOUT) :: var, cvar
    INTEGER :: i
    IF ( SIZE(var) .NE. SIZE(cvar) ) THEN
      CALL finish('set_config (dp, 1d)', 'size mismatch')
    END IF
    DO i=1,SIZE(var)
      IF ( .NOT. ismissing(cvar(i)) ) THEN
        var(i)=cvar(i)
      ELSE
        cvar(i)=var(i)
      END IF
    END DO
  END SUBROUTINE set_config_dp_1d

!---------------------------------------------------------------------------------------------------

  SUBROUTINE set_config_dp_2d( var, cvar)
    REAL(dp), DIMENSION(:,:), INTENT(INOUT) :: var, cvar
    INTEGER :: i,j
    IF ( SIZE(var,1) .NE. SIZE(cvar,1) .OR. SIZE(var,2) .NE. SIZE(cvar,2) ) THEN
      CALL finish('set_config (dp, 2d)', 'size mismatch')
    END IF
    DO j=1,SIZE(var,2)
      DO i=1,SIZE(var,1)
        IF ( .NOT. ismissing(cvar(i,j)) ) THEN
          var(i,j)=cvar(i,j)
        ELSE
          cvar(i,j)=var(i,j)
        END IF
      END DO
    END DO
  END SUBROUTINE set_config_dp_2d

!---------------------------------------------------------------------------------------------------

  LOGICAL FUNCTION ismissing_int( var )
    INTEGER, INTENT(IN ) :: var
    LOGICAL :: ismissing
    IF ( var .EQ. missing_val_int ) THEN
      ismissing_int=.True.
    ELSE
      ismissing_int=.False.
    END IF
  END FUNCTION ismissing_int

!---------------------------------------------------------------------------------------------------
  
  LOGICAL FUNCTION ismissing_dp( var )
    REAL(dp), INTENT(IN ) :: var
    LOGICAL :: ismissing
    IF ( var .EQ. missing_val_dp ) THEN
      ismissing_dp=.True.
    ELSE
      ismissing_dp=.False.
    END IF
  END FUNCTION ismissing_dp

!---------------------------------------------------------------------------------------------------

  LOGICAL FUNCTION ismissing_string( var )
    CHARACTER(len=*), INTENT(IN ) :: var
    LOGICAL :: ismissing
    IF ( TRIM(var) .EQ. ''  ) THEN
      ismissing_string=.True.
    ELSE
      ismissing_string=.False.
    END IF
  END FUNCTION ismissing_string

!---------------------------------------------------------------------------------------------------

  SUBROUTINE set_to_missing_int( var )
    INTEGER, INTENT(INOUT ) :: var
    var=missing_val_int
  END SUBROUTINE set_to_missing_int
 
!---------------------------------------------------------------------------------------------------

  SUBROUTINE set_to_missing_dp( var )
    REAL(dp), INTENT(INOUT ) :: var
    var=missing_val_dp
  END SUBROUTINE set_to_missing_dp

!---------------------------------------------------------------------------------------------------
  
  SUBROUTINE set_to_missing_string( var )
    CHARACTER(LEN=*), INTENT(INOUT ) :: var
    var=''
  END SUBROUTINE set_to_missing_string

!---------------------------------------------------------------------------------------------------

  SUBROUTINE set_to_missing_int_1d( var ) 
    INTEGER, DIMENSION(:), INTENT(INOUT ) :: var
    var(:)=missing_val_int
  END SUBROUTINE set_to_missing_int_1d
 
!---------------------------------------------------------------------------------------------------

  SUBROUTINE set_to_missing_dp_1d( var )
    REAL(dp), DIMENSION(:), INTENT(INOUT ) :: var
    var(:)=missing_val_dp
  END SUBROUTINE set_to_missing_dp_1d

!---------------------------------------------------------------------------------------------------

  SUBROUTINE set_to_missing_int_2d( var )
    INTEGER, DIMENSION(:,:), INTENT(INOUT ) :: var
    var(:,:)=missing_val_int
  END SUBROUTINE set_to_missing_int_2d

!---------------------------------------------------------------------------------------------------
  
  SUBROUTINE set_to_missing_dp_2d( var )
    REAL(dp), DIMENSION(:,:), INTENT(INOUT ) :: var
    var(:,:)=missing_val_dp
  END SUBROUTINE set_to_missing_dp_2d

END MODULE mo_util_config_lite
