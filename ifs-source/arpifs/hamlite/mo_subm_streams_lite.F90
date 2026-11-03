! 
! based on mo_submodel_streams.f90 
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

MODULE mo_subm_streams_lite

  USE mtime_lite,                       ONLY: MAX_TIMEDELTA_STR_LEN
  USE mo_subm_streams_para_lite,   ONLY: nmaxstreamvars 
  USE mo_subm_trac_para_lite,      ONLY: nmaxtrac, ln 

  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: init_submodel_streams
 
  PUBLIC :: wetdep_lpost, wetdep_tinterval, wetdepnam, wetdep_keytype
  PUBLIC :: drydep_lpost, drydep_tinterval, drydepnam, drydep_keytype   
  PUBLIC :: sedi_lpost, sedi_tinterval, sedinam, sedi_keytype
  PUBLIC :: emi_lpost, emi_lpost_sector, emi_tinterval, eminam, emi_keytype

  !--------------------
  ! module variables
  !--------------------
 
  ! wet deposition
  INTEGER                                 :: wetdep_keytype
  LOGICAL                                 :: wetdep_lpost
  CHARACTER(LEN=MAX_TIMEDELTA_STR_LEN)    :: wetdep_tinterval
  CHARACTER(LEN=32)                       :: wetdepnam(nmaxstreamvars) 
  ! dry deposition
  INTEGER                                 :: drydep_keytype
  LOGICAL                                 :: drydep_lpost 
  CHARACTER(LEN=MAX_TIMEDELTA_STR_LEN)    :: drydep_tinterval
  CHARACTER(LEN=32)                       :: drydepnam(nmaxstreamvars)  
  ! sedimentation
  INTEGER                                 :: sedi_keytype
  LOGICAL                                 :: sedi_lpost
  CHARACTER(LEN=MAX_TIMEDELTA_STR_LEN)    :: sedi_tinterval
  CHARACTER(LEN=32)                       :: sedinam(nmaxstreamvars)
  ! emission
  INTEGER                                 :: emi_keytype
  LOGICAL                                 :: emi_lpost, emi_lpost_sector
  CHARACTER(LEN=MAX_TIMEDELTA_STR_LEN)    :: emi_tinterval
  CHARACTER(LEN=32)                       :: eminam(nmaxstreamvars)     

  CONTAINS

!---------------------------------------------------------------------------------------------------

  SUBROUTINE init_submodel_streams
 
    USE mo_exception_lite,                 ONLY: finish, message
    USE mo_util_string_lite,               ONLY: separator
    USE mo_subm_diag_list_config_lite,     ONLY: subm_diag_list
    USE mo_util_config_lite,               ONLY: set_config
    USE mo_subm_main_lite,                 ONLY: ldrydep, lemissions, lwetdep

    eminam(:)=''
    drydepnam(:)=''
    sedinam(:)=''
    eminam(:)=''
    wetdepnam(:)='' 

    CALL message('','')
    CALL message('',separator)
    CALL message('submodel_streams', 'import parameters from subm_diag_list')
  
    !--- wetdep
    CALL set_config( wetdep_lpost, subm_diag_list % wetdep_lpost )
    CALL set_config( wetdep_tinterval, subm_diag_list % wetdep_tinterval ) 
    CALL set_config( wetdepnam, subm_diag_list % wetdepnam ) 
    CALL set_config( wetdep_keytype, subm_diag_list % wetdep_keytype )

    !--- drydep
    CALL set_config( drydep_lpost, subm_diag_list % drydep_lpost )
    CALL set_config( drydep_tinterval, subm_diag_list % drydep_tinterval ) 
    CALL set_config( drydepnam, subm_diag_list % drydepnam ) 
    CALL set_config( drydep_keytype, subm_diag_list % drydep_keytype )  

    !--- sedimentation
    CALL set_config( sedi_lpost, subm_diag_list % sedi_lpost )
    CALL set_config( sedi_tinterval, subm_diag_list % sedi_tinterval ) 
    CALL set_config( sedinam, subm_diag_list % sedinam )
    CALL set_config( sedi_keytype, subm_diag_list % sedi_keytype )

    !--- emissions
    CALL set_config( emi_lpost, subm_diag_list % emi_lpost )
    CALL set_config( emi_lpost_sector, subm_diag_list % emi_lpost_sector )
    CALL set_config( emi_tinterval, subm_diag_list % emi_tinterval ) 
    CALL set_config( eminam, subm_diag_list % eminam ) 
    CALL set_config( emi_keytype, subm_diag_list % emi_keytype )

    !--- turn off lpost flags if submodel process flags are FALSE
    IF (drydep_lpost .AND. .NOT. ldrydep) THEN
      drydep_lpost = .FALSE.
      CALL message('init_submodel_streams', 'drydep_lpost = FALSE because ldrydep = FALSE')
    END IF
    IF (emi_lpost .AND. .NOT. lemissions) THEN
      emi_lpost = .FALSE.
      CALL message('init_submodel_streams', 'emi_lpost = FALSE because lemissions = FALSE')
    END IF
    IF (emi_lpost_sector .AND. .NOT. lemissions) THEN
      emi_lpost_sector = .FALSE.
      CALL message('init_submodel_streams', 'emi_lpost_sector = FALSE because lemissions = FALSE')
    END IF
    IF (wetdep_lpost .AND. .NOT. lwetdep) THEN
      wetdep_lpost = .FALSE.
      CALL message('init_submodel_streams', 'wetdep_lpost = FALSE because lwetdep = FALSE')
    END IF

  END SUBROUTINE init_submodel_streams

END MODULE mo_subm_streams_lite
