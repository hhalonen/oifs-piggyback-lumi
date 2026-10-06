! 
! based on mo_submodel.f90  
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

MODULE mo_subm_main_lite

  USE mo_kind_lite,                      ONLY: wp
  USE mo_exception_lite,                 ONLY: finish, message, message_text 
  USE mo_subm_trac_para_lite,            ONLY: ln  
  USE mo_subm_main_list_config_lite,     ONLY: subm_main_list

  IMPLICIT NONE
  PRIVATE
  
  !--------------------
  ! module interface
  !--------------------
 
  PUBLIC :: new_submodel                   ! define a new submodel
  PUBLIC :: set_submodel                   ! read namelist   

  PUBLIC :: print_status                   ! utility routines
   
  PUBLIC :: lhamlite                        ! central switch for aerosol module  
  PUBLIC :: lemissions                      ! switch for emission diagnostics
  PUBLIC :: ldrydep                         ! switch for dry diagnostics
  PUBLIC :: lwetdep                         ! switch for wet diagnostics
  PUBLIC :: lsedimentation                  ! switch for sedimentation 
  PUBLIC :: lburden                         ! switch for burden diagnostics
  PUBLIC :: lradiation                      ! switch for radiation
  
 
  PUBLIC :: emi_basepath
  PUBLIC :: emi_scenario  
 
  PUBLIC :: id_ham  
  
  !--------------------
  ! module parameters
  !--------------------

  INTEGER, PARAMETER :: nmaxsubm = 10     ! maximum number of submodels

  !--------------------
  ! module variables
  !--------------------
 
  INTEGER :: id_ham
  INTEGER, SAVE :: nsubm = 0              ! number of submodels defined  

  TYPE t_submlist
    CHARACTER(len=ln) :: modulename ! name of sub-model 
  END TYPE t_submlist
 
  TYPE(t_submlist), SAVE :: submlist (nmaxsubm) ! submodel list

  ! submodel master switch: default false, set to true if any submodel registers 
  LOGICAL :: lburden       
  LOGICAL :: lhamlite                    
  LOGICAL :: lemissions     
  LOGICAL :: ldrydep      
  LOGICAL :: lwetdep       
  LOGICAL :: lsedimentation  
  LOGICAL :: lradiation
  
  CHARACTER(LEN=256) :: emi_basepath 
  CHARACTER(LEN=8)   :: emi_scenario 
  
  CONTAINS

!---------------------------------------------------------------------------------------------------

  SUBROUTINE new_submodel(name,id)

    USE mo_util_string_lite,    ONLY: toupper 

    CHARACTER (len=*) ,INTENT(in)  :: name
    INTEGER           ,INTENT(out) ,OPTIONAL :: id ! index of submodel, 0 if undefined

    IF (PRESENT(id)) id = 0

    nsubm = nsubm + 1

    IF (nsubm > nmaxsubm) THEN
      WRITE(message_text,*) 'submodel list full, nmaxsubm = ', nmaxsubm
      CALL finish ('new_submodel', message_text)
    END IF

    IF (PRESENT(id)) id = nsubm
    submlist (nsubm)% modulename = toupper(name) 
    
  END SUBROUTINE new_submodel

!---------------------------------------------------------------------------------------------------

  SUBROUTINE set_submodel
    
    USE mo_util_config_lite,           ONLY: set_config
    USE mo_util_string_lite,           ONLY: separator  
    USE mo_subm_main_list_config_lite, ONLY: subm_main_list

    CALL message('', ' ') 
    CALL message('', separator)
    CALL message('', '|') 
    CALL message('', '| set_submodel ') 
    CALL message('', '|')  
    
    ! submodel list
    submlist (:)% modulename = '' 
  
    ! submodel switches  
    CALL set_config( lhamlite , subm_main_list % lhamlite )   

    ! generic process switches 
    CALL set_config( ldrydep , subm_main_list % ldrydep )
    CALL set_config( lwetdep , subm_main_list % lwetdep )
    CALL set_config( lemissions , subm_main_list % lemissions ) 
    CALL set_config( lsedimentation , subm_main_list % lsedimentation )  
    CALL set_config( lradiation , subm_main_list % lradiation )  

    ! coupling and diagnostic switches  
    CALL set_config( lburden , subm_main_list % lburden )
    CALL set_config( emi_basepath , subm_main_list % emi_basepath )
    CALL set_config( emi_scenario , subm_main_list % emi_scenario )

    ! register submodel    
    IF (lhamlite) CALL new_submodel( 'ham', id_ham )  

    ! report submodel status  
    IF (nsubm > 0) THEN
      WRITE (message_text,*) nsubm, ' submodels registered'
    ELSE
      WRITE (message_text,*) 'no submodels registered'
    END IF
    
    CALL message('', message_text)
    CALL print_status('ham-lite aerosol module', lhamlite) 
    CALL print_status('burden diagnostics', lburden)  

    IF (lhamlite) THEN 
      IF ( .NOT. lemissions )     CALL message('', 'Run with lemissions = false !')
      IF ( .NOT. ldrydep )        CALL message('', 'Run with ldrydep = false !')
      IF ( .NOT. lwetdep )        CALL message('', 'Run with lwetdep = false !')
      IF ( .NOT. lsedimentation ) CALL message('', 'Run with lsedimentation = false !') 
      IF ( .NOT. lradiation ) CALL message('', 'Run with lradiation = false !')
    END IF
 
  END SUBROUTINE set_submodel

!---------------------------------------------------------------------------------------------------

  SUBROUTINE print_status (mstring, flag)

    IMPLICIT NONE

    CHARACTER(len=*), intent(in)   :: mstring
    LOGICAL, intent(in)            :: flag

    IF ( flag ) THEN
      write(message_text,'(a60,1x,":",a)') mstring,'active'
    ELSE
      write(message_text,'(a60,1x,":",a)') mstring,'*not* active'
    END IF
    CALL message('', message_text)

  END SUBROUTINE print_status

END MODULE mo_subm_main_lite
