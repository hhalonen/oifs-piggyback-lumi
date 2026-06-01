! 
! based on mo_subm_get_from_jsbach.f90 
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

MODULE mo_subm_jsbach_lite

  USE mo_kind_lite,          ONLY: wp
  USE mo_subm_bcs_lite,      ONLY: bc_set
  USE mo_exception_lite,     ONLY: finish

  IMPLICIT NONE
  PRIVATE
    
  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: get_from_jsbach 

  !--------------------
  ! module parameters
  !--------------------

  CHARACTER(LEN=*), PARAMETER :: thismodule = 'mo_subm_jsbach'

  CONTAINS

!---------------------------------------------------------------------------------------------------

  SUBROUTINE get_from_jsbach( jg, jb, jcs, jce, kblks_c, kproma, ibc, jsbach_process, jsbach_ndim  )

    !eehol: this is modified to boxmodel purposes

    !USE mo_aes_phy_config,  ONLY: aes_phy_config
    USE mo_jsb_interface_lite,   ONLY: jsbach_get_var_1d, jsbach_get_var_2d

    INTEGER, INTENT(IN)          :: jg, jb, jcs, jce, ibc, kblks_c, kproma
    INTEGER, INTENT(IN)          :: jsbach_ndim       ! dimension of JSBACH variable
    CHARACTER(len=*), INTENT(IN) :: jsbach_process    ! name of JSBACH process   
    INTEGER :: jsbach_model_id 
    REAL(wp), POINTER :: jsbach_var1d(:), jsbach_var2d(:,:)
    LOGICAL :: ljsb = .TRUE.

    jsbach_model_id = 1  

    !eehol: allocate simplified varibles
    ALLOCATE(jsbach_var1d(kproma))
    ALLOCATE(jsbach_var2d(kproma,kblks_c))

    !IF (aes_phy_config(jg)%ljsb) THEN
    IF (ljsb) THEN

      !jsbach_var1d => NULL()
      !jsbach_var2d => NULL()

      SELECT CASE(jsbach_ndim)
        CASE(2) 
          !$ACC ENTER DATA ATTACH(jsbach_var1d) ASYNC(1) 
! #ifdef _OPENACC
!           CALL jsbach_get_var(  var_name=jsbach_process, model_id=jsbach_model_id,  &
!             ics=jcs, ice=jce, iblk=jb, ptr1d=jsbach_var1d, lacc=.TRUE.              )
! #else 
!           CALL jsbach_get_var(  var_name=jsbach_process, model_id=jsbach_model_id,  &
!             ics=jcs, ice=jce, iblk=jb, ptr1d=jsbach_var1d                           )
! #endif
          !$ACC UPDATE HOST (jsbach_var1d) ASYNC(1) 
          !$ACC WAIT(1)
          !$ACC EXIT DATA DETACH(jsbach_var1d) ASYNC(1)

          CALL jsbach_get_var_1d(var_name=jsbach_process, model_id=jsbach_model_id,  &
            ics=jcs, ice=jce, iblk=kblks_c, kproma=kproma, ptr1d=jsbach_var1d )

          CALL bc_set(jg, jb, jcs, jce, ibc, kblks_c, kproma, jsbach_var1d)
        CASE(3)  
          !$ACC ENTER DATA ATTACH(jsbach_var2d) ASYNC(1) 
! #ifdef _OPENACC
!           CALL jsbach_get_var(  var_name=jsbach_process, model_id=jsbach_model_id,  &
!             ics=jcs, ice=jce, iblk=jb, ptr2d=jsbach_var2d, lacc=.TRUE.              )
! #else 
!           CALL jsbach_get_var(  var_name=jsbach_process, model_id=jsbach_model_id,  &
!             ics=jcs, ice=jce, iblk=jb, ptr2d=jsbach_var2d                           )
! #endif 
          ! pass only first level 
          !$ACC UPDATE HOST (jsbach_var2d) ASYNC(1) 
          !$ACC WAIT(1)
          !$ACC EXIT DATA DETACH(jsbach_var2d) ASYNC(1) 

          CALL jsbach_get_var_2d(var_name=jsbach_process, model_id=jsbach_model_id,  &
            ics=jcs, ice=jce, iblk=kblks_c, kproma=kproma, ptr2d=jsbach_var2d )

          CALL bc_set(jg, jb, jcs, jce, ibc, kblks_c, kproma, jsbach_var2d(:,1))
        CASE DEFAULT
          CALL finish(thismodule, "dimension not implemented")
        END SELECT

    ELSE

      CALL finish(thismodule, "pointer to jsbach variables needed")

    ENDIF  

  END SUBROUTINE get_from_jsbach

END MODULE mo_subm_jsbach_lite
