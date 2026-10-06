! 
! based on mo_activ.f90
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

MODULE mo_hammoz_lite_act

  USE mo_kind_lite,                ONLY: dp  
  USE mo_exception_lite,           ONLY: message, finish
  !USE mo_master_control,      ONLY: get_my_process_name
  USE mo_impl_constants_lite,      ONLY: SUCCESS, MAX_CHAR_LENGTH,  & 
    &                               VINTP_METHOD_PRES,         &
    &                               VINTP_METHOD_LIN,          &
    &                               VINTP_METHOD_LIN_NLEVP1
  !USE mo_var,                 ONLY: level_type_ml 
  ! USE mo_var_list_register,   ONLY: vlr_add, vlr_del
  ! USE mo_var_list,            ONLY: add_var, add_ref,          &
  !   &                               t_var_list => t_var_list_ptr
  ! USE mo_var_metadata,        ONLY: create_vert_interp_metadata, vintp_types
  ! USE mo_cf_convention,       ONLY: t_cf_var
  ! USE mo_grib2,               ONLY: t_grib2_var, grib2_var
  ! USE mo_cdi,                 ONLY: DATATYPE_PACK16, DATATYPE_PACK24,  &
  !   &                               DATATYPE_FLT32,  DATATYPE_FLT64,   &
  !   &                               GRID_UNSTRUCTURED,                 &
  !   &                               TSTEP_INSTANT, TSTEP_AVG,          &
  !   &                               TSTEP_ACCUM, cdiDefMissval
  ! USE mo_cdi_constants,       ONLY: GRID_UNSTRUCTURED_CELL, GRID_CELL
  ! USE mo_zaxis_type,          ONLY: ZA_HYBRID, ZA_HYBRID_HALF,         &
  !   &                               ZA_SURFACE
  ! USE mo_parallel_config,     ONLY: nproma
  ! USE mo_io_config,           ONLY: lnetcdf_flt64_output

#ifndef _OPENACC
#define __acc_attach(ptr)
#endif

  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: idt_cdnc, idt_icnc 
  PUBLIC :: activ_field 
  !PUBLIC :: init_act_stream 

  !--------------------
  ! module parameters
  !--------------------

  CHARACTER(LEN=*), PARAMETER :: thismodule = 'mo_hammoz_act'

  !--------------------
  ! module variables
  !--------------------

  INTEGER :: idt_cdnc, idt_icnc  
 
  TYPE t_activ_field
    REAL(dp),         POINTER :: swat(:,:,:) 
    REAL(dp),         POINTER :: reffl(:,:,:)
    REAL(dp),         POINTER :: reffi(:,:,:) 
    REAL(dp),         POINTER :: nsum(:,:,:)
    REAL(dp),         POINTER :: qnuc(:,:,:)  
    REAL(dp),         POINTER :: cdnc(:,:,:)
    REAL(dp),         POINTER :: icnc(:,:,:)
    REAL(dp),         POINTER :: base(:,:,:)   
    REAL(dp),         POINTER :: cdnc_2d(:,:)    
    REAL(dp),         POINTER :: cdnc_mx(:,:)  
    REAL(dp),         POINTER :: wa_2d(:,:)    
    REAL(dp),         POINTER :: wa_mx(:,:)  
    REAL(dp),         POINTER :: base_2d(:,:) 
  END TYPE t_activ_field

  TYPE(t_activ_field), ALLOCATABLE, TARGET :: activ_field(:) ! shape: (n_dom)

  !TYPE(t_var_list), ALLOCATABLE :: activ_field_list(:) ! shape: (n_dom) 
 
CONTAINS
  
!---------------------------------------------------------------------------------------------------

  ! SUBROUTINE init_act_stream( p_patch ) 
 
  !   USE mo_model_domain,            ONLY: t_patch  

  !   TYPE(t_patch), INTENT(IN), DIMENSION(:)  :: p_patch 

  !   CHARACTER(len=MAX_CHAR_LENGTH) :: listname
  !   INTEGER :: ndomain, jg, ist, nblks, nlev

  !   CALL message(TRIM(thismodule),'construction of act stream started')

  !   ndomain = SIZE(p_patch)

  !   ALLOCATE( activ_field(ndomain), STAT=ist)  
  !   IF (ist/=SUCCESS) CALL finish(TRIM(thismodule), 'allocation of activ_field failed')

  !   ALLOCATE( activ_field_list(ndomain), STAT=ist)  
  !   IF (ist/=SUCCESS) CALL finish(TRIM(thismodule), 'allocation of activ_field_list failed')
 
  !   DO jg = 1,ndomain

  !     nblks = p_patch(jg)%nblks_c
  !     nlev  = p_patch(jg)%nlev

  !     WRITE(listname,'(a,i2.2)') 'activ_state_diag_of_domain', jg
  !     CALL new_act_field_list( jg, nproma, nlev, nblks,                              &
  !                              TRIM(listname), activ_field_list(jg), activ_field(jg) )

  !   END DO

  !   CALL message(TRIM(thismodule),'construction of act stream finished')

  ! END SUBROUTINE init_act_stream

!---------------------------------------------------------------------------------------------------

  ! SUBROUTINE new_act_field_list(  jg, kproma, klev, kblks, listname, field_list, field  ) 

  !   INTEGER, INTENT(IN) :: jg  
  !   INTEGER, INTENT(IN) :: kproma, klev, kblks  
  !   CHARACTER(len=*),  INTENT(IN) :: listname
  !   TYPE(t_var_list),INTENT(INOUT) :: field_list
  !   TYPE(t_activ_field), INTENT(INOUT) :: field
 
  !   INTEGER :: ibits 
  !   INTEGER :: datatype_flt
  !   INTEGER :: shape2d(2), shape3d(3)
  !   TYPE(t_cf_var)    :: cf_desc
  !   TYPE(t_grib2_var) :: grib2_desc
  !   CHARACTER(:), ALLOCATABLE :: model_type

  !   ibits = DATATYPE_PACK16 
  !   datatype_flt = MERGE(DATATYPE_FLT64, DATATYPE_FLT32, lnetcdf_flt64_output)   
  !   shape2d  = (/kproma, kblks/)
  !   shape3d  = (/kproma, klev, kblks/) 
  !   model_type = get_my_process_name()

  !   !$ACC ENTER DATA COPYIN(field)

  !   CALL vlr_add( field_list, TRIM(listname), patch_id=jg,          &
  !                 vlevel_type=level_type_ml, model_type=model_type, &
  !                 lrestart=.TRUE.                                   )

  !   cf_desc    = t_cf_var('swat', '% [0-1]', 'icon supersaturation over water', datatype_flt)
  !   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
  !   CALL add_var( field_list, 'swat', field%swat,                                         &
  !               & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc, ldims=shape3d,  &
  !               & vert_interp=create_vert_interp_metadata(                                &
  !               &             vert_intp_type=vintp_types("P","Z","I"),                    &
  !               &             vert_intp_method=VINTP_METHOD_LIN,                          &
  !               &             l_loglin=.FALSE.,                                           &
  !               &             l_extrapol=.FALSE.),                                        &
  !               &  lrestart=.TRUE., lopenacc=.TRUE.                                       )
  !   __acc_attach(field%swat)

  !   cf_desc    = t_cf_var('reffl', 'um', 'cloud drop effective radius', datatype_flt)
  !   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
  !   CALL add_var( field_list, 'reffl', field%reffl,                                       &
  !               & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc, ldims=shape3d,  &
  !               & vert_interp=create_vert_interp_metadata(                                &
  !               &             vert_intp_type=vintp_types("P","Z","I"),                    &
  !               &             vert_intp_method=VINTP_METHOD_LIN,                          &
  !               &             l_loglin=.FALSE.,                                           &
  !               &             l_extrapol=.FALSE.),                                        &
  !               &  lrestart=.TRUE., lopenacc=.TRUE.                                       )
  !   __acc_attach(field%reffl)

  !   cf_desc    = t_cf_var('reffi', 'um', 'ice crystal effective radius', datatype_flt)
  !   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
  !   CALL add_var( field_list, 'reffi', field%reffi,                                       &
  !               & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc, ldims=shape3d,  &
  !               & vert_interp=create_vert_interp_metadata(                                &
  !               &             vert_intp_type=vintp_types("P","Z","I"),                    &
  !               &             vert_intp_method=VINTP_METHOD_LIN,                          &
  !               &             l_loglin=.FALSE.,                                           &
  !               &             l_extrapol=.FALSE.),                                        &
  !               &  lrestart=.TRUE., lopenacc=.TRUE.                                       )
  !   __acc_attach(field%reffi)
    
  !   cf_desc    = t_cf_var('nsum', 'm-3', 'aerosol number for activation', datatype_flt)
  !   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
  !   CALL add_var( field_list, 'nsum', field%nsum,                                         &
  !               & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc, ldims=shape3d,  &
  !               & vert_interp=create_vert_interp_metadata(                                &
  !               &             vert_intp_type=vintp_types("P","Z","I"),                    &
  !               &             vert_intp_method=VINTP_METHOD_LIN,                          &
  !               &             l_loglin=.FALSE.,                                           &
  !               &             l_extrapol=.FALSE.),                                        &
  !               &  lrestart=.TRUE., lopenacc=.TRUE.                                       )
  !   __acc_attach(field%nsum)

  !   cf_desc    = t_cf_var('qnuc', 'm-3 s-1', 'cd nucleation rate', datatype_flt)
  !   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
  !   CALL add_var( field_list, 'qnuc', field%qnuc,                                         &
  !               & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc, ldims=shape3d,  &
  !               & vert_interp=create_vert_interp_metadata(                                &
  !               &             vert_intp_type=vintp_types("P","Z","I"),                    &
  !               &             vert_intp_method=VINTP_METHOD_LIN,                          &
  !               &             l_loglin=.FALSE.,                                           &
  !               &             l_extrapol=.FALSE.),                                        &
  !               &  lrestart=.TRUE., lopenacc=.TRUE.                                       )
  !   __acc_attach(field%qnuc)
   
  !   cf_desc    = t_cf_var('cdnc', 'm-3', 'cdnc',  datatype_flt)
  !   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
  !   CALL add_var( field_list, 'cdnc', field%cdnc,                                         &
  !               & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc, ldims=shape3d,  &
  !               & vert_interp=create_vert_interp_metadata(                                &
  !               &             vert_intp_type=vintp_types("P","Z","I"),                    &
  !               &             vert_intp_method=VINTP_METHOD_LIN,                          &
  !               &             l_loglin=.FALSE.,                                           &
  !               &             l_extrapol=.FALSE.),                                        &
  !               &  lrestart=.TRUE., lopenacc=.TRUE.                                       )
  !   __acc_attach(field%cdnc)

  !   cf_desc    = t_cf_var('icnc', 'm-3', 'icnc', datatype_flt)
  !   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
  !   CALL add_var( field_list, 'icnc', field%icnc,                                         &
  !               & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc, ldims=shape3d,  &
  !               & vert_interp=create_vert_interp_metadata(                                &
  !               &             vert_intp_type=vintp_types("P","Z","I"),                    &
  !               &             vert_intp_method=VINTP_METHOD_LIN,                          &
  !               &             l_loglin=.FALSE.,                                           &
  !               &             l_extrapol=.FALSE.),                                        &
  !               &  lrestart=.TRUE., lopenacc=.TRUE.                                       )
  !   __acc_attach(field%icnc)

  !   cf_desc    = t_cf_var('base', '-', 'level of cloud base',  datatype_flt)
  !   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
  !   CALL add_var( field_list, 'base', field%base,                                         &
  !               & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc, ldims=shape3d,  &
  !               & vert_interp=create_vert_interp_metadata(                                &
  !               &             vert_intp_type=vintp_types("P","Z","I"),                    &
  !               &             vert_intp_method=VINTP_METHOD_LIN,                          &
  !               &             l_loglin=.FALSE.,                                           &
  !               &             l_extrapol=.FALSE.),                                        &
  !               &  lrestart=.TRUE., lopenacc=.TRUE.                                       )
  !   __acc_attach(field%base)

  !   cf_desc    = t_cf_var('cdnc_2d', 'm-3', 'average cdnc in cloud levels', datatype_flt)
  !   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
  !   CALL add_var( field_list, 'cdnc_2d', field%cdnc_2d,                                   &
  !               & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc, ldims=shape2d, &
  !               &  lrestart=.TRUE., lopenacc=.TRUE.                                       )
  !   __acc_attach(field%cdnc_2d)
    
  !   cf_desc    = t_cf_var('cdnc_mx', 'm-3', 'maximum cdnc in cloud levels', datatype_flt)
  !   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
  !   CALL add_var( field_list, 'cdnc_mx', field%cdnc_mx,                                   &
  !               & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc, ldims=shape2d, &
  !               &  lrestart=.TRUE., lopenacc=.TRUE.                                       )
  !   __acc_attach(field%cdnc_mx)

  !   cf_desc    = t_cf_var('wa_2d', 'm s-1', 'average updraft velocity', datatype_flt)
  !   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
  !   CALL add_var( field_list, 'wa_2d', field%wa_2d,                                       &
  !               & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc, ldims=shape2d, &
  !               &  lrestart=.TRUE., lopenacc=.TRUE.                                       )
  !   __acc_attach(field%wa_2d)
    
  !   cf_desc    = t_cf_var('wa_mx', 'm s-1', 'maximum updraft velocity', datatype_flt)
  !   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
  !   CALL add_var( field_list, 'wa_mx', field%wa_mx,                                       &
  !               & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc, ldims=shape2d, &
  !               &  lrestart=.TRUE., lopenacc=.TRUE.                                       )
  !   __acc_attach(field%wa_mx)

  !   cf_desc    = t_cf_var('base_2d', '-', 'average level of cloud base', datatype_flt)
  !   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL )
  !   CALL add_var( field_list, 'base_2d', field%base_2d,                                   &
  !               & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc, ldims=shape2d, &
  !               &  lrestart=.TRUE., lopenacc=.TRUE.                                       )
  !   __acc_attach(field%base_2d)

  ! END SUBROUTINE new_act_field_list

END MODULE mo_hammoz_lite_act