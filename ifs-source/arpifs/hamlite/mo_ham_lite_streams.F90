! ! 
! ! based on mo_ham_streams.f90 
! ! in https://git.iac.ethz.ch/hammoz/icon-hammoz.git (hammoz/gpu/master)
! !
! ! Philipp Weiss, University of Oxford  
! !
! ! ICON
! !
! ! ---------------------------------------------------------------
! ! Copyright (C) 2004-2024, DWD, MPI-M, DKRZ, KIT, ETH, MeteoSwiss
! ! Contact information: icon-model.org
! !
! ! See AUTHORS.TXT for a list of authors
! ! See LICENSES/ for license information
! ! SPDX-License-Identifier: BSD-3-Clause
! ! ---------------------------------------------------------------

! MODULE mo_ham_lite_streams

!   USE mo_kind_lite,                ONLY: dp
!   USE mo_exception_lite,           ONLY: message, finish
!   USE mo_master_control_lite,      ONLY: get_my_process_name
!   USE mo_impl_constants_lite,      ONLY: SUCCESS, MAX_CHAR_LENGTH,  & 
!     &                               VINTP_METHOD_PRES,         &
!     &                               VINTP_METHOD_LIN,          &
!     &                               VINTP_METHOD_LIN_NLEVP1
!   USE mo_var_lite,                 ONLY: level_type_ml 
!   USE mo_var_list_register_lite,   ONLY: vlr_add
!   USE mo_var_list_lite,            ONLY: add_var, add_ref,          &
!     &                               t_var_list=> t_var_list_ptr
!   USE mo_var_metadata_lite,        ONLY: create_vert_interp_metadata, vintp_types
!   USE mo_cf_convention_lite,       ONLY: t_cf_var
!   USE mo_grib2_lite,               ONLY: t_grib2_var, grib2_var
!   USE mo_cdi_lite,                 ONLY: DATATYPE_PACK16, DATATYPE_PACK24,  &
!     &                               DATATYPE_FLT32,  DATATYPE_FLT64,   &
!     &                               GRID_UNSTRUCTURED,                 &
!     &                               TSTEP_INSTANT, TSTEP_AVG,          &
!     &                               TSTEP_ACCUM, cdiDefMissval
!   USE mo_cdi_constants_lite,       ONLY: GRID_UNSTRUCTURED_CELL, GRID_CELL
!   USE mo_zaxis_type_lite,          ONLY: ZA_HYBRID, ZA_HYBRID_HALF,         &
!     &                               ZA_SURFACE
!   USE mo_parallel_config_lite,     ONLY: nproma
!   USE mo_io_config_lite,           ONLY: lnetcdf_flt64_output

!   USE mo_fortran_tools_lite,       ONLY: t_ptr_2d3d
!   USE mo_ham_lite_main_lite,       ONLY: nmaxclass, sizeclass 
!   USE mo_ham_lite_rad_data_lite,   ONLY: Nwv_tot

! # include "add_var_acc_macro.inc"

!   IMPLICIT NONE
!   PRIVATE

!   !--------------------
!   ! module interface
!   !--------------------

!   PUBLIC :: new_stream_ham, &     ! construct the ham streams
!             new_stream_ham_rad    ! construct the ham_rad stream

!   PUBLIC :: ham_field,  &
!             ham_rad_field

!   !--------------------
!   ! module parameters
!   !--------------------

!   CHARACTER(len=*), PARAMETER :: thismodule = 'mo_ham_lite_streams'

!   !--------------------
!   ! module variables
!   !-------------------- 

!   !! Derived data type: t_ham_field
!   !!
!   !! This structure contains HAM variables
!   !!
!   !! All components are arrays of one of the following shapes:
!   !! <ol>
!   !! <li> (nproma,           nblks_phy)
!   !! <li> (nproma, nlev_phy, nblks_phy)
!   !! <li> (nproma, nlev_phy, nblks_phy, ntracers)
!   !! </ol>
  
!   TYPE t_ham_field
 
!     TYPE (t_ptr_2d3d), ALLOCATABLE :: rwet(:)    
!     TYPE (t_ptr_2d3d), ALLOCATABLE :: daer(:)    
!     TYPE (t_ptr_2d3d), ALLOCATABLE :: rwet_2d(:)  
!     TYPE (t_ptr_2d3d), ALLOCATABLE :: daer_2d(:) 

!     REAL(dp), POINTER :: rwet4d(:,:,:,:)
!     REAL(dp), POINTER :: daer4d(:,:,:,:)
!     REAL(dp), POINTER :: rwet3d(:,:,:)
!     REAL(dp), POINTER :: daer3d(:,:,:)

!     REAL(dp), POINTER :: rhum(:,:,:)     
!     REAL(dp), POINTER :: rhum_2d(:,:)  
!     REAL(dp), POINTER :: temp_2d(:,:)  

!     !-- cloud activation
    
!     TYPE (t_ptr_2d3d), ALLOCATABLE :: nact(:) 
!     TYPE (t_ptr_2d3d), ALLOCATABLE :: nact_2d(:) 
!     TYPE (t_ptr_2d3d), ALLOCATABLE :: frac(:)
!     TYPE (t_ptr_2d3d), ALLOCATABLE :: frac_2d(:)
!     TYPE (t_ptr_2d3d), ALLOCATABLE :: a(:)
!     TYPE (t_ptr_2d3d), ALLOCATABLE :: b(:)
!     TYPE (t_ptr_2d3d), ALLOCATABLE :: sc(:)
!     TYPE (t_ptr_2d3d), ALLOCATABLE :: rc(:) 

!     REAL(dp), POINTER :: nact4d(:,:,:,:) 
!     REAL(dp), POINTER :: nact3d(:,:,:)  
!     REAL(dp), POINTER :: frac4d(:,:,:,:)
!     REAL(dp), POINTER :: frac3d(:,:,:)
!     REAL(dp), POINTER :: a4d(:,:,:,:)
!     REAL(dp), POINTER :: b4d(:,:,:,:)
!     REAL(dp), POINTER :: sc4d(:,:,:,:)
!     REAL(dp), POINTER :: rc4d(:,:,:,:) 

!   END TYPE t_ham_field

!   TYPE t_ham_rad_field

!     !-- radiation 

!     ! per mode
!     TYPE (t_ptr_2d3d) :: tau_mode(nmaxclass,Nwv_tot)
!     TYPE (t_ptr_2d3d) :: abs_mode(nmaxclass,Nwv_tot)
!     TYPE (t_ptr_2d3d) :: sigma_mode(nmaxclass,Nwv_tot)
!     TYPE (t_ptr_2d3d) :: omega_mode(nmaxclass,Nwv_tot)
!     TYPE (t_ptr_2d3d) :: asym_mode(nmaxclass,Nwv_tot)
!     TYPE (t_ptr_2d3d) :: nr_mode(nmaxclass,Nwv_tot)
!     TYPE (t_ptr_2d3d) :: ni_mode(nmaxclass,Nwv_tot)

!     REAL(dp), POINTER :: tau_mode4d(:,:,:,:) ! for container
!     REAL(dp), POINTER :: abs_mode4d(:,:,:,:)
!     REAL(dp), POINTER :: sigma_mode4d(:,:,:,:)
!     REAL(dp), POINTER :: omega_mode4d(:,:,:,:)
!     REAL(dp), POINTER :: asym_mode4d(:,:,:,:)
!     REAL(dp), POINTER :: nr_mode4d(:,:,:,:)
!     REAL(dp), POINTER :: ni_mode4d(:,:,:,:)

!     TYPE (t_ptr_2d3d) :: tau_2d_mode(nmaxclass,Nwv_tot)
!     TYPE (t_ptr_2d3d) :: abs_2d_mode(nmaxclass,Nwv_tot)
!     TYPE (t_ptr_2d3d) :: sigma_2d_mode(nmaxclass,Nwv_tot)
!     TYPE (t_ptr_2d3d) :: omega_2d_mode(nmaxclass,Nwv_tot)
!     TYPE (t_ptr_2d3d) :: asym_2d_mode(nmaxclass,Nwv_tot)
!     TYPE (t_ptr_2d3d) :: nr_2d_mode(nmaxclass,Nwv_tot)
!     TYPE (t_ptr_2d3d) :: ni_2d_mode(nmaxclass,Nwv_tot)
    
!     REAL(dp), POINTER :: tau_2d_mode3d(:,:,:) ! for container
!     REAL(dp), POINTER :: abs_2d_mode3d(:,:,:)
!     REAL(dp), POINTER :: sigma_2d_mode3d(:,:,:)  
!     REAL(dp), POINTER :: omega_2d_mode3d(:,:,:)
!     REAL(dp), POINTER :: asym_2d_mode3d(:,:,:)
!     REAL(dp), POINTER :: nr_2d_mode3d(:,:,:)
!     REAL(dp), POINTER :: ni_2d_mode3d(:,:,:)

!     ! over all modes
!     TYPE (t_ptr_2d3d) :: tau_tot(Nwv_tot) ! for container
!     TYPE (t_ptr_2d3d) :: abs_tot(Nwv_tot)

!     REAL(dp), POINTER :: tau_tot4d(:,:,:,:)
!     REAL(dp), POINTER :: abs_tot4d(:,:,:,:)

!     TYPE (t_ptr_2d3d) :: tau_2d_tot(Nwv_tot) ! for container
!     TYPE (t_ptr_2d3d) :: abs_2d_tot(Nwv_tot)

!     REAL(dp), POINTER :: tau_2d_tot3d(:,:,:)
!     REAL(dp), POINTER :: abs_2d_tot3d(:,:,:)

!     REAL(dp), POINTER :: ang(:,:)     
!     REAL(dp), POINTER :: aod_533nm(:,:) 
!     REAL(dp), POINTER :: ssa_533nm(:,:) 
!     REAL(dp), POINTER :: asy_533nm(:,:) 
!     REAL(dp), POINTER :: aod_9732nm(:,:) 
    
!   END TYPE t_ham_rad_field

!   !!--------------------------------------------------------------------------
!   !!                          STATE VARIABLES 
!   !!--------------------------------------------------------------------------
!   TYPE(t_ham_field),     ALLOCATABLE, TARGET :: ham_field(:)      ! shape: (n_dom)
!   TYPE(t_ham_rad_field), ALLOCATABLE, TARGET :: ham_rad_field(:)  ! shape: (n_dom)

!   !!--------------------------------------------------------------------------
!   !!                          VARIABLE LISTS
!   !!--------------------------------------------------------------------------
!   TYPE(t_var_list), ALLOCATABLE :: ham_field_list(:)      ! shape: (n_dom)
!   TYPE(t_var_list), ALLOCATABLE :: ham_rad_field_list(:)  ! shape: (n_dom) 

!   CONTAINS

! !---------------------------------------------------------------------------------------------------

!   SUBROUTINE new_stream_ham (p_patch, nclass)

!     USE mo_model_domain_lite,  ONLY: t_patch   

!     IMPLICIT NONE

!     INTEGER, INTENT(IN) :: nclass ! number of aerosol modes (or bins)
!     TYPE(t_patch), INTENT(IN), DIMENSION(:) :: p_patch

!     CHARACTER(len=MAX_CHAR_LENGTH) :: listname
!     INTEGER :: ndomain, jg, ist, nblks, nlev

!     CALL message(TRIM(thismodule),'construction of ham state started')

!     ndomain = SIZE(p_patch)

!     ALLOCATE( ham_field(ndomain), STAT=ist)  
!     IF (ist/=SUCCESS) CALL finish(TRIM(thismodule), 'allocation of ham_field failed')

!     ALLOCATE( ham_field_list(ndomain), STAT=ist) 
!     IF (ist/=SUCCESS) CALL finish(TRIM(thismodule), 'allocation of ham_field_list failed')

!     DO jg = 1,ndomain

!       nblks = p_patch(jg)%nblks_c
!       nlev  = p_patch(jg)%nlev

!       WRITE(listname,'(a,i2.2)') 'ham_state_diag_of_domain',jg
!       CALL new_ham_field_list(  jg, nproma, nlev, nblks, nclass,                  &
!                                 TRIM(listname), ham_field_list(jg), ham_field(jg) )

!     END DO

!     CALL message(TRIM(thismodule),'construction of ham state finished')


!   END SUBROUTINE new_stream_ham

! !---------------------------------------------------------------------------------------------------

!   SUBROUTINE new_ham_field_list( jg, kproma, klev, kblks, kclass, listname, field_list, field ) 
 
!     USE mo_util_string_lite,     ONLY: tolower
!     USE mo_ham_lite_main,        ONLY: sizeclass 

!     INTEGER, INTENT(IN) :: jg  
!     INTEGER, INTENT(IN) :: kproma, klev, kblks  
!     INTEGER, INTENT(IN) :: kclass 
!     CHARACTER(len=*),  INTENT(IN) :: listname
!     TYPE(t_var_list),  INTENT(INOUT) :: field_list
!     TYPE(t_ham_field), INTENT(INOUT) :: field

!     INTEGER :: ibits 
!     INTEGER :: datatype_flt
!     INTEGER :: shape2d(2), shape3d(3)    
!     INTEGER :: jt, jw, jclass, jclass_act, nclass_act 
!     CHARACTER(len=30) :: cname
!     TYPE(t_cf_var)    :: cf_desc
!     TYPE(t_grib2_var) :: grib2_desc
!     CHARACTER(:), ALLOCATABLE :: model_type

!     ibits = DATATYPE_PACK16 
!     datatype_flt = MERGE(DATATYPE_FLT64, DATATYPE_FLT32, lnetcdf_flt64_output) 
!     shape2d  = (/kproma, kblks/)
!     shape3d  = (/kproma, klev, kblks/) 
!     model_type = get_my_process_name()

!     !$ACC ENTER DATA COPYIN(field)

!     CALL vlr_add( field_list, TRIM(listname), patch_id=jg,          &
!                   vlevel_type=level_type_ml, model_type=model_type, & 
!                   lrestart=.TRUE.                                   )

!     !--- Hydrological variables:
!     cf_desc    = t_cf_var('rhum', '%', 'relative humidity', datatype_flt)
!     grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!     CALL add_var( field_list, 'rhum', field%rhum,                               &
!                 & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc,       &
!                 & ldims=shape3d,                                                &
!                 & vert_interp=create_vert_interp_metadata(                      &
!                 &             vert_intp_type=vintp_types("P","Z","I"),          &
!                 &             vert_intp_method=VINTP_METHOD_LIN,                &
!                 &             l_loglin=.FALSE.,                                 &
!                 &             l_extrapol=.FALSE., l_pd_limit=.TRUE.,            &
!                 &             lower_limit=0._dp),                               &
!                 & lopenacc=.TRUE.                                               )
!     __acc_attach(field%rhum)

!     cf_desc    = t_cf_var('rhum_2d', '%', 'relative humidity 2d', datatype_flt)
!     grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!     CALL add_var( field_list, 'rhum_2d', field%rhum_2d,                         &
!                 & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc,      &
!                 & ldims=shape2d,                                                &
!                 & lrestart=.TRUE., lopenacc=.TRUE.                              )
!     __acc_attach(field%rhum_2d)
 
!     cf_desc    = t_cf_var('temp_2d', '%', 'temperature 2d', datatype_flt)
!     grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!     CALL add_var( field_list, 'temp_2d', field%temp_2d,                         &
!                 & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc,      &
!                 & ldims=shape2d,                                                &
!                 & lrestart=.TRUE., lopenacc=.TRUE.                              )
!     __acc_attach(field%temp_2d)

!     !--- Aerosol properties: 
!     CALL add_var( field_list, 'rwet', field%rwet4d,                             &
!                 & GRID_UNSTRUCTURED_CELL, ZA_HYBRID,                            &
!                 & t_cf_var('wet number median radius', 'm',                     &
!                 &          'wet number median radius', datatype_flt),           &
!                 & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),  &
!                 & ldims = (/kproma,klev,kblks,kclass/),                         &
!                 & lcontainer=.TRUE., lrestart=.FALSE., loutput=.FALSE.,         &
!                 & lopenacc=.TRUE.                                               )  
!     __acc_attach(field%rwet4d)

!     CALL add_var( field_list, 'daer', field%daer4d,                             &
!                 & GRID_UNSTRUCTURED_CELL, ZA_HYBRID,                            &
!                 & t_cf_var('wet density', 'kg m-3',                             &
!                 &          'wet density', datatype_flt),                        &
!                 & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),  &
!                 & ldims = (/kproma,klev,kblks,kclass/),                         &
!                 & lcontainer=.TRUE., lrestart=.FALSE., loutput=.FALSE.,         &
!                 & lopenacc=.TRUE.                                               )
!     __acc_attach(field%daer4d)

!     CALL add_var( field_list, 'rwet_2d', field%rwet3d,                          &
!                 & GRID_UNSTRUCTURED_CELL, ZA_SURFACE,                           &
!                 & t_cf_var('wet number median radius 2d', 'm',                  &
!                 &          'wet number median radius 2d', datatype_flt),        &
!                 & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),  &
!                 & ldims = (/kproma,kblks,kclass/),                              &
!                 & lcontainer=.TRUE., lrestart=.FALSE., lopenacc=.TRUE.          )  
!     __acc_attach(field%rwet3d)

!     CALL add_var( field_list, 'daer_2d', field%daer3d,                          &
!                 & GRID_UNSTRUCTURED_CELL, ZA_SURFACE,                           &
!                 & t_cf_var('wet density 2d', 'kg m-3',                          &
!                 &          'wet density 2d', datatype_flt),                     &
!                 & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),  &
!                 & ldims = (/kproma,kblks,kclass/),                              &
!                 & lcontainer=.TRUE., lrestart=.FALSE., lopenacc=.TRUE.          )
!     __acc_attach(field%daer3d)
 
!     ALLOCATE( field%rwet(kclass) )
!     ALLOCATE( field%daer(kclass) )
!     ALLOCATE( field%rwet_2d(kclass) )
!     ALLOCATE( field%daer_2d(kclass) )

!     DO jt = 1,kclass

!       cname = tolower(sizeclass(jt)%shortname)

!       cf_desc    = t_cf_var('rwet_'//TRIM(cname), 'm',                            &
!                   & 'wet number median radius - '//TRIM(cname), datatype_flt      )
!       grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)

!       CALL add_ref( field_list, 'rwet',                                           &
!                   & 'rwet_'//TRIM(cname), field%rwet(jt)%p_3d,                    &
!                   & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc,       &
!                   & ref_idx=jt,                                                   &
!                   & ldims=shape3d,                                                &
!                   & vert_interp=create_vert_interp_metadata(                      &
!                   &   vert_intp_type=vintp_types("P","Z","I"),                    &
!                   &   vert_intp_method=VINTP_METHOD_LIN,                          &
!                   &   l_extrapol=.FALSE.)                                         )

!       cf_desc    = t_cf_var('daer_'//TRIM(cname), 'kg m-3',                       &
!                   & 'wet density - '//TRIM(cname), datatype_flt                   )
!       grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)

!       CALL add_ref( field_list, 'daer',                                           &
!                   & 'daer_'//TRIM(cname), field%daer(jt)%p_3d,                    &
!                   & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc,       &
!                   & ref_idx=jt,                                                   &
!                   & ldims=shape3d,                                                &
!                   & vert_interp=create_vert_interp_metadata(                      &
!                   &   vert_intp_type=vintp_types("P","Z","I"),                    &
!                   &   vert_intp_method=VINTP_METHOD_LIN,                          &
!                   &   l_extrapol=.FALSE.)                                         )
          
!       cf_desc    = t_cf_var('rwet_2d_'//TRIM(cname), 'm',                         &
!                   & 'wet number median radius 2d - '//TRIM(cname), datatype_flt   )
!       grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)

!       CALL add_ref( field_list, 'rwet_2d', 'rwet_2d_'//TRIM(cname),               &
!                   & field%rwet_2d(jt)%p_2d,                                       &
!                   & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc,      &
!                   & ref_idx=jt,                                                   &
!                   & ldims=shape2d,                                                &
!                   & lrestart=.TRUE.                                               )

!       cf_desc    = t_cf_var('daer_2d_'//TRIM(cname), 'kg m-3',                    &
!                   & 'wet density 2d - '//TRIM(cname), datatype_flt                )
!       grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)

!       CALL add_ref( field_list, 'daer_2d', 'daer_2d_'//TRIM(cname),               &
!                   & field%daer_2d(jt)%p_2d,                                       &
!                   & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc,      &
!                   & ref_idx=jt,                                                   &
!                   & ldims=shape2d,                                                &
!                   & lrestart=.TRUE.                                               )

!     END DO

!     !--- 3D fields:

!     ! size of 4-d containers
!     nclass_act=0
!     DO jclass=1, kclass
!       IF (sizeclass(jclass)%lacti) THEN
!         nclass_act=nclass_act+1
!       END IF
!     END DO

!     CALL add_var( field_list, 'nact', field%nact4d,                               &
!                 & GRID_UNSTRUCTURED_CELL, ZA_HYBRID,                              &
!                 & t_cf_var('nact', 'm-3',                                         &
!                 &          'number of activated particles', datatype_flt),        &
!                 & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),    &
!                 & ldims = (/kproma,klev,kblks,nclass_act/),                       &
!                 & lcontainer=.TRUE., lrestart=.FALSE., lopenacc=.TRUE.            )
!     __acc_attach(field%nact4d)

!     CALL add_var( field_list, 'nact_2d', field%nact3d,                            &
!                 & GRID_UNSTRUCTURED_CELL, ZA_SURFACE,                             &
!                 & t_cf_var('nact_2d', 'm-3',                                      &
!                 &          'number of activated particles 2d', datatype_flt),     &
!                 & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),    &
!                 & ldims = (/kproma,kblks,nclass_act/),                            &
!                 & lcontainer=.TRUE., lrestart=.FALSE., lopenacc=.TRUE.            )
!     __acc_attach(field%nact3d)

!     CALL add_var( field_list, 'frac', field%frac4d,                               &
!                 & GRID_UNSTRUCTURED_CELL, ZA_HYBRID,                              &
!                 & t_cf_var('frac', '%',                                           &
!                 &          'fraction of activated particles', datatype_flt),      &
!                 & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),    &
!                 & ldims = (/kproma,klev,kblks,nclass_act/),                       &
!                 & lcontainer=.TRUE., lrestart=.FALSE., lopenacc=.TRUE.            )
!     __acc_attach(field%frac4d)

!     CALL add_var( field_list, 'frac_2d', field%frac3d,                            &
!                 & GRID_UNSTRUCTURED_CELL, ZA_SURFACE,                             &
!                 & t_cf_var('frac_2d', '%',                                        &
!                 &          'fraction of activated particles 2d', datatype_flt),   &
!                 & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),    &
!                 & ldims = (/kproma,kblks,nclass_act/),                            &
!                 & lcontainer=.TRUE., lrestart=.FALSE., lopenacc=.TRUE.            )
!     __acc_attach(field%frac3d)
 
!     CALL add_var( field_list, 'acurv', field%a4d,                                 &
!                 & GRID_UNSTRUCTURED_CELL, ZA_HYBRID,                              &
!                 & t_cf_var('acurv', 'm',                                          &
!                 &          'curvature parameter', datatype_flt),                  &
!                 & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),    &
!                 & ldims = (/kproma,klev,kblks,nclass_act/),                       &
!                 & lcontainer=.TRUE., lrestart=.FALSE., lopenacc=.TRUE.            )
!     __acc_attach(field%a4d)

!     CALL add_var( field_list, 'bhygr', field%b4d,                                 &
!                 & GRID_UNSTRUCTURED_CELL, ZA_HYBRID,                              &
!                 & t_cf_var('bhygr', 'm3',                                         &
!                 &          'hygroscopicity parameter', datatype_flt),             &
!                 & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),    &
!                 & ldims = (/kproma,klev,kblks,nclass_act/),                       &
!                 & lcontainer=.TRUE., lrestart=.FALSE., lopenacc=.TRUE.            )
!       __acc_attach(field%b4d)

!     CALL add_var( field_list, 'scrit', field%sc4d,                                &
!                 & GRID_UNSTRUCTURED_CELL, ZA_HYBRID,                              &
!                 & t_cf_var('scrit', '1',                                          &
!                 &          'critical supersaturation', datatype_flt),             &
!                 & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),    &
!                 & ldims = (/kproma,klev,kblks,nclass_act/),                       &
!                 & lcontainer=.TRUE., lrestart=.FALSE., lopenacc=.TRUE.            )
!     __acc_attach(field%sc4d)
 
!     CALL add_var( field_list, 'rcrit', field%rc4d,                                &
!                 & GRID_UNSTRUCTURED_CELL, ZA_HYBRID,                              &
!                 & t_cf_var('rcrit', 'm',                                          &
!                 &          'critical radius', datatype_flt),                      &
!                 & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),    &
!                 & ldims = (/kproma,klev,kblks,nclass_act/),                       &
!                 & lcontainer=.TRUE., lrestart=.FALSE., lopenacc=.TRUE.            )
!     __acc_attach(field%rc4d)
 
!     ALLOCATE( field%nact    (kclass) )
!     ALLOCATE( field%nact_2d (kclass) )
!     ALLOCATE( field%frac    (kclass) )  
!     ALLOCATE( field%frac_2d (kclass) ) 
!     ALLOCATE( field%a       (kclass) ) 
!     ALLOCATE( field%b       (kclass) ) 
!     ALLOCATE( field%sc      (kclass) ) 
!     ALLOCATE( field%rc      (kclass) )   

!     jclass_act = 0
!     DO jclass = 1,kclass

!       cname = tolower(sizeclass(jclass)%shortname)

!       IF (sizeclass(jclass)%lacti) THEN ! lactivation

!         jclass_act = jclass_act + 1
        
!         cf_desc    = t_cf_var('nact_'//TRIM(cname), 'm-3',                          &
!                     & 'number of activated particles '//TRIM(cname), datatype_flt   )
!         grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!         CALL add_ref( field_list, 'nact', 'nact_'//TRIM(cname),                     &
!                     & field%nact(jclass)%p_3d,                                      &
!                     & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc,       &
!                     & ref_idx=jclass_act,                                           &
!                     & ldims=shape3d,                                                &
!                     & vert_interp=create_vert_interp_metadata(                      &
!                     &   vert_intp_type=vintp_types("P","Z","I"),                    &
!                     &   vert_intp_method=VINTP_METHOD_LIN,                          &
!                     &   l_extrapol=.FALSE.),                                        &
!                     & lrestart=.TRUE.                                               )

!         cf_desc    = t_cf_var('nact_2d_'//TRIM(cname), 'm-3',                       &
!                     & 'number of activated particles 2d '//TRIM(cname),             &
!                     & datatype_flt                                                  )
!         grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!         CALL add_ref( field_list, 'nact_2d', 'nact_2d_'//TRIM(cname),               &
!                     & field%nact_2d(jclass)%p_2d,                                   &
!                     & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc,      &
!                     & ref_idx=jclass_act,                                           &
!                     & ldims=shape2d,                                                & 
!                     & lrestart=.TRUE.                                               )

!         cf_desc    = t_cf_var('frac_'//TRIM(cname), '%',                            &
!                     & 'fraction of activated particles '//TRIM(cname), datatype_flt )
!         grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!         CALL add_ref( field_list, 'frac', 'frac_'//TRIM(cname),                     &
!                     & field%frac(jclass)%p_3d,                                      &
!                     & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc,       &
!                     & ref_idx=jclass_act,                                           &
!                     & ldims=shape3d,                                                &
!                     & vert_interp=create_vert_interp_metadata(                      &
!                     &   vert_intp_type=vintp_types("P","Z","I"),                    &
!                     &   vert_intp_method=VINTP_METHOD_LIN,                          &
!                     &   l_extrapol=.FALSE.),                                        &
!                     & lrestart=.TRUE.                                               )
 
!         cf_desc    = t_cf_var('frac_2d_'//TRIM(cname), '%',                         &
!                     & 'fraction of activated particles 2d '//TRIM(cname),           &
!                     & datatype_flt                                                  )
!         grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!         CALL add_ref( field_list, 'frac_2d', 'frac_2d_'//TRIM(cname),               &
!                     & field%frac_2d(jclass)%p_2d,                                   &
!                     & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc,      &
!                     & ref_idx=jclass_act,                                           &
!                     & ldims=shape2d,                                                & 
!                     & lrestart=.TRUE.                                               )

!         cf_desc    = t_cf_var('acurv_'//TRIM(cname), 'm',                           &
!                     & 'curvature parameter '//TRIM(cname), datatype_flt             )
!         grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!         CALL add_ref( field_list, 'acurv', 'acurv_'//TRIM(cname),                   &
!                     & field%a(jclass)%p_3d,                                         &
!                     & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc,       &
!                     & ref_idx=jclass_act,                                           &
!                     & ldims=shape3d,                                                &
!                     & vert_interp=create_vert_interp_metadata(                      &
!                     &   vert_intp_type=vintp_types("P","Z","I"),                    &
!                     &   vert_intp_method=VINTP_METHOD_LIN,                          &
!                     &   l_extrapol=.FALSE.),                                        &
!                     & lrestart=.TRUE.                                               )

!         cf_desc    = t_cf_var('bhygr_'//TRIM(cname), 'm3',                          &
!                     & 'hygroscopicity parameter '//TRIM(cname), datatype_flt        )
!         grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!         CALL add_ref( field_list, 'bhygr', 'bhygr_'//TRIM(cname),                   &
!                     & field%b(jclass)%p_3d,                                         &
!                     & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc,       &
!                     & ref_idx=jclass_act,                                           &
!                     & ldims=shape3d,                                                &
!                     & vert_interp=create_vert_interp_metadata(                      &
!                     &   vert_intp_type=vintp_types("P","Z","I"),                    &
!                     &   vert_intp_method=VINTP_METHOD_LIN,                          &
!                     &   l_extrapol=.FALSE.),                                        &
!                     & lrestart=.TRUE.                                               )

!         cf_desc    = t_cf_var('scrit_'//TRIM(cname), '1',                           &
!                     & 'critcal supersaturation '//TRIM(cname), datatype_flt         )
!         grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!         CALL add_ref( field_list, 'scrit', 'scrit_'//TRIM(cname),                   &
!                     & field%sc(jclass)%p_3d,                                        &
!                     & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc,       &
!                     & ref_idx=jclass_act,                                           &
!                     & ldims=shape3d,                                                &
!                     & vert_interp=create_vert_interp_metadata(                      &
!                     &   vert_intp_type=vintp_types("P","Z","I"),                    &
!                     &   vert_intp_method=VINTP_METHOD_LIN,                          &
!                     &   l_extrapol=.FALSE.),                                        &
!                     & lrestart=.TRUE.                                               )

!         cf_desc    = t_cf_var('rcrit_'//TRIM(cname), 'm',                           &
!                     & 'critcal radius '//TRIM(cname), datatype_flt                  )
!         grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!         CALL add_ref( field_list, 'rcrit', 'rcrit_'//TRIM(cname),                   & 
!                     & field%rc(jclass)%p_3d,                                        &
!                     & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc,       &
!                     & ref_idx=jclass_act,                                           &
!                     & ldims=shape3d,                                                &
!                     & vert_interp=create_vert_interp_metadata(                      &
!                     &   vert_intp_type=vintp_types("P","Z","I"),                    &
!                     &   vert_intp_method=VINTP_METHOD_LIN,                          &
!                     &   l_extrapol=.FALSE.),                                        &
!                     & lrestart=.TRUE.                                               )
  
!       END IF ! lactivation

!     END DO

!   END SUBROUTINE new_ham_field_list

! !---------------------------------------------------------------------------------------------------

!   SUBROUTINE new_stream_ham_rad ( p_patch )

!     USE mo_model_domain_lite,    ONLY: t_patch 
!     USE mo_ham_lite_main,        ONLY: nclass

!     TYPE(t_patch), INTENT(IN), DIMENSION(:)  :: p_patch

!     CHARACTER(len=MAX_CHAR_LENGTH) :: listname
!     INTEGER :: ndomain, jg, ist, nblks, nlev

!     CALL message(TRIM(thismodule),'Construction of ham_rad stream started.')

!     ndomain = SIZE(p_patch)

!     ALLOCATE( ham_rad_field(ndomain), STAT=ist) 
!     IF (ist/=SUCCESS) CALL finish(TRIM(thismodule), 'allocation of ham_rad_field failed')

!     ALLOCATE( ham_rad_field_list(ndomain), STAT=ist) 
!     IF (ist/=SUCCESS) CALL finish(TRIM(thismodule), 'allocation of ham_rad_field_list failed')

!     DO jg = 1,ndomain

!       nblks = p_patch(jg)%nblks_c
!       nlev  = p_patch(jg)%nlev

!       WRITE(listname,'(a,i2.2)') 'ham_rad_state_diag_of_domain', jg
!       CALL new_ham_rad_field_list(  jg, nproma, nlev, nblks, nclass,                          &
!                                     TRIM(listname), ham_rad_field_list(jg), ham_rad_field(jg) )

!     END DO

!     CALL message(TRIM(thismodule),'Construction of ham_rad stream finished.')

!   END SUBROUTINE new_stream_ham_rad

! !---------------------------------------------------------------------------------------------------

!   SUBROUTINE new_ham_rad_field_list( jg, kproma, klev, kblks, kclass, listname, field_list, field ) 

!     USE mo_exception_lite,           ONLY: finish 
!     USE mo_util_string_lite,         ONLY: tolower
!     USE mo_subm_spec_lite,           ONLY: speclist
!     USE mo_ham_lite_main,            ONLY: nclass, nrad, nraddiag, sizeclass 
!     USE mo_ham_lite_rad_data,        ONLY: lambda, nradang, Nwv_tot, nraddiagwv 

!     INTEGER, INTENT(IN) :: jg  
!     INTEGER, INTENT(IN) :: kproma, klev, kblks  
!     INTEGER, INTENT(IN) :: kclass
!     CHARACTER(len=*),  INTENT(IN) :: listname
!     TYPE(t_var_list),  INTENT(INOUT) :: field_list
!     TYPE(t_ham_rad_field), INTENT(INOUT) :: field

!     INTEGER :: ibits 
!     INTEGER :: datatype_flt
!     INTEGER :: shape2d(2), shape3d(3)
!     INTEGER :: jclass, jwv
!     INTEGER :: Nwv_diag, Nwv_diag0, Nwv_diag_wv1, Nwv_diag_wv1_0
!     INTEGER :: jwv_diag, jwv_diag0, jwv_diag_wv1, jwv_diag_wv1_0
!     TYPE(t_cf_var)    :: cf_desc
!     TYPE(t_grib2_var) :: grib2_desc
!     CHARACTER(len=30) :: cwv, cwv2, cname 
!     CHARACTER(:), ALLOCATABLE :: model_type

!     ibits = DATATYPE_PACK16 
!     datatype_flt = MERGE(DATATYPE_FLT64, DATATYPE_FLT32, lnetcdf_flt64_output)   
!     shape2d  = (/kproma, kblks/)
!     shape3d  = (/kproma, klev, kblks/) 
!     model_type = get_my_process_name()

!     !$ACC ENTER DATA COPYIN(field)

!     CALL vlr_add( field_list, TRIM(listname), patch_id=jg,          &
!                   vlevel_type=level_type_ml, model_type=model_type, & 
!                   lrestart=.TRUE.                                   )

!     ! count elements for containers 
!     Nwv_diag=0
!     Nwv_diag0=0
!     Nwv_diag_wv1=0
!     Nwv_diag_wv1_0=0 
!     DO jwv=1, Nwv_tot
!       IF (nraddiagwv(jwv)>0) THEN! nraddiagwv>0
!         Nwv_diag0=Nwv_diag0+1
!         DO jclass=1, nclass  
!           IF( nrad(jclass)>0 ) THEN ! nrad  
!             Nwv_diag=Nwv_diag+1
!             IF (nraddiagwv(jwv)>1) THEN
!               Nwv_diag_wv1=Nwv_diag_wv1+1
!             END IF
!           END IF ! nrad
!         END DO  ! nclass
!         IF (nraddiagwv(jwv)>1) THEN
!           Nwv_diag_wv1_0=Nwv_diag_wv1_0+1
!         END IF 
!       END IF ! nraddiagwv>0
!     END DO ! jwv

!     !-- Add containers

!     CALL add_var( field_list, 'tau_mode', field%tau_mode4d,                       &
!                 & GRID_UNSTRUCTURED_CELL, ZA_HYBRID,                              &
!                 & t_cf_var('tau_mode', '1',                                       &
!                 &          'optical depth', datatype_flt),                        &  
!                 & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),    &  
!                 & ldims = (/kproma,klev,kblks,Nwv_diag/),                         &
!                 & lcontainer=.TRUE., lrestart=.FALSE., loutput=.FALSE.,           &
!                 & lopenacc=.TRUE.                                                 )
!     __acc_attach(field%tau_mode4d)

!     CALL add_var( field_list, 'abs_mode', field%abs_mode4d,                       &
!                 & GRID_UNSTRUCTURED_CELL, ZA_HYBRID,                              &
!                 & t_cf_var('abs_mode', '1',                                       &
!                 &          'absorption optical depth', datatype_flt),             &
!                 & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),    &  
!                 & ldims = (/kproma,klev,kblks,Nwv_diag_wv1/),                     &
!                 & lcontainer=.TRUE., lrestart=.FALSE., loutput=.FALSE.,           &
!                 & lopenacc=.TRUE.                                                 )
!     __acc_attach(field%abs_mode4d)

!     IF (nraddiag>0) THEN

!       CALL add_var( field_list, 'tau_2d_mode', field%tau_2d_mode3d,               &
!                   & GRID_UNSTRUCTURED_CELL, ZA_SURFACE,                           &
!                   & t_cf_var('tau_2d_mode', '1',                                  &
!                   &          'optical depth 2d', datatype_flt),                   &
!                   & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),  & 
!                   & ldims = (/kproma,kblks,Nwv_diag_wv1/),                        &
!                   & lcontainer=.TRUE., lrestart=.FALSE., loutput=.FALSE.,         &
!                   & lopenacc=.TRUE.                                               )
!       __acc_attach(field%tau_2d_mode3d)

!       CALL add_var( field_list, 'abs_2d_mode', field%abs_2d_mode3d,               &
!                   & GRID_UNSTRUCTURED_CELL, ZA_SURFACE,                           &
!                   & t_cf_var('abs_2d_mode', '1',                                  &
!                   &          'absorption optical depth 2d', datatype_flt),        &
!                   & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),  & 
!                   & ldims = (/kproma,kblks,Nwv_diag_wv1/),                        &
!                   & lcontainer=.TRUE., lrestart=.FALSE., loutput=.FALSE.,         &
!                   & lopenacc=.TRUE.                                               )
!       __acc_attach(field%abs_2d_mode3d)

!       CALL add_var( field_list, 'sigma_2d_mode', field%sigma_2d_mode3d,           &
!                   & GRID_UNSTRUCTURED_CELL, ZA_SURFACE,                           &
!                   & t_cf_var('sigma_2d_mode', 'm-2',                              &
!                   &          'extinction cross section per particle 2d',          &
!                   &          datatype_flt),                                       &
!                   & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),  & 
!                   & ldims = (/kproma,kblks,Nwv_diag_wv1/),                        &
!                   & lcontainer=.TRUE., lrestart=.FALSE., loutput=.FALSE.,         &
!                   & lopenacc=.TRUE.                                               )
!       __acc_attach(field%sigma_2d_mode3d)

!       CALL add_var( field_list, 'omega_2d_mode', field%omega_2d_mode3d,           &
!                   & GRID_UNSTRUCTURED_CELL, ZA_SURFACE,                           &
!                   & t_cf_var('omega_2d_mode', '1',                                &
!                   &          'single scattering albedo 2d', datatype_flt),        &
!                   & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),  & 
!                   & ldims = (/kproma,kblks,Nwv_diag_wv1/),                        &
!                   & lcontainer=.TRUE., lrestart=.FALSE., loutput=.FALSE.,         &
!                   & lopenacc=.TRUE.                                               )
!       __acc_attach(field%omega_2d_mode3d)

!       CALL add_var( field_list, 'asym_2d_mode', field%asym_2d_mode3d,             &
!                   & GRID_UNSTRUCTURED_CELL, ZA_SURFACE,                           &
!                   & t_cf_var('asym_2d_mode', '1',                                 &
!                   &          'asymmetry factor 2d', datatype_flt),                &
!                   & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),  & 
!                   & ldims = (/kproma,kblks,Nwv_diag_wv1/),                        &
!                   & lcontainer=.TRUE., lrestart=.FALSE., loutput=.FALSE.,         &
!                   & lopenacc=.TRUE.                                               )
!       __acc_attach(field%asym_2d_mode3d)

!       CALL add_var( field_list, 'nr_2d_mode', field%nr_2d_mode3d,                 &
!                   & GRID_UNSTRUCTURED_CELL, ZA_SURFACE,                           &
!                   & t_cf_var('nr_2d_mode', '1',                                   &
!                   &          'refractive index - real part 2d', datatype_flt),    &
!                   & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),  & 
!                   & ldims = (/kproma,kblks,Nwv_diag_wv1/),                        &
!                   & lcontainer=.TRUE., lrestart=.FALSE., loutput=.FALSE.,         &
!                   & lopenacc=.TRUE.                                               )
!         __acc_attach(field%nr_2d_mode3d)

!       CALL add_var( field_list, 'ni_2d_mode', field%ni_2d_mode3d,                 &
!                   & GRID_UNSTRUCTURED_CELL, ZA_SURFACE,                           &
!                   & t_cf_var('ni_2d_mode', '1',                                   &
!                   &          'refractive index - imaginary part 2d',              &
!                   &          datatype_flt),                                       &
!                   & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),  & 
!                   & ldims = (/kproma,kblks,Nwv_diag_wv1/),                        &
!                   & lcontainer=.TRUE., lrestart=.FALSE., loutput=.FALSE.,         &
!                   & lopenacc=.TRUE.                                               )
!       __acc_attach(field%ni_2d_mode3d)

!     END IF

!     IF (nraddiag==2) THEN

!       CALL add_var( field_list, 'sigma_mode', field%sigma_mode4d,                 &
!                   & GRID_UNSTRUCTURED_CELL, ZA_HYBRID,                            &
!                   & t_cf_var('sigma_mode', 'cm2 part-1',                          &
!                   &          'extinction cross section per particle',             &
!                   &          datatype_flt),                                       &
!                   & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),  & 
!                   & ldims = (/kproma,klev,kblks,Nwv_diag_wv1/),                   &
!                   & lcontainer=.TRUE., lrestart=.FALSE., loutput=.FALSE.,         &
!                   & lopenacc=.TRUE.                                               )
!       __acc_attach(field%sigma_mode4d)

!       CALL add_var( field_list, 'omega_mode', field%omega_mode4d,                 &
!                   & GRID_UNSTRUCTURED_CELL, ZA_HYBRID,                            &
!                   & t_cf_var('omega_mode', 'cm2 part-1',                          &
!                   &          'single scattering albedo', datatype_flt),           &
!                   & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),  & 
!                   & ldims = (/kproma,klev,kblks,Nwv_diag_wv1/),                   &
!                   & lcontainer=.TRUE., lrestart=.FALSE., loutput=.FALSE.,         &
!                   & lopenacc=.TRUE.                                               )
!       __acc_attach(field%omega_mode4d)

!       CALL add_var( field_list, 'asym_mode', field%asym_mode4d,                   &
!                   & GRID_UNSTRUCTURED_CELL, ZA_HYBRID,                            &
!                   & t_cf_var('asym_mode', '1',                                    &
!                   &          'asymmetry factor', datatype_flt),                   &
!                   & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),  & 
!                   & ldims = (/kproma,klev,kblks,Nwv_diag_wv1/),                   &
!                   & lcontainer=.TRUE., lrestart=.FALSE., loutput=.FALSE.,         &
!                   & lopenacc=.TRUE.                                               )
!       __acc_attach(field%asym_mode4d)

!       CALL add_var( field_list, 'nr_mode', field%nr_mode4d,                       &
!                   & GRID_UNSTRUCTURED_CELL, ZA_HYBRID,                            &
!                   & t_cf_var('nr_mode', '1',                                      &
!                   &          'refractive index - real part', datatype_flt),       &
!                   & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),  & 
!                   & ldims = (/kproma,klev,kblks,Nwv_diag_wv1/),                   &
!                   & lcontainer=.TRUE., lrestart=.FALSE., loutput=.FALSE.,         &
!                   & lopenacc=.TRUE.                                               )
!       __acc_attach(field%nr_mode4d)

!       CALL add_var( field_list, 'ni_mode', field%ni_mode4d,                       &
!                   & GRID_UNSTRUCTURED_CELL, ZA_HYBRID,                            &
!                   & t_cf_var('ni_mode', '1',                                      &
!                   &          'refractive index - imaginary part', datatype_flt),  &
!                   & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),  & 
!                   & ldims = (/kproma,klev,kblks,Nwv_diag_wv1/),                   &
!                   & lcontainer=.TRUE., lrestart=.FALSE., loutput=.FALSE.,         &
!                   & lopenacc=.TRUE.                                               )
!       __acc_attach(field%ni_mode4d)

!     END IF

!     CALL add_var( field_list, 'tau_tot', field%tau_tot4d,                         &
!                 & GRID_UNSTRUCTURED_CELL, ZA_HYBRID,                              &
!                 & t_cf_var('tau_tot', '1',                                        &
!                 &          'optical depth', datatype_flt),                        & 
!                 & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),    & 
!                 & ldims = (/kproma,klev,kblks,Nwv_diag0/),                        &
!                 & lcontainer=.TRUE., lrestart=.FALSE., loutput=.FALSE.,           &
!                 & lopenacc=.TRUE.                                                 )
!     __acc_attach(field%tau_tot4d)

!     CALL add_var( field_list, 'tau_2d_tot', field%tau_2d_tot3d,                   &  
!                 & GRID_UNSTRUCTURED_CELL, ZA_SURFACE,                             &
!                 & t_cf_var('tau_2d_tot', '1',                                     &
!                 &          'optical depth', datatype_flt),                        &
!                 & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),    & 
!                 & ldims = (/kproma,kblks,Nwv_diag0/),                             &
!                 & lcontainer=.TRUE., lrestart=.FALSE., loutput=.FALSE.,           &
!                 & lopenacc=.TRUE.                                                 )
!     __acc_attach(field%tau_2d_tot3d)

!     CALL add_var( field_list, 'abs_tot', field%abs_tot4d,                         &
!                 & GRID_UNSTRUCTURED_CELL, ZA_HYBRID,                              &
!                 & t_cf_var('abs_tot', '1',                                        &
!                 &          'absorption optical depth', datatype_flt),             &
!                 & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),    & 
!                 & ldims = (/kproma,klev,kblks,Nwv_diag0/),                        &
!                 & lcontainer=.TRUE., lrestart=.FALSE., loutput=.FALSE.,           &
!                 & lopenacc=.TRUE.                                                 )
!     __acc_attach(field%abs_tot4d)

!     CALL add_var( field_list, 'abs_2d_tot', field%abs_2d_tot3d,                   &
!                 & GRID_UNSTRUCTURED_CELL, ZA_SURFACE,                             &
!                 & t_cf_var('abs_2d_tot', '1',                                     &
!                 &          'absorption optical depth', datatype_flt),             &
!                 & grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL),    &
!                 & ldims = (/kproma,kblks,Nwv_diag_wv1_0/),                        &
!                 & lcontainer=.TRUE., lrestart=.FALSE., loutput=.FALSE.,           &
!                 & lopenacc=.TRUE.                                                 )
!     __acc_attach(field%abs_2d_tot3d)

!     jwv_diag=0
!     jwv_diag0=0
!     jwv_diag_wv1=0
!     jwv_diag_wv1_0=0 
!     DO jwv=1, Nwv_tot

!        IF (nraddiagwv(jwv)>0) THEN

!           WRITE(cwv,'(I6)') INT(lambda(jwv)*1.E9_dp)
!           cwv=TRIM(ADJUSTL(cwv))//'nm'

!           !--- Diagnostics for each mode

!           DO jclass = 1,nclass

!             cname = tolower(sizeclass(jclass)%shortname)

!             IF( nrad(jclass)>0 ) THEN ! nrad
!               jwv_diag = jwv_diag+1

!               cf_desc    = t_cf_var('tau_'//TRIM(cname)//'_'//cwv, '1',                         &
!                 & 'optical depth '//TRIM(cname)//' '//cwv, datatype_flt                         )
!               grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!               CALL add_ref( field_list, 'tau_mode',                                             &
!                           & 'tau_'//TRIM(cname)//'_'//cwv,                                      &
!                           & field%tau_mode(jclass,jwv)%p_3d,                                    &
!                           & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc,             &
!                           & ref_idx=jwv_diag,                                                   &
!                           & ldims=shape3d,                                                      &
!                           & vert_interp=create_vert_interp_metadata(                            &
!                           &             vert_intp_type=vintp_types("P","Z","I"),                &
!                           &             vert_intp_method=VINTP_METHOD_LIN,                      &
!                           &             l_loglin=.FALSE.,                                       &
!                           &             l_extrapol=.FALSE.),                                    &                  
!                           & lrestart=.TRUE.                                                     )

!               IF (nraddiagwv(jwv)>1) THEN

!                 jwv_diag_wv1 = jwv_diag_wv1 + 1
!                 cf_desc    = t_cf_var('abs_'//TRIM(cname)//'_'//cwv, '1',                         &
!                   & 'absorption optical depth '//TRIM(cname)//' '//cwv, datatype_flt              )
!                 grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!                 CALL add_ref( field_list, 'abs_mode',                                             &
!                             & 'abs_'//TRIM(cname)//'_'//cwv,                                      &
!                             & field%abs_mode(jclass,jwv)%p_3d,                                    &
!                             & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc,             &
!                             & ref_idx=jwv_diag_wv1,                                               &
!                             & ldims=shape3d,                                                      &
!                             & vert_interp=create_vert_interp_metadata(                            &
!                             &             vert_intp_type=vintp_types("P","Z","I"),                &
!                             &             vert_intp_method=VINTP_METHOD_LIN,                      &
!                             &             l_loglin=.FALSE.,                                       &
!                             &             l_extrapol=.FALSE.),                                    &        
!                             & lrestart=.TRUE.                                                     )


!                 IF (nraddiag>0) THEN

!                   cf_desc    = t_cf_var('tau_2d_'//TRIM(cname)//'_'//cwv, '1',                      &
!                     & 'optical depth 2d '//TRIM(cname)//' '//cwv, datatype_flt                      )
!                   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!                   CALL add_ref( field_list, 'tau_2d_mode',                                          &
!                               & 'tau_2d_'//TRIM(cname)//'_'//cwv,                                   &
!                               & field%tau_2d_mode(jclass,jwv)%p_2d,                                 &
!                               & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc,            &
!                               & ref_idx=jwv_diag_wv1,                                               &
!                               & ldims=shape2d,                                                      &
!                               & lrestart=.TRUE.                                                     )
                              
!                   cf_desc    = t_cf_var('abs_2d_'//TRIM(cname)//'_'//cwv, '1',                      &
!                     & 'absorption optical depth 2d '//TRIM(cname)//' '//cwv, datatype_flt           )
!                   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!                   CALL add_ref( field_list, 'abs_2d_mode',                                          &
!                               & 'abs_2d_'//TRIM(cname)//'_'//cwv,                                   &
!                               & field%abs_2d_mode(jclass,jwv)%p_2d,                                 &
!                               & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc,            &
!                               & ref_idx=jwv_diag_wv1,                                               &
!                               & ldims=shape2d,                                                      &
!                               & lrestart=.TRUE.                                                     ) 

!                   cf_desc    = t_cf_var('sigma_2d_'//TRIM(cname)//'_'//cwv, 'm-2',                  &
!                     & 'extinction cross section per particle 2d '//TRIM(cname)//' '//cwv,           &
!                     & datatype_flt                                                                  )
!                   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!                   CALL add_ref( field_list, 'sigma_2d_mode',                                        &
!                               & 'sigma_2d_'//TRIM(cname)//'_'//cwv,                                 &
!                               & field%sigma_2d_mode(jclass,jwv)%p_2d,                               &
!                               & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc,            &
!                               & ref_idx=jwv_diag_wv1,                                               &
!                               & ldims=shape2d,                                                      &              
!                               & lrestart=.TRUE.                                                     )


!                   cf_desc    = t_cf_var('omega_2d_'//TRIM(cname)//'_'//cwv, '1',                    &
!                     & 'single scattering albedo 2d '//TRIM(cname)//' '//cwv, datatype_flt           )
!                   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!                   CALL add_ref( field_list, 'omega_2d_mode',                                        &
!                               & 'omega_2d_'//TRIM(cname)//'_'//cwv,                                 &
!                               & field%omega_2d_mode(jclass,jwv)%p_2d,                               &
!                               & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc,            &
!                               & ref_idx=jwv_diag_wv1,                                               &
!                               & ldims=shape2d,                                                      &    
!                               & lrestart=.TRUE.                                                     )

!                   cf_desc    = t_cf_var('asym_2d_'//TRIM(cname)//'_'//cwv, '1',                     &
!                     & 'asymmetry factor 2d '//TRIM(cname)//' '//cwv, datatype_flt                   )
!                   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!                   CALL add_ref( field_list, 'asym_2d_mode',                                         &
!                               & 'asym_2d_'//TRIM(cname)//'_'//cwv,                                  &
!                               & field%asym_2d_mode(jclass,jwv)%p_2d,                                &
!                               & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc,            &    
!                               & ref_idx=jwv_diag_wv1,                                               &
!                               & ldims=shape2d,                                                      &               
!                               & lrestart=.TRUE.                                                     )

!                   cf_desc    = t_cf_var('nr_2d_'//TRIM(cname)//'_'//cwv, '1',                       &
!                     & 'refractive index - real part 2d '//TRIM(cname)//' '//cwv, datatype_flt       )
!                   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!                   CALL add_ref( field_list, 'nr_2d_mode',                                           &
!                               & 'nr_2d_'//TRIM(cname)//'_'//cwv,                                    &
!                               & field%nr_2d_mode(jclass,jwv)%p_2d,                                  &
!                               & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc,            &
!                               & ref_idx=jwv_diag_wv1,                                               &
!                               & ldims=shape2d,                                                      &
!                               & lrestart=.TRUE.                                                     )

!                   cf_desc    = t_cf_var('ni_2d_'//TRIM(cname)//'_'//cwv, '1',                       &
!                     & 'refractive index - imaginary part 2d '//TRIM(cname)//' '//cwv, datatype_flt  )
!                   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!                   CALL add_ref( field_list, 'ni_2d_mode',                                           &
!                               & 'ni_2d_'//TRIM(cname)//'_'//cwv,                                    &
!                               & field%ni_2d_mode(jclass,jwv)%p_2d,                                  &
!                               & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc,            &
!                               & ref_idx=jwv_diag_wv1,                                               &
!                               & ldims=shape2d,                                                      &       
!                               & lrestart=.TRUE.                                                     )

!                 END IF

!                 IF (nraddiag==2) THEN

!                   cf_desc    = t_cf_var('sigma_'//TRIM(cname)//'_'//cwv, '1',                         &
!                     & 'extinction cross section per particle '//TRIM(cname)//' '//cwv, datatype_flt   )
!                   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!                   CALL add_ref( field_list, 'sigma_mode',                                             &
!                               & 'sigma_'//TRIM(cname)//'_'//cwv,                                      &
!                               & field%sigma_mode(jclass,jwv)%p_3d,                                    &
!                               & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc,               &
!                               & ref_idx=jwv_diag_wv1,                                                 &
!                               & ldims=shape3d,                                                        &
!                               & vert_interp=create_vert_interp_metadata(                              &
!                               &             vert_intp_type=vintp_types("P","Z","I"),                  &
!                               &             vert_intp_method=VINTP_METHOD_LIN,                        &
!                               &             l_loglin=.FALSE.,                                         &
!                               &             l_extrapol=.FALSE.),                                      &       
!                               & lrestart=.TRUE.                                                       )

!                   cf_desc    = t_cf_var('omega_'//TRIM(cname)//'_'//cwv, '1',                         &
!                     & 'single scattering albedo '//TRIM(cname)//' '//cwv, datatype_flt                )
!                   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!                   CALL add_ref( field_list, 'omega_mode',                                             &
!                               & 'omega_'//TRIM(cname)//'_'//cwv,                                      &
!                               & field%omega_mode(jclass,jwv)%p_3d,                                    &
!                               & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc,               &
!                               & ref_idx=jwv_diag_wv1,                                                 &
!                               & ldims=shape3d,                                                        &
!                               & vert_interp=create_vert_interp_metadata(                              &
!                               &             vert_intp_type=vintp_types("P","Z","I"),                  &
!                               &             vert_intp_method=VINTP_METHOD_LIN,                        &
!                               &             l_loglin=.FALSE.,                                         &
!                               &             l_extrapol=.FALSE.),                                      &        
!                               & lrestart=.TRUE.                                                       )

!                   cf_desc    = t_cf_var('asym_'//TRIM(cname)//'_'//cwv, '1',                          &
!                     & 'asymmetry factor '//TRIM(cname)//' '//cwv, datatype_flt                        )
!                   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!                   CALL add_ref( field_list, 'asym_mode',                                              &
!                               & 'asym_'//TRIM(cname)//'_'//cwv,                                       &
!                               & field%asym_mode(jclass,jwv)%p_3d,                                     &
!                               & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc,               &
!                               & ref_idx=jwv_diag_wv1,                                                 &
!                               & ldims=shape3d,                                                        &
!                               & vert_interp=create_vert_interp_metadata(                              &
!                               &             vert_intp_type=vintp_types("P","Z","I"),                  &
!                               &             vert_intp_method=VINTP_METHOD_LIN,                        &
!                               &             l_loglin=.FALSE.,                                         &
!                               &             l_extrapol=.FALSE.),                                      &        
!                               &   lrestart=.TRUE.                                                     )

!                   cf_desc    = t_cf_var('nr_'//TRIM(cname)//'_'//cwv, '1',                            &
!                     & 'refractive index - real part '//TRIM(cname)//' '//cwv, datatype_flt            )
!                   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!                   CALL add_ref( field_list, 'nr_mode',                                                &
!                               & 'nr_'//TRIM(cname)//'_'//cwv,                                         &
!                               & field%nr_mode(jclass,jwv)%p_3d,                                       &
!                               & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc,               &
!                               & ref_idx=jwv_diag_wv1,                                                 &
!                               & ldims=shape3d,                                                        &
!                               & vert_interp=create_vert_interp_metadata(                              &
!                               &             vert_intp_type=vintp_types("P","Z","I"),                  & 
!                               &             vert_intp_method=VINTP_METHOD_LIN,                        &
!                               &             l_loglin=.FALSE.,                                         &
!                               &             l_extrapol=.FALSE.),                                      &                  
!                               & lrestart=.TRUE.                                                       )

!                   cf_desc    = t_cf_var('ni_'//TRIM(cname)//'_'//cwv, '1',                            &
!                     & 'refractive index - imaginary part '//  TRIM(cname)//' '//cwv, datatype_flt     )
!                   grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!                   CALL add_ref( field_list, 'ni_mode',                                                &
!                               & 'ni_'//TRIM(cname)//'_'//cwv,                                         &
!                               & field%ni_mode(jclass,jwv)%p_3d,                                       &
!                               & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc,               &
!                               & ref_idx=jwv_diag_wv1,                                                 &
!                               & ldims=shape3d,                                                        &
!                               & vert_interp=create_vert_interp_metadata(                              &
!                               &             vert_intp_type=vintp_types("P","Z","I"),                  &
!                               &             vert_intp_method=VINTP_METHOD_LIN,                        &
!                               &             l_loglin=.FALSE.,                                         &
!                               &             l_extrapol=.FALSE.),                                      &           
!                               & lrestart=.TRUE.                                                       )

!                 END IF ! nraddiag

!               END IF !nraddiagwv>1

!             END IF ! nrad

!           END DO !nclass

!           !--- 2D and 3D diagnostics over all modes

!           IF (nraddiagwv(jwv)>0) THEN

!             jwv_diag0 = jwv_diag0+1

!             cf_desc    = t_cf_var('tau_2d_tot_'//cwv, '1',                                &
!                         & 'optical depth 2d '//cwv, datatype_flt                          )
!             grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!             CALL add_ref( field_list, 'tau_2d_tot', 'tau_2d_tot_'//cwv,                   &
!                         & field%tau_2d_tot(jwv)%p_2d,                                     &
!                         & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc,        &
!                         & ref_idx=jwv_diag0,                                              &
!                         & ldims=shape2d,                                                  &                 
!                         & lrestart=.TRUE.                                                 )

!             cf_desc    = t_cf_var('tau_tot_'//cwv, '1',                                   &
!                         & 'optical depth '//cwv, datatype_flt                             )
!             grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!             CALL add_ref( field_list, 'tau_tot', 'tau_tot_'//cwv,                         &
!                         & field%tau_tot(jwv)%p_3d,                                        &
!                         & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc,         &
!                         & ref_idx=jwv_diag0,                                              &
!                         & ldims=shape3d,                                                  &
!                         & vert_interp=create_vert_interp_metadata(                        &
!                         &             vert_intp_type=vintp_types("P","Z","I"),            &
!                         &             vert_intp_method=VINTP_METHOD_LIN,                  &
!                         &             l_loglin=.FALSE.,                                   &
!                         &             l_extrapol=.FALSE.),                                &
!                         & lrestart=.TRUE.                                                 )

!           END IF

!           IF (nraddiagwv(jwv)>1) THEN

!             jwv_diag_wv1_0 = jwv_diag_wv1_0+1

!             cf_desc    = t_cf_var('abs_2d_tot'//'_'//cwv, '1',                            &
!                         & 'absorption optical depth 2d '//cwv, datatype_flt               )
!             grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!             CALL add_ref( field_list, 'abs_2d_tot', 'abs_2d_tot'//'_'//cwv,               &
!                         & field%abs_2d_tot(jwv)%p_2d,                                     &
!                         & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc,        &
!                         & ref_idx=jwv_diag_wv1_0,                                         &
!                         & ldims=shape2d,                                                  &           
!                         & lrestart=.TRUE.                                                 )

!             cf_desc    = t_cf_var('abs_tot_'//cwv, '1',                                   &
!                         & 'absorption optical depth '//cwv, datatype_flt                  )
!             grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!             CALL add_ref( field_list, 'abs_tot', 'abs_tot_'//cwv,                         &
!                         & field%abs_tot(jwv)%p_3d,                                        &
!                         & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc,         &
!                         & ref_idx=jwv_diag_wv1_0,                                         &
!                         & ldims=shape3d,                                                  &
!                         & vert_interp=create_vert_interp_metadata(                        &
!                         &             vert_intp_type=vintp_types("P","Z","I"),            &
!                         &             vert_intp_method=VINTP_METHOD_LIN,                  &
!                         &             l_loglin=.FALSE.,                                   &
!                         &             l_extrapol=.FALSE.),                                &
!                         & lrestart=.TRUE.                                                 )
                        
!           END IF !nraddiagwv>1


!        END IF !nraddiagwv>0
!     END DO !jwv


!     IF (nradang(1)/=0 .AND. nradang(2)/=0) THEN

!       IF (nraddiagwv(nradang(1))>0 .AND. nraddiagwv(nradang(2))>0 ) THEN

!         WRITE(cwv,'(I6)') INT(lambda(nradang(1))*1.E9_dp)
!         cwv=TRIM(ADJUSTL(cwv))//'nm'

!         WRITE(cwv2,'(I6)') INT(lambda(nradang(2))*1.E9_dp)
!         cwv2=TRIM(ADJUSTL(cwv2))//'nm'

!         cf_desc    = t_cf_var('ang_'//TRIM(ADJUSTL(cwv))//'_'//TRIM(ADJUSTL(cwv2)), '1',              &
!           & 'angstroem parameter between '//TRIM(ADJUSTL(cwv))//' and '//TRIM(ADJUSTL(cwv2)),         &
!           & datatype_flt                                                                              )
!         grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!         CALL add_var( field_list, 'ang_'//TRIM(ADJUSTL(cwv))//'_'//TRIM(ADJUSTL(cwv2)), field%ang,    &
!                     & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc,                        &
!                     & ldims=shape2d,                                                                  &        
!                     & lrestart=.TRUE., lopenacc=.TRUE.                                                )
!         __acc_attach(field%ang)

!       ELSE

!         CALL finish('new_ham_rad_field_list','angstroem parameter between '// &
!           TRIM(ADJUSTL(cwv))//' and '//TRIM(ADJUSTL(cwv2))//' requested but inconsistent nraddiagwv')

!       END IF

!     END IF

!     cf_desc    = t_cf_var( 'aod_533nm', '1', 'aerosol optical depth at 533 nm', datatype_flt )
!     grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!     CALL add_var( field_list, 'aod_533nm',  field%aod_533nm,                  &
!                   & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc,  &
!                   & ldims=shape2d,                                            &        
!                   & lrestart=.TRUE., lopenacc=.TRUE.                          )
!     __acc_attach(field%aod_533nm)

!     cf_desc    = t_cf_var( 'ssa_533nm', '1', 'single scattering albedo at 533 nm', datatype_flt )
!     grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!     CALL add_var( field_list, 'ssa_533nm',  field%ssa_533nm,                  &
!                   & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc,  &
!                   & ldims=shape2d,                                            &        
!                   & lrestart=.TRUE., lopenacc=.TRUE.                          )
!     __acc_attach(field%ssa_533nm)

!     cf_desc    = t_cf_var( 'asy_533nm', '1', 'asymmetry factor at 533 nm', datatype_flt )
!     grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!     CALL add_var( field_list, 'asy_533nm',  field%asy_533nm,                  &
!                   & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc,  &
!                   & ldims=shape2d,                                            &        
!                   & lrestart=.TRUE., lopenacc=.TRUE.                          )
!     __acc_attach(field%asy_533nm)

!     cf_desc    = t_cf_var( 'aod_9732nm', '1', 'aerosol optical depth at 9732 nm', datatype_flt )
!     grib2_desc = grib2_var(255,255,255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!     CALL add_var( field_list, 'aod_9732nm',  field%aod_9732nm,                &
!                   & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc,  &
!                   & ldims=shape2d,                                            &        
!                   & lrestart=.TRUE., lopenacc=.TRUE.                          )
!     __acc_attach(field%aod_9732nm)

!   END SUBROUTINE new_ham_rad_field_list

! END MODULE mo_ham_lite_streams
