! 
! based on mo_hammoz_drydep.f90 
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

MODULE mo_hammoz_lite_drydep

  USE mo_kind_lite,              ONLY: dp
  !USE mo_master_control,    ONLY: get_my_process_name
  !USE mo_subm_diag,         ONLY: t_diag_list
  !USE mo_var,               ONLY: level_type_ml 
  !USE mo_var_list,          ONLY: t_var_list => t_var_list_ptr

  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------
 
  PUBLIC  ::  init_drydep,  &
              !init_drydep_stream, &
              drydep_lite_interface 

  !--------------------
  ! module parameters
  !--------------------
 
  INTEGER, PARAMETER  :: ndrydepvars=2
  CHARACTER(LEN=32), PARAMETER :: drydepvars(1:ndrydepvars) = (/        &
                                                    CHARACTER(len=10):: &
                                                    'ddep',             &  ! total drydep flux
                                                    'vddep'   /)           ! deposition velocity

  CHARACTER(LEN=*), PARAMETER :: thismodule = 'mo_hammoz_drydep'

  !--------------------
  ! module variables
  !--------------------

  ! TYPE t_drydep_field
  !   ! variable pointers and diagnostic lists
  !   TYPE (t_diag_list), PUBLIC   :: ddep         ! dry deposition flux
  !   TYPE (t_diag_list), PUBLIC   :: vddep        ! dry deposition velocity
  ! END TYPE t_drydep_field

  !TYPE(t_drydep_field), ALLOCATABLE, TARGET :: drydep_field(:)  ! shape: (n_dom)
  !TYPE(t_var_list), POINTER :: drydep_field_list(:)  ! shape: (n_dom)
  !TYPE(t_var_list), POINTER :: drydep_field_list1

  INTEGER :: idt_ddep_detail

  ! 'boundary conditions' from JSBACH 
  INTEGER, SAVE :: ibc_lai  = -1,  &  ! index of leaf area index 
                   ibc_vgf  = -1      ! index of vegetation fraction
 
  CONTAINS
 
!---------------------------------------------------------------------------------------------------

  SUBROUTINE init_drydep( p_patch )

    USE mo_exception_lite,           ONLY: finish
    USE mo_model_domain_lite,        ONLY: t_patch
    USE mo_subm_trac_lite,           ONLY: ntrac, trlist 

    TYPE(t_patch),INTENT(IN), DIMENSION(:)  :: p_patch

    INTEGER :: ndomain

    ndomain=SIZE( p_patch )
    IF ( ndomain /= 1 ) THEN
     CALL finish(thismodule, 'nesting currently not supported')
    END IF  

    !CALL init_drydep_bc_from_jsbach( p_patch(1)%id )

  END SUBROUTINE init_drydep

!---------------------------------------------------------------------------------------------------

!   SUBROUTINE init_drydep_stream( p_patch )

!     USE mo_parallel_config,     ONLY: nproma
!     USE mo_model_domain,        ONLY: t_patch
!     USE mo_impl_constants,      ONLY: MAX_CHAR_LENGTH, SUCCESS
!     USE mo_exception,           ONLY: finish, message 
!     USE mo_util_str12,          ONLY: st1_in_st2_proof
!     USE mo_util_string,         ONLY: tolower
!     USE mo_subm_trac,           ONLY: ntrac, trlist, validate_traclist
!     USE mo_subm_trac_para,      ONLY: ln, GAS, AEROSOL
!     USE mo_subm_spec,           ONLY: nspec, speclist 
!     USE mo_subm_streams,        ONLY: drydep_lpost, drydep_tinterval, &
!                                       drydepnam, drydep_keytype  
!     USE mo_subm_diag,           ONLY: BYTRACER, BYSPECIES, BYNUMMODE, BYMODE  
!     USE mo_ham_lite_main,       ONLY: nclass

!     IMPLICIT NONE

!     TYPE(t_patch),INTENT(IN), DIMENSION(:)  :: p_patch
 
!     INTEGER, PARAMETER  :: ndefault = 2
!     CHARACTER(LEN=32), PARAMETER  :: defnam(1:ndefault) = (/        & ! default diagnostics
!                                                 CHARACTER(len=10):: &
!                                                 'ddep',             & ! total drydep flux
!                                                 'vddep'   /)          ! dry deposition velocity
!     CHARACTER(len=ln), PARAMETER  :: defaultgas(1) = (/ '    ' /) ! default diagnostics for gas-phase tracers

!     INTEGER                         :: ierr, jt, ist
!     INTEGER                         :: jg, ndomain, nblks, nlev
!     LOGICAL                         :: tracflag(ntrac), specflag(nspec), modflag(MAX(nclass,1))
!     CHARACTER(LEN=ln)               :: tracname(ntrac), specname(nspec), modname(MAX(nclass,1)), &
!                                         modnumname(MAX(nclass,1))   
!     CHARACTER(LEN=MAX_CHAR_LENGTH)  :: listname
  
!     !-- handle ALL, DETAIL and DEFAULT options for drydep output variables
!     !-- Note: ALL and DETAIL are identical for drydep output
!     IF (TRIM(tolower(drydepnam(1))) == 'detail')  drydepnam(1:ndrydepvars) = drydepvars(:)
!     IF (TRIM(tolower(drydepnam(1))) == 'all')     drydepnam(1:ndrydepvars) = drydepvars(:)
!     IF (TRIM(tolower(drydepnam(1))) == 'default') drydepnam(1:ndefault) = defnam(:)

!     !-- check that all diagnostic names from namelist are valid
!     IF (.NOT. st1_in_st2_proof( drydepnam, drydepvars, ierr=ierr) ) THEN
!       IF (ierr > 0) CALL finish ( 'ini_drydep_stream', 'variable '// &
!                                   drydepnam(ierr)//' does not exist in drydep stream' )
!     END IF               

!     !-- define the flags and names for the diagnostic lists. We need one set of flags and
!     !   names for each key_type (BYTRACER, BYSPECIES, BYMODE)
!     !   gas-phase tracers will always be defined BYTRACER, for aerosol tracers one of the
!     !   following lists will be empty.
!     !   Note: vddep uses BYTRACER or BYMODE, ddep uses BYTRACER or BYSPECIES 
!     tracflag(:) = .FALSE.
!     DO jt = 1,ntrac
!       tracname(jt) = trlist%ti(jt)%fullname
!       IF (trlist%ti(jt)%nphase /= GAS) THEN
!         IF (drydep_keytype == BYTRACER .AND. nclass > 0) THEN
!           tracflag(jt) = trlist%ti(jt)%ndrydep > 0
!         END IF
!       END IF
!     END DO
!     specflag(:) = .FALSE.
!     DO jt = 1,nspec
!       specname(jt) = speclist(jt)%shortname
!       IF (drydep_keytype == BYSPECIES .AND. IAND(speclist(jt)%nphase, AEROSOL) /= 0 .AND. nclass > 0) THEN
!         specflag(jt) = speclist(jt)%ldrydep
!       END IF
!     END DO 
!     modflag(:) = .FALSE.
!     modname(:) = '' 

!     CALL message(TRIM(thismodule),'construction of HAM drydep state started')

!     ndomain = SIZE(p_patch)

!     ALLOCATE( drydep_field(ndomain), STAT=ist)
!     !$ACC ENTER DATA COPYIN( drydep_field(ndomain) )

!     IF (ist/=SUCCESS) CALL finish(TRIM(thismodule), 'allocation of drydep_field failed')

!     ALLOCATE( drydep_field_list(ndomain), STAT=ist)
!     !$ACC ENTER DATA COPYIN( drydep_field_list(ndomain) )

!     IF (ist/=SUCCESS) CALL finish(TRIM(thismodule), 'allocation of drydep_field_list failed')

!     DO jg = 1,ndomain

!       nblks = p_patch(jg)%nblks_c
!       nlev  = p_patch(jg)%nlev

!       drydep_field_list1 => drydep_field_list(jg)

!       WRITE(listname,'(a,i2.2)') 'drydep_state_diag_of_domain', jg

!       CALL new_drydep_field_list( jg,        nproma,     nlev,        nblks,               &
!                                &  nclass,    ntrac,      nspec,                            &
!                                &  tracflag,  specflag,   modflag,                          &
!                                &  tracname,  specname,   modname,     modnumname,          &
!                                &  TRIM(listname),                                          &
!                                &  drydep_field_list1, drydep_field(jg)                     )

!     END DO

!     CALL message(TRIM(thismodule),'construction of HAM drydep state finished')
  
!   END SUBROUTINE init_drydep_stream

! !---------------------------------------------------------------------------------------------------

!   SUBROUTINE new_drydep_field_list( jg,         kproma,     klev,       kblks,        &
!                                     kclass,     ktrac,      kspec,                    &
!                                     tracflag,   specflag,   modflag,                  &
!                                     tracname,   specname,   modname,     modnumname,  &                      
!                                     listname,                                         &
!                                     field_list, field                                 ) 

!     USE mo_var_list_register,   ONLY: vlr_add 
!     USE mo_cdi,                 ONLY: DATATYPE_PACK16, DATATYPE_PACK24,  &
!       &                               DATATYPE_FLT32, DATATYPE_FLT64          
!     USE mo_io_config,           ONLY: lnetcdf_flt64_output 
!     USE mo_util_str12,          ONLY: st1_in_st2_proof 
!     USE mo_subm_diag,           ONLY: new_diag_list, new_diag,  &
!                                       BYTRACER, BYSPECIES, BYNUMMODE, BYMODE  
!     USE mo_subm_streams,        ONLY: drydep_lpost, drydepnam  
!     USE mo_subm_trac_para,      ONLY: ln 

!     IMPLICIT NONE

!     INTEGER, INTENT(IN) :: jg ! grid index
!     INTEGER, INTENT(IN) :: kproma, klev,  kblks ! dimensions
!     INTEGER, INTENT(IN) :: kclass, ktrac, kspec
!     LOGICAL, INTENT(IN)            :: tracflag(ktrac), specflag(kspec), modflag(MAX(kclass,1))
!     CHARACTER(LEN=ln), INTENT(IN)  :: tracname(ktrac), specname(kspec), modname(MAX(kclass,1)), &
!                                       modnumname(MAX(kclass,1))  
!     CHARACTER(len=*),  INTENT(IN) :: listname
!     TYPE(t_var_list),  POINTER,   INTENT(INOUT) :: field_list
!     TYPE(t_drydep_field), INTENT(INOUT)         :: field
 
!     INTEGER :: ibits, iextbits
!     INTEGER :: datatype_flt
!     INTEGER :: shape2d(2), shape3d(3)
!     LOGICAL :: lpost
!     CHARACTER(:), ALLOCATABLE :: model_type
    
!     ibits = DATATYPE_PACK16
!     iextbits = DATATYPE_PACK24

!     IF ( lnetcdf_flt64_output ) THEN
!       datatype_flt = DATATYPE_FLT64
!     ELSE
!       datatype_flt = DATATYPE_FLT32
!     END IF

!     shape2d  = (/kproma,       kblks/)
!     shape3d  = (/kproma, klev, kblks/)

!     !model_type = get_my_process_name()

!     CALL vlr_add( field_list, TRIM(listname), patch_id=jg,          & 
!                   vlevel_type=level_type_ml, model_type=model_type, &
!                   lrestart=.FALSE., loutput= drydep_lpost           )

!     !-- instantaneous diagnostic quantities
!     !-- drydep velocities
!     !-- these are used in the calculation and must always be present in the stream 

!     lpost = st1_in_st2_proof('vddep', drydepnam) .AND. drydep_lpost
!     CALL new_diag_list( field%vddep, field_list, diagname='vddep',                  &
!                         longname='dry deposition velocity', units='m s-1',          &
!                         ndims=2, nmaxkey=(/ktrac, 0, kclass, kclass, 0 /),          &
!                         table=199, lpost=lpost                                      )
!     CALL new_diag(field%vddep, ktrac, tracflag, tracname, BYTRACER, shape2d=shape2d )
!     IF (ANY(modflag)) THEN 
!       CALL new_diag( field%vddep, kclass, modflag, modname, BYMODE,  shape2d=shape2d )
!       CALL new_diag( field%vddep, kclass, modflag, modnumname, BYNUMMODE,  shape2d=shape2d )
!     END IF

!     ! add total drydep flux
!     lpost = st1_in_st2_proof('ddep', drydepnam) .AND. drydep_lpost
!     CALL new_diag_list( field%ddep, field_list, diagname='ddep',                    &
!                         longname='accumulated dry deposition flux',                 &
!                         units='m-2 s-1', ndims=2,                                   &
!                         nmaxkey=(/ktrac, 0, kclass, kclass, 0 /), lpost=lpost,      &
!                         table=199                                                   )

!     ! add diagnostic elements only when output is activated
!     IF (lpost) THEN
!       CALL new_diag(field%ddep, ktrac, tracflag, tracname, BYTRACER, shape2d=shape2d )
!       CALL new_diag(field%ddep, kspec, specflag, specname, BYSPECIES, shape2d=shape2d )
!       IF (ANY(modflag)) THEN  
!         CALL new_diag(field%ddep, kclass, modflag, modname, BYMODE,  shape2d=shape2d )
!         CALL new_diag(field%ddep, kclass, modflag, modnumname, BYNUMMODE,  shape2d=shape2d )
!       END IF
!     END IF


!   END SUBROUTINE new_drydep_field_list

!---------------------------------------------------------------------------------------------------

  ! SUBROUTINE init_drydep_bc_from_jsbach( jg )

  !   USE mo_exception,         ONLY: message, finish 
  !   USE mo_util_extfield,     ONLY: EF_FILE, EF_MODULE, EF_NOINTER, &
  !                                   EF_LINEAR, EF_LONLAT, EF_IGNOREYEAR
  !   USE mo_subm_bcs,          ONLY: bc_find, bc_nml, bc_define, BC_REPLACE, BC_BOTTOM

  !   INTEGER, INTENT(IN) :: jg ! grid index
 
  !   INTEGER :: ierr
  !   TYPE(bc_nml)  ::  bc_lai,   &   ! leaf area index 
  !                     bc_vgf        ! vegetation fraction 

  !   CALL bc_find(jg, 'leaf area index', ibc_lai, ierr=ierr)
  !   IF (ierr /= 0) THEN
  !     bc_lai%ef_type = EF_MODULE
  !     bc_lai%bc_mode = BC_REPLACE
  !     bc_lai%ef_actual_unit=''
  !     ibc_lai = bc_define(jg, 'leaf area index', bc_lai, 2, .TRUE.)
  !   ELSE
  !     CALL message(thismodule,'leaf area index already defined - use previous definition')
  !   END IF
 
  !   CALL bc_find(jg, 'vegetation fraction', ibc_vgf, ierr=ierr)
  !   IF (ierr /= 0) THEN
  !     bc_vgf%ef_type = EF_MODULE
  !     bc_vgf%bc_mode = BC_REPLACE
  !     bc_vgf%ef_actual_unit = ''
  !     ibc_vgf = bc_define(jg, 'vegetation fraction', bc_vgf, 2, .TRUE.)
  !   ELSE
  !     CALL message(thismodule,'vegetation fraction already defined - use previous definition')
  !   END IF

  ! END SUBROUTINE init_drydep_bc_from_jsbach 

!---------------------------------------------------------------------------------------------------

  SUBROUTINE drydep_lite_interface( jg, jb, jcs, jce,                       &
                                    kblks_c, kproma, klev, klevp1, ktrac,   &
                                    ksfc_type, idx_wtr, idx_ice, idx_lnd,   &
                                    !datetime, pdtime,                       &
                                    pdtime,                       &
                                    pzf, pzh, pdz,                          &
                                    papm1, prho, ptm1,                      &
                                    pfrc_tile, ptsfc_tile, psfcWind_tile,   &
                                    pustar_tile, pfheat_tile,               &
                                    pxtm1, pxt_emis, pm6rp, prhop, pvgf, plai)

    USE mo_exception_lite,             ONLY: finish, message
    !USE mtime_lite,                    ONLY: t_datetime => datetime 
    !USE mo_subm_bcs,              ONLY: bc_apply
    !USE mo_subm_jsbach,           ONLY: get_from_jsbach
    USE mo_subm_trac_lite,             ONLY: trlist  
    !USE mo_subm_diag,             ONLY: get_diag_pointer   
    USE mo_ham_lite_main,              ONLY: nclass, sizeclass 
    USE mo_ham_lite_misc,              ONLY: lcheck_ddep, ham_lite_check_r1d, ham_lite_check_wcc
    USE mo_ham_lite_drydep,            ONLY: ham_lite_prep_drydep, ham_lite_drydep

    IMPLICIT NONE

    INTEGER,  INTENT(IN) :: jg,           & ! grid index
                            jb,           & ! index of block loop
                            jcs, jce,     & ! start and end indices of columns loop
                            kblks_c,      & ! number of geographic blocks
                            kproma,       & ! maximum number of columns
                            klev,         & ! number of levels
                            klevp1,       & ! number of levels + 1 
                            ktrac,        & ! number of tracers
                            ksfc_type,    & ! number of surface types 
                            idx_wtr,      & ! index for water
                            idx_ice,      & ! index for ice  
                            idx_lnd         ! index for land
    !TYPE(t_datetime), INTENT(IN), POINTER :: datetime 
    REAL(dp), INTENT (IN) ::  pdtime,               & ! time step 
                              pzf           (:,:),  & ! geometric height above sea level, full level   
                              pzh           (:,:),  & ! geometric height above sea level, half level
                              pdz           (:,:),  & ! geometric height thickness of layer  
                              papm1         (:,:),  & ! air pressure  
                              prho          (:,:),  & ! air density 
                              ptm1          (:,:),  & ! air temperature         
                              pfrc_tile     (:,:),  & ! area fraction of each surface type (land/water/ice) 
                              ptsfc_tile    (:,:),  & ! surface temperature      
                              psfcWind_tile (:,:),  & ! 10m windspeed   
                              pustar_tile   (:,:),  & ! friction velocity   
                              pfheat_tile   (:,:),  & ! heat profile  
                              pxtm1         (:,:,:),& ! tracer mass mixing ratio (or number)
                              pm6rp         (:,:,:,:),& ! aerosol wet radius per mode [m]
                              prhop         (:,:,:,:),& ! mean mode particle density [kg m-3]
                              pvgf          (:),      & ! vegetation fraction
                              plai          (:)         ! leaf area index

    REAL(dp), INTENT(INOUT) :: pxt_emis (:,:) ! tracer tendency due to surface emission [#/m2/s]
           
    INTEGER :: jl, jk, jt, ierr  
    REAL(dp)  ::  zdyvi       (kproma),         & ! dynamics viscosity [kg/m/s]
                  zkivi       (kproma),         & ! kinematic viscosity [m2/s]
                  zmfpa       (kproma),         & ! mean free path [m] 
                  zvdep       (kproma),         & ! dry deposition velocity [m/s]    
                  zdepflux    (kproma),         & ! dry deposition mass flux [#/m2/s]
                  zraero_tile (kproma,ksfc_type)  ! aerodynamic resistance [s/m]
    REAL(dp), POINTER :: fld2d(:,:) ! pointer to diagnostic field
    LOGICAL :: ltest
 
    !$ACC DATA PRESENT(pzf,pzh,pdz,papm1,prho,ptm1,pfrc_tile,ptsfc_tile) &
    !$ACC   PRESENT(psfcWind_tile,pustar_tile,pfheat_tile,pxtm1,pxt_emis) &
    !$ACC   PRESENT(pm6rp,prhop,pvgf,plai) &
    !$ACC   CREATE(zdyvi,zkivi,zmfpa,zvdep,zdepflux,zraero_tile)  

    ! boundary conditions from jsbach
    !CALL get_from_jsbach(jg, jb, jcs, jce, ibc_vgf, 'pheno_fract_fpc', 2) 
    !CALL get_from_jsbach(jg, jb, jcs, jce, ibc_lai, 'pheno_lai', 2)     

    !CALL bc_apply(jg, jb, jcs, jce, ibc_vgf, kblks_c, datetime, zvgf ) 
    !CALL bc_apply(jg, jb, jcs, jce, ibc_lai, kblks_c, datetime, zlai )   
 
    !!$ACC UPDATE DEVICE(zvgf,zlai) ASYNC(1)
    !!$ACC WAIT(1)

    !--- tracer independent computations
    CALL ham_lite_prep_drydep(  jg, jcs, jce, kproma,                       &
                                ksfc_type, idx_wtr, idx_ice, idx_lnd,       &   
                                papm1(:,klev), prho(:,klev), ptm1(:,klev),  & 
                                pfrc_tile, pustar_tile, pfheat_tile,        &
                                zdyvi, zkivi, zmfpa, zraero_tile            )  

    IF (lcheck_ddep) THEN   
      !$ACC UPDATE HOST(zdyvi,zkivi,zmfpa,zraero_tile,pfrc_tile,pustar_tile,pfheat_tile) ASYNC(1) 
      !$ACC WAIT(1)
      CALL ham_lite_check_r1d( jb, jcs, jce, kproma, pvgf, 'drydep_lite_interface', 'pvgf' ) 
      CALL ham_lite_check_r1d( jb, jcs, jce, kproma, plai, 'drydep_lite_interface', 'plai' )
      CALL ham_lite_check_r1d( jb, jcs, jce, kproma, zdyvi, 'drydep_lite_interface', 'zdyvi' )
      CALL ham_lite_check_r1d( jb, jcs, jce, kproma, zkivi, 'drydep_lite_interface', 'zkivi' )
      CALL ham_lite_check_r1d( jb, jcs, jce, kproma, zmfpa, 'drydep_lite_interface', 'zmfpa' )    
      CALL ham_lite_check_r1d( jb, jcs, jce, kproma, zraero_tile(:,idx_lnd), 'drydep_lite_interface', 'zraero_lnd' ) 
      CALL ham_lite_check_r1d( jb, jcs, jce, kproma, zraero_tile(:,idx_wtr), 'drydep_lite_interface', 'zraero_wtr' ) 
      CALL ham_lite_check_r1d( jb, jcs, jce, kproma, zraero_tile(:,idx_ice), 'drydep_lite_interface', 'zraero_ice' )   
    END IF 

    !--- tracer loop
    DO jt = 1,ktrac
 
      !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
      !$ACC LOOP GANG VECTOR  
      DO jl = jcs,jce
        zvdep(jl)     = 0._dp 
        zdepflux(jl)  = 0._dp
      END DO 
      !$ACC END PARALLEL LOOP

      ! deposition velocity
      IF (trlist%ti(jt)%ndrydep > 0) THEN 

        ! dry deposition velocity
        CALL ham_lite_drydep( jg, jb, jcs, jce, jt,                       &
                              kproma, klev,                               &
                              ksfc_type, idx_wtr, idx_ice, idx_lnd,       &
                              pdtime,                                     &  
                              pdz(:,klev), ptm1(:,klev), prho(:,klev),    &
                              pvgf, plai, zdyvi, zkivi, zmfpa,            &
                              pfrc_tile, ptsfc_tile, psfcWind_tile,       &
                              pustar_tile, zraero_tile,                   &
                              zvdep, pm6rp, prhop                         )  

        ! surface emission flux [#/m2/s]
        !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
        !$ACC LOOP GANG VECTOR PRIVATE(ltest)
        DO jl = jcs,jce
          ltest = (pxtm1(jl,klev,jt) > 0.0_dp)
          zdepflux(jl) = MERGE(pxtm1(jl,klev,jt) * prho(jl,klev) * zvdep(jl), 0.0_dp, ltest)  
          pxt_emis(jl,jt) = pxt_emis(jl,jt) - zdepflux(jl)  
        END DO
        !$ACC END PARALLEL LOOP

      END IF 

      ! ! diagnostics
      ! !$ACC UPDATE HOST(zvdep,zdepflux) ASYNC(1) 
      ! !$ACC WAIT(1)
      ! CALL get_diag_pointer(drydep_field(jg)%vddep, fld2d, jt, ierr=ierr)
      ! IF (ierr == 0) THEN 
      !   !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
      !   !$ACC LOOP GANG VECTOR 
      !   DO jl = jcs,jce
      !     fld2d(jl,jb) = zvdep(jl)  
      !   END DO
      !   !$ACC END PARALLEL LOOP
      ! END IF 
      ! CALL get_diag_pointer(drydep_field(jg)%ddep, fld2d, jt, ierr=ierr)
      ! IF (ierr == 0) THEN 
      !   !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
      !   !$ACC LOOP GANG VECTOR 
      !   DO jl = jcs,jce
      !     fld2d(jl,jb) = zdepflux(jl) 
      !   END DO
      !   !$ACC END PARALLEL LOOP
      ! END IF

      ! checks
      IF (lcheck_ddep) THEN    
        !$ACC UPDATE HOST(zdepflux,zvdep,pxtm1) ASYNC(1) 
        !$ACC WAIT(1)
        CALL ham_lite_check_wcc( jb, sizeclass(jt)%shortname, 'mode' )
        CALL ham_lite_check_r1d( jb, jcs, jce, kproma, zdepflux, 'drydep_lite_interface', 'zdepflux' )
        CALL ham_lite_check_r1d( jb, jcs, jce, kproma, zvdep, 'drydep_lite_interface', 'zvdep' )
        CALL ham_lite_check_r1d( jb, jcs, jce, kproma, pxtm1(:,klev,jt), 'drydep_lite_interface', 'pxtm1' ) 
      END IF

    END DO

    !$ACC END DATA

  END SUBROUTINE drydep_lite_interface

END MODULE mo_hammoz_lite_drydep