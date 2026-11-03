! 
! based on mo_hammoz_wetdep.f90 
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

MODULE mo_hammoz_lite_wetdep

  USE mo_kind_lite,            ONLY: dp
  !USE mo_subm_diag,       ONLY: t_diag_list
  !USE mo_var_list,        ONLY: t_var_list => t_var_list_ptr
  USE mo_ham_lite_main,        ONLY: nclass
  
#ifndef _OPENACC
#define __acc_attach(ptr)
#endif

  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------
 
  ! PUBLIC  ::  init_wetdep_stream,   & 
  !             wetdep_lite_interface
  PUBLIC  ::  wetdep_lite_interface

  !--------------------
  ! module parameters
  !--------------------

  INTEGER, PARAMETER  :: nwetdepvars = 1
  CHARACTER(LEN=32), PARAMETER  :: wetdepvars(1:nwetdepvars) = (/ 'wdep' /) ! wet deposition flux     

  CHARACTER(LEN=*), PARAMETER :: thismodule = 'mo_hammoz_wetdep'

  !--------------------
  ! module variables
  !--------------------

  ! TYPE t_wetdep_field
  !   ! variable pointers and diagnostic lists 
  !   TYPE (t_diag_list), POINTER   :: wdep ! wet deposition flux 
  ! END TYPE t_wetdep_field

  !TYPE(t_wetdep_field), ALLOCATABLE, TARGET :: wetdep_field(:) ! shape: (n_dom)
  !TYPE(t_var_list), POINTER :: wetdep_field_list(:) ! shape: (n_dom)
  !TYPE(t_var_list), POINTER :: wetdep_field_list1

  CONTAINS

!---------------------------------------------------------------------------------------------------

!   SUBROUTINE init_wetdep_stream( p_patch )

!     USE mo_util_str12,          ONLY: st1_in_st2_proof
!     USE mo_util_string,         ONLY: tolower
!     USE mo_exception,           ONLY: finish, message  
!     USE mo_subm_trac,           ONLY: ntrac, trlist, validate_traclist
!     USE mo_subm_trac_para,      ONLY: ln, GAS, AEROSOL
!     USE mo_subm_spec,           ONLY: nspec, speclist
!     USE mo_subm_streams,        ONLY: wetdepnam, wetdep_keytype
!     USE mo_subm_diag,           ONLY: BYTRACER, BYSPECIES, BYMODE, BYNUMMODE
!     USE mo_parallel_config,     ONLY: nproma
!     USE mo_model_domain,        ONLY: t_patch
!     USE mo_impl_constants,      ONLY: MAX_CHAR_LENGTH, SUCCESS

!     IMPLICIT none 

!     TYPE(t_patch),INTENT(IN), DIMENSION(:)  :: p_patch

!     INTEGER, PARAMETER  :: ndefault = 1
!     INTEGER, PARAMETER  :: nall     = 1
!     CHARACTER(LEN=32), PARAMETER  :: defnam(1:ndefault) = (/ 'wdep' /)  ! wet deposition flux 
!     CHARACTER(LEN=32), PARAMETER  :: allnam(1:nall) = (/ 'wdep' /)  ! wet deposition flux    
!     CHARACTER(len=ln), PARAMETER  :: defaultgas(1) = (/ '    ' /) ! default diagnostics for gas-phase tracers
!     INTEGER :: ierr, jt, ist 
!     INTEGER :: jg, ndomain, nblks, nlev
!     LOGICAL :: tracflag(ntrac), specflag(nspec), modflag(MAX(nclass,1))
!     LOGICAL :: lpost
!     CHARACTER(LEN=ln) :: tracname(ntrac), specname(nspec), modname(MAX(nclass,1)), modnumname(MAX(nclass,1))  
!     CHARACTER(LEN=MAX_CHAR_LENGTH) :: listname  
 
!     !-- handle ALL, DETAIL and DEFAULT options for wetdep output variables
!     !-- note: ALL and DETAIL are identical
!     IF (TRIM(tolower(wetdepnam(1))) == 'detail')  wetdepnam(1:nwetdepvars) = wetdepvars(:)  
!     IF (TRIM(tolower(wetdepnam(1))) == 'all')     wetdepnam(1:nall) = allnam(:)
!     IF (TRIM(tolower(wetdepnam(1))) == 'default') wetdepnam(1:ndefault) = defnam(:)
 
!     !-- check that all variable names from namelist are valid
!     IF (.NOT. st1_in_st2_proof( wetdepnam, wetdepvars, ierr=ierr) ) THEN
!       IF (ierr > 0) CALL finish ( 'ini_wetdep_stream', 'variable '// &
!                                   wetdepnam(ierr)//' does not exist in wetdep stream' )
!     END IF
 
!     !-- define the flags and names for the diagnostic lists. We need one set of flags and
!     !   names for each key_type (BYTRACER, BYSPECIES, BYMODE)
!     !   gas-phase tracers will always be defined BYTRACER, for aerosol tracers one of the
!     !   following lists will be empty.
!     !   Note: wdep uses BYTRACER, BYSPECIES or BYMODE
!     tracflag(:) = .FALSE.
!     DO jt = 1,ntrac
!       tracname(jt) = trlist%ti(jt)%fullname
!       IF (wetdep_keytype == BYTRACER .AND. nclass > 0) THEN
!         tracflag(jt) = trlist%ti(jt)%nwetdep > 0 
!       END IF
!     END DO
!     specflag(:) = .FALSE.
!     DO jt = 1,nspec
!       specname(jt) = speclist(jt)%shortname
!       IF (wetdep_keytype == BYSPECIES .AND.           &
!         IAND(speclist(jt)%nphase, AEROSOL) /= 0 .AND. &  
!         nclass > 0) THEN
!           specflag(jt) = speclist(jt)%lwetdep
!       END IF
!     END DO
!     modflag(:) = .FALSE.
!     modname(:) = ''

!     CALL message(TRIM(thismodule), 'construction of HAM wetdep state started')

!     ndomain = SIZE(p_patch)

!     ALLOCATE( wetdep_field(ndomain), STAT=ist)
!     !$ACC ENTER DATA COPYIN( wetdep_field(ndomain) )

!     IF (ist/=SUCCESS) CALL finish(TRIM(thismodule), 'allocation of wetdep_field failed')

!     ALLOCATE( wetdep_field_list(ndomain), STAT=ist)
!     !$ACC ENTER DATA COPYIN( wetdep_field_list(ndomain) )

!     IF (ist/=SUCCESS) CALL finish(TRIM(thismodule), 'allocation of wetdep_field_list failed')

!     DO jg = 1,ndomain

!       nblks = p_patch(jg)%nblks_c
!       nlev  = p_patch(jg)%nlev

!       wetdep_field_list1 => wetdep_field_list(jg)

!       WRITE(listname,'(a,i2.2)') 'wetdep_state_diag_of_domain',jg
!       CALL new_wetdep_field_list( jg,        nproma,     nlev,        nblks,               &
!                                &  nclass,    ntrac,      nspec,                            &
!                                &  tracflag,  specflag,   modflag,                          &
!                                &  tracname,  specname,   modname,     modnumname,          &
!                                &  TRIM(listname),                                          &
!                                &  wetdep_field_list1, wetdep_field(jg)                     )

!     END DO

!     CALL message(TRIM(thismodule),'construction of HAM wetdep state finished')

!   END SUBROUTINE init_wetdep_stream

! !---------------------------------------------------------------------------------------------------

!   SUBROUTINE new_wetdep_field_list( jg,         kproma,     klev,       kblks,        &
!                                  &  kclass,     ktrac,      kspec,                    &
!                                  &  tracflag,   specflag,   modflag,                  &
!                                  &  tracname,   specname,   modname,     modnumname,  &                      
!                                  &  listname,                                         &
!                                  &  field_list, field                                 ) 

!     USE mo_master_control,      ONLY: get_my_process_name
!     USE mo_var,                 ONLY: level_type_ml 
!     USE mo_var_list_register,   ONLY: vlr_add 
!     USE mo_cdi,                 ONLY: DATATYPE_PACK16, DATATYPE_PACK24,  &
!       &                               DATATYPE_FLT32,  DATATYPE_FLT64 
!     USE mo_io_config,           ONLY: lnetcdf_flt64_output 
!     USE mo_subm_diag,           ONLY: new_diag_list, new_diag,  &
!                                       BYTRACER, BYSPECIES, BYNUMMODE, BYMODE 
!     USE mo_subm_streams,        ONLY: wetdep_lpost, wetdepnam 
!     USE mo_subm_trac_para,      ONLY: ln
!     USE mo_util_str12,          ONLY: st1_in_st2_proof  

!     INTEGER, INTENT(IN) :: jg ! grid index
!     INTEGER, INTENT(IN) :: kproma, klev,  kblks ! dimensions
!     INTEGER, INTENT(IN) :: kclass, ktrac, kspec
!     LOGICAL, INTENT(IN)            :: tracflag(ktrac), specflag(kspec), modflag(MAX(kclass,1))
!     CHARACTER(LEN=ln), INTENT(IN)  :: tracname(ktrac), specname(kspec), modname(MAX(kclass,1)), &
!                                       modnumname(MAX(kclass,1)) 
!     CHARACTER(len=*),  INTENT(IN) :: listname
!     TYPE(t_var_list),  POINTER,   INTENT(INOUT) :: field_list
!     TYPE(t_wetdep_field), INTENT(INOUT)         :: field
 
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

!     model_type = get_my_process_name()

!     CALL vlr_add( field_list, TRIM(listname), patch_id=jg,          &
!                   vlevel_type=level_type_ml, model_type=model_type, &
!                   lrestart=.FALSE., loutput= wetdep_lpost           )         

!     !-- pure diagnostic quantities (averaged)

!     ! add total wetdep flux  
!     ALLOCATE( field%wdep ) 
   
!     lpost = st1_in_st2_proof( 'wdep', wetdepnam) .AND. wetdep_lpost
!     CALL new_diag_list( field%wdep, field_list, diagname='wdep',    &
!                         longname='wet deposition flux total',       &
!                         units='m-2 s-1', ndims=2,                   &
!                         nmaxkey=(/ktrac, 0, kclass, kclass, 0 /),   &
!                         table=199, lpost=lpost                      )

!     ! add diagnostic elements only when output is activated
!     IF (lpost) THEN
!       IF (ANY(tracflag)) CALL new_diag(field%wdep, ktrac, tracflag, tracname, BYTRACER, shape2d=shape2d)
!       IF (ANY(specflag)) CALL new_diag(field%wdep, kspec, specflag, specname, BYSPECIES, shape2d=shape2d)
!       IF (ANY(modflag))  THEN
!         CALL new_diag(field%wdep, kclass , modflag,  modname,    BYMODE,  shape2d=shape2d)    
!         CALL new_diag(field%wdep, kclass , modflag,  modnumname, BYNUMMODE,  shape2d=shape2d)  
!       END IF
!     END IF 

!   END SUBROUTINE new_wetdep_field_list

!---------------------------------------------------------------------------------------------------

  SUBROUTINE wetdep_lite_interface( jg, jb, jcs, jce, jks,                  &
                                    klev, kproma,                           &
                                    pdtime,                                 &
                                    prho, pxlta, pxlqc, pxlqi,              &
                                    pxlqr, pxlqs, pxlqg,                    &
                                    pxprr, pxpri, pxprs, pxprg,             &
                                    pmair, pxtm1, pxtte, pflux_wdep         )
 
    USE mo_mpi_lite,               ONLY: p_pe, p_io
    USE mo_subm_trac_lite,         ONLY: ntrac, trlist  
    !USE mo_subm_diag,         ONLY: get_diag_pointer
    USE mo_ham_lite_main,          ONLY: nclass, sizeclass  
    USE mo_ham_lite_misc,          ONLY: lcheck_wdep,  &
                                    ham_lite_check_r1d, ham_lite_check_r2d, ham_lite_check_wcc 
    USE mo_ham_lite_wetdep,        ONLY: ham_lite_prep_wetdep, ham_lite_wetdep   

    INTEGER, INTENT(IN) ::  jg,         & ! grid index
                            jb,         & ! index of block loop
                            jcs, jce,   & ! start and end indices of columns loop  
                            jks,        & ! index of top level
                            klev,       & ! number of levels 
                            kproma        ! maximum number of columns 
    REAL(dp), INTENT(IN) :: pdtime,               &   ! time step 
                            prho  (kproma,klev),  &   ! air density
                            pxlta (kproma,klev),  &   ! air temperature
                            pxlqc (kproma,klev),  &   ! cloud water
                            pxlqi (kproma,klev),  &   ! cloud ice
                            pxlqr (kproma,klev),  &   ! rain  
                            pxlqs (kproma,klev),  &   ! snow
                            pxlqg (kproma,klev),  &   ! graupel
                            pxprr (kproma),       &   ! precip rate rain    
                            pxpri (kproma),       &   ! precip rate ice    
                            pxprs (kproma),       &   ! precip rate snow    
                            pxprg (kproma),       &   ! precip rate graupel  
                            pmair (kproma,klev),  &   ! air mass [kg/m2]
                            pxtm1 (kproma,klev,ntrac) ! tracer number mixing ratio  
    REAL(dp), INTENT(INOUT) :: pxtte  (kproma,klev,ntrac)  ! tracer number mixing ratio tendency 
    REAL(dp), INTENT(INOUT) :: pflux_wdep  (kproma,ntrac)  ! tracer wdep flux
    
    INTEGER :: ierr, jk, jl, jt 
    REAL(dp) :: zeffic    (kproma),       &
                zratio    (kproma,klev),  &
                zdepflux  (kproma) 
    REAL(dp), POINTER :: fld2d(:,:) ! pointer for diagnostics 

    !--- tracer independent computations
    DO jl = jcs,jce
      zeffic(jl) = 0.0_dp 
    END DO

    DO jk = jks,klev
      DO jl = jcs,jce 
        zratio(jl,jk) = 0.0_dp 
      END DO
    END DO 

    CALL ham_lite_prep_wetdep(  jcs, jce, jks, klev, kproma,        & 
                                pxlta, pxlqc, pxlqi,                &
                                pxlqr, pxlqs, pxlqg,                & 
                                pxprr, pxpri, pxprs, pxprg, pmair,  & 
                                zeffic, zratio                      )     

    !--- tracer loop
    DO jt = 1,ntrac
  
      DO jl = jcs,jce
        zdepflux(jl)  = 0.0_dp  
      END DO 

      IF (trlist%ti(jt)%nwetdep > 0) THEN    

        ! wet deposition 
        CALL ham_lite_wetdep( jcs, jce, jks, jt, klev, kproma,            & 
                              pdtime,                                     &
                              pxlta, pxlqc, pxlqi, pmair, pxtm1(:,:,jt),  &
                              zeffic, zratio,                             &
                              pxtte(:,:,jt), zdepflux                     )    
  
      END IF

      ! ! diagnostics
      ! CALL get_diag_pointer(wetdep_field(jg)%wdep, fld2d, jt, ierr=ierr)
      ! IF (ierr == 0) THEN 
      !   DO jl = jcs,jce
      !     fld2d(jl,jb) = zdepflux(jl) 
      !   END DO 
      ! END IF
      DO jl = jcs,jce
        pflux_wdep(jl,jt) = zdepflux(jl) !add wet dep flux to diagnostics
      END DO

    END DO  

    ! checks 
    IF (lcheck_wdep) THEN  
      CALL ham_lite_check_r1d( jb, jcs, jce, kproma, zeffic, 'wetdep_lite_interface', 'zeffic' )
      CALL ham_lite_check_r2d( jb, jcs, jce, jks, klev, kproma, zratio, 'wetdep_lite_interface', 'zratio' )
      DO jt = 1,ntrac 
        CALL ham_lite_check_wcc( jb, sizeclass(jt)%shortname, 'mode' ) 
        CALL ham_lite_check_r2d( jb, jcs, jce, jks, klev, kproma, pxtte(:,:,jt), 'wetdep_lite_interface', 'pxtte' )
      END DO 
    END IF  

  END SUBROUTINE wetdep_lite_interface

END MODULE mo_hammoz_lite_wetdep
