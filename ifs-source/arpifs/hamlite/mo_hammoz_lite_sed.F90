! 
! based on mo_hammoz_sedimentation.f90 
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

MODULE mo_hammoz_lite_sed

  USE mo_kind_lite,            ONLY: dp
  !USE mo_subm_diag,       ONLY: t_diag_list
  !USE mo_var_list,        ONLY: t_var_list => t_var_list_ptr

  IMPLICIT NONE
  PRIVATE
 
  !--------------------
  ! module interface
  !--------------------

  PUBLIC  ::  sed_lite_interface !init_sed_stream, sed_lite_interface

  !--------------------
  ! module parameters
  !--------------------
 
  INTEGER, PARAMETER  :: nsedivars = 2
  CHARACTER(LEN=32), PARAMETER :: sedivars(1:nsedivars) = (/        &
                                                CHARACTER(len=10):: &
                                                'sed  ',            & ! sedimentation flux
                                                'vsed'    /)          ! sedimentation velocity
 
  CHARACTER(LEN=*), PARAMETER :: thismodule = 'mo_hammoz_sed' 

  !--------------------
  ! module variables
  !--------------------

  ! TYPE t_sedi_field
  !   ! variable pointers and diagnostic lists
  !   TYPE (t_diag_list), PUBLIC  :: sed         ! sedimentation flux
  !   TYPE (t_diag_list), PUBLIC  :: vsed        ! sedimentation velocity
  ! END TYPE t_sedi_field

  ! TYPE(t_sedi_field), ALLOCATABLE, TARGET :: sedi_field(:) ! shape: (n_dom)
  ! TYPE(t_var_list), POINTER :: sedi_field_list(:) ! shape: (n_dom)
  ! TYPE(t_var_list), POINTER :: sedi_field_list1

  CONTAINS

!---------------------------------------------------------------------------------------------------

!   SUBROUTINE init_sed_stream ( p_patch )

!     USE mo_parallel_config,     ONLY: nproma
!     USE mo_impl_constants,      ONLY: MAX_CHAR_LENGTH, SUCCESS
!     USE mo_model_domain,        ONLY: t_patch
!     USE mo_util_str12,          ONLY: st1_in_st2_proof
!     USE mo_util_string,         ONLY: tolower
!     USE mo_exception,           ONLY: finish, message 
!     USE mo_subm_trac,           ONLY: ntrac, trlist 
!     USE mo_subm_trac_para,      ONLY: ln, AEROSOL
!     USE mo_subm_spec,           ONLY: nspec, speclist 
!     USE mo_subm_streams,        ONLY: sedi_tinterval, sedinam, sedi_keytype
!     USE mo_subm_diag,           ONLY: BYTRACER, BYSPECIES, BYNUMMODE, BYMODE   
!     USE mo_ham_lite_main,       ONLY: nclass

!     IMPLICIT none 

!     TYPE(t_patch),INTENT(IN), DIMENSION(:)  :: p_patch

!     INTEGER, PARAMETER            :: ndefault = 2
!     CHARACTER(LEN=32), PARAMETER  :: defnam(1:ndefault)   = (/         &   ! default output
!                                                    CHARACTER(len=10):: &
!                                                    'sed',              &   ! total sedimentation flux
!                                                    'vsed'   /)             ! sedimentation velocity

!     INTEGER                         :: ierr, jt, ist
!     INTEGER                         :: ndomain, jg, nblks, nlev 
!     LOGICAL                         :: tracflag(ntrac), specflag(nspec), modflag(MAX(nclass,1))
!     CHARACTER(LEN=ln)               :: tracname(ntrac), specname(nspec), modname(MAX(nclass,1)), &
!                                        modnumname(MAX(nclass,1))  
!     CHARACTER(len=MAX_CHAR_LENGTH)  :: listname

!     !-- handle ALL, DETAIL and DEFAULT options for sedi output variables
!     !-- Note: ALL and DETAIL are identical for sedi output
!     IF (TRIM(tolower(sedinam(1))) == 'detail')  sedinam(1:nsedivars) = sedivars(:)
!     IF (TRIM(tolower(sedinam(1))) == 'all')     sedinam(1:nsedivars) = sedivars(:)
!     IF (TRIM(tolower(sedinam(1))) == 'default') sedinam(1:ndefault) = defnam(:)

!     !-- check that all variable names from namelist are valid
!     IF (.NOT. st1_in_st2_proof( sedinam, sedivars, ierr=ierr) ) THEN
!       IF (ierr > 0) CALL finish ( 'ini_sedi_stream', 'variable '// &
!                                   sedinam(ierr)//' does not exist in sedi stream' )
!     END IF

!     !-- define the flags and names for the diagnostic lists
!     !   we need one set of flags and names for each key_type (BYTRACER, BYSPECIES, BYMODE)
!     !   gas-phase tracers will always be defined BYTRACER, for aerosol tracers one of the
!     !   following lists will be empty 
!     tracflag(:) = .FALSE.
!     DO jt = 1,ntrac
!       tracname(jt) = trlist%ti(jt)%fullname
!       IF (sedi_keytype == BYTRACER .AND.                  &
!           IAND(trlist%ti(jt)%nphase,AEROSOL) /= 0 .AND.   &   
!           nclass > 0) THEN
!         tracflag(jt) = trlist%ti(jt)%nsedi > 0
!       END IF
!     END DO
!     specflag(:) = .FALSE.
!     DO jt = 1,nspec
!       specname(jt) = speclist(jt)%shortname
!       IF (sedi_keytype == BYSPECIES .AND.                 &
!           IAND(speclist(jt)%nphase, AEROSOL) /= 0 .AND.   &
!           nclass > 0) THEN
!         specflag(jt) = .TRUE.
!       END IF
!     END DO
!     modflag(:) = .false.
!     modname(:) = ''

!     CALL message(TRIM(thismodule),'construction of HAM sedimentation state started')

!     ndomain = SIZE(p_patch)

!     ALLOCATE( sedi_field(ndomain), STAT=ist)
!     !$ACC ENTER DATA COPYIN( sedi_field(ndomain) )

!     IF (ist/=SUCCESS) CALL finish(TRIM(thismodule), 'allocation of sedi_field failed')

!     ALLOCATE( sedi_field_list(ndomain), STAT=ist)
!     !$ACC ENTER DATA COPYIN( sedi_field_list(ndomain) )

!     IF (ist/=SUCCESS) CALL finish(TRIM(thismodule), 'allocation of sedi_field_list failed')

!     DO jg = 1,ndomain

!       nblks = p_patch(jg)%nblks_c
!       nlev  = p_patch(jg)%nlev

!       sedi_field_list1 => sedi_field_list(jg)

!       WRITE(listname,'(a,i2.2)') 'sedi_state_diag_of_domain',jg
!       CALL new_sedi_field_list( jg, nproma, nlev,       nblks,                      &
!                               &  nclass,    ntrac,      nspec,                      &
!                               &  tracflag,  specflag,   modflag,                    &
!                               &  tracname,  specname,   modname,     modnumname,    &
!                               &  TRIM(listname),                                    &
!                               &  sedi_field_list1, sedi_field(jg)                   )

!     END DO

!     CALL message(TRIM(thismodule),'construction of HAM sedimentation state finished')

!   END SUBROUTINE init_sed_stream

! !---------------------------------------------------------------------------------------------------
  
!   SUBROUTINE new_sedi_field_list( jg,         kproma,     klev,       kblks,        &
!                                   kclass,     ktrac,      kspec,                    &
!                                   tracflag,   specflag,   modflag,                  &
!                                   tracname,   specname,   modname,     modnumname,  &                      
!                                   listname,                                         &
!                                   field_list, field                                 ) 

!     USE mo_master_control,      ONLY: get_my_process_name
!     USE mo_var,                 ONLY: level_type_ml 
!     USE mo_var_list_register,   ONLY: vlr_add    
!     USE mo_cdi,                 ONLY: DATATYPE_PACK16, DATATYPE_PACK24,  &
!       &                               DATATYPE_FLT32,  DATATYPE_FLT64  
!     USE mo_io_config,           ONLY: lnetcdf_flt64_output  
!     USE mo_util_str12,          ONLY: st1_in_st2_proof 
!     USE mo_subm_streams,        ONLY: sedi_lpost, sedinam
!     USE mo_subm_trac_para,      ONLY: ln 
!     USE mo_subm_diag,           ONLY: new_diag_list, new_diag,  &
!                                       BYTRACER, BYSPECIES, BYNUMMODE, BYMODE    

!     IMPLICIT none 

!     INTEGER, INTENT(IN) :: jg ! grid index
!     INTEGER, INTENT(IN) :: kproma, klev,  kblks ! dimensions
!     INTEGER, INTENT(IN) :: kclass, ktrac, kspec
!     LOGICAL, INTENT(IN)            :: tracflag(ktrac), specflag(kspec), modflag(MAX(kclass,1))
!     CHARACTER(LEN=ln), INTENT(IN)  :: tracname(ktrac), specname(kspec), modname(MAX(kclass,1)), &
!                                       modnumname(MAX(kclass,1)) 
!     CHARACTER(len=*),  INTENT(IN) :: listname
!     TYPE(t_var_list),  POINTER,   INTENT(INOUT) :: field_list
!     TYPE(t_sedi_field), INTENT(INOUT)           :: field

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
!                   lrestart=.FALSE.                                  )

!     ! sedimentation velocities by tracer or by aerosol mode
!     lpost = st1_in_st2_proof('vsed', sedinam) .AND. sedi_lpost
!     CALL new_diag_list( field%vsed, field_list, diagname='vsed',                        &
!                         longname='sedimentation velocity', units='m s-1',               &
!                         ndims=2, nmaxkey=(/ktrac, 0, kclass, kclass, 0 /), lpost=lpost, &
!                         table=199                                                       )

!     CALL new_diag(field%vsed, ktrac, tracflag, tracname, BYTRACER, shape2d=shape2d )

!     IF (ANY(modflag)) THEN  
!       CALL new_diag(field%vsed, kclass, modflag, modname, BYMODE, shape2d=shape2d)
!       CALL new_diag(field%vsed, kclass, modflag, modnumname, BYNUMMODE, shape2d=shape2d)
!     END IF
 
!     ! add total sedi flux
!     lpost = st1_in_st2_proof('sed', sedinam) .AND. sedi_lpost

!     CALL new_diag_list( field%sed,  field_list, diagname='sed',                         &
!                         longname='sedimentation flux', units='m-2 s-1',                 &
!                         ndims=2, nmaxkey=(/ktrac, 0, kclass, kclass, 0 /), lpost=lpost, &
!                         table=199                                                       )

!     ! add diagnostic elements only when output is activated
!     IF (lpost) THEN
!       CALL new_diag(field%sed, ktrac, tracflag, tracname, BYTRACER, shape2d=shape2d)
!       CALL new_diag(field%sed, kspec, specflag, specname, BYSPECIES, shape2d=shape2d)
!       IF (ANY(modflag)) THEN 
!         CALL new_diag(field%sed, kclass, modflag, modname, BYMODE, shape2d=shape2d)
!         CALL new_diag(field%sed, kclass, modflag, modnumname, BYNUMMODE, shape2d=shape2d)
!       END IF
!     END IF

!   END SUBROUTINE new_sedi_field_list

!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE sed_lite_interface(  jg, jb, jcs, jce, kproma, klev,   & 
                                  pdtime, pdz, pmair, prho, ptm1,   &
                                  papm1, pxtm1, ptend, pm6rp, prhop, pflux_sedi ) 
 
    USE mo_subm_trac_lite,         ONLY: ntrac, trlist
    !USE mo_subm_diag,         ONLY: get_diag_pointer
    USE mo_ham_lite_main,          ONLY: sizeclass, nclass  
    USE mo_ham_lite_misc,          ONLY: lcheck_sed,  &
                                    ham_lite_check_r2d, ham_lite_check_wcc
    USE mo_ham_lite_sed,           ONLY: ham_lite_prep_sed, ham_lite_sed
 
    IMPLICIT none 

    INTEGER,  INTENT(IN)  ::  jg, jb, jcs, jce, kproma, klev
    REAL(dp), INTENT(IN)  ::  pdtime,       & ! time step         
                              pdz   (:,:),  & ! geometric height thickness of layer
                              pmair (:,:),  & ! air mass [kg/m2]
                              prho  (:,:),  & ! density
                              ptm1  (:,:),  & ! temperature
                              papm1 (:,:),  & ! full level pressure
                              pxtm1 (:,:,:),& ! tracer number mixing ratio [#/kg]
                              pm6rp (:,:,:,:),& ! aerosol wet radius per mode [m]
                              prhop (:,:,:,:)   ! mean mode particle density [kg m-3]
    REAL(dp), INTENT(INOUT) ::  ptend(:,:,:)  ! sedimentation tendency [#/kg/s] 

    INTEGER ::  ierr, jk, jl, jt
    REAL(dp)  ::  zvisc     (kproma,klev),  & ! dynamic viscosity
                  zpath     (kproma,klev),  & ! mean free path  
                  zvsed     (kproma,klev),  & ! sedimentation velocity
                  zsedflux  (kproma,klev),  & ! sedimentation flux   
                  zsedtend  (kproma,klev)     ! sedimentation tendency        
    REAL(dp), POINTER :: fld2d(:,:) ! pointer for diagnostics
    REAL(dp)  ::  pflux_sedi (kproma,nclass) ! sedimentation flux
   
    !$ACC DATA PRESENT(pdz,pmair,prho,ptm1,papm1,pxtm1,ptend) &
    !$ACC   CREATE(zvisc,zpath,zvsed,zsedflux,zsedtend)

    !--- tracer independent computations
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR COLLAPSE(2)
    DO jk = 1,klev
      DO jl = jcs,jce
        zvisc(jl,jk) = 0.0_dp
        zpath(jl,jk) = 0.0_dp
      END DO 
    END DO
    !$ACC END PARALLEL LOOP

    CALL ham_lite_prep_sed( jcs, jce, kproma, klev, ptm1, papm1,  zvisc, zpath )

    IF (lcheck_sed) THEN  
      !$ACC UPDATE HOST(zvisc,zpath) ASYNC(1)
      !$ACC WAIT(1) 
      CALL ham_lite_check_r2d( jb, jcs, jce, 1, klev, kproma, zvisc, 'sed_lite_interface', 'zvisc' )
      CALL ham_lite_check_r2d( jb, jcs, jce, 1, klev, kproma, zpath, 'sed_lite_interface', 'zpath' ) 
    END IF 

    !--- tracer loop
    DO jt = 1,ntrac

      !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
      !$ACC LOOP GANG VECTOR COLLAPSE(2)
      DO jk = 1,klev
        DO jl = jcs,jce 
          zvsed(jl,jk)    = 0.0_dp 
          zsedflux(jl,jk) = 0.0_dp 
          zsedtend(jl,jk) = 0.0_dp 
        END DO 
      END DO  
      !$ACC END PARALLEL LOOP

      IF (trlist%ti(jt)%nsedi > 0) THEN   

        ! sedimentation   
        CALL ham_lite_sed(  jg, jb, jcs, jce, jt, kproma, klev, &
                            pdtime,                             &
                            pdz, prho, pmair, pxtm1(:,:,jt),    &
                            zvisc, zpath,                       &
                            zvsed, zsedflux, zsedtend, pm6rp, prhop)

        !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
        !$ACC LOOP GANG VECTOR COLLAPSE(2)
        DO jk = 1,klev
          DO jl = jcs,jce
            ptend(jl,jk,jt) = zsedtend(jl,jk) 
            pflux_sedi(jl,jt) = zsedflux(jl,klev) !eehol: only from surface
          END DO 
        END DO  
        !$ACC END PARALLEL LOOP

      END IF

      ! ! diagnostics
      ! CALL get_diag_pointer(sedi_field(jg)%vsed, fld2d, jt, ierr=ierr)
      ! IF (ierr == 0) THEN 
      !   !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
      !   !$ACC LOOP GANG VECTOR 
      !   DO jl = jcs,jce
      !     fld2d(jl,jb) = zvsed(jl,klev)
      !   END DO 
      !   !$ACC END PARALLEL LOOP
      ! END IF
      ! CALL get_diag_pointer(sedi_field(jg)%sed, fld2d, jt, ierr=ierr)
      ! IF (ierr == 0) THEN 
      !   !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
      !   !$ACC LOOP GANG VECTOR 
      !   DO jl = jcs,jce
      !     fld2d(jl,jb) = zsedflux(jl,klev)
      !   END DO 
      !   !$ACC END PARALLEL LOOP
      ! END IF
  
      ! checks 
      IF (lcheck_sed) THEN  
        !$ACC UPDATE HOST(zvsed,zsedflux,zsedtend) ASYNC(1)
        !$ACC WAIT(1) 
        CALL ham_lite_check_wcc( jb, sizeclass(jt)%shortname, 'mode' )  
        CALL ham_lite_check_r2d( jb, jcs, jce, 1, klev, kproma, zvsed, 'sed_lite_interface', 'zvsed' )
        CALL ham_lite_check_r2d( jb, jcs, jce, 1, klev, kproma, zsedflux, 'sed_lite_interface', 'zsedflux' )
        CALL ham_lite_check_r2d( jb, jcs, jce, 1, klev, kproma, zsedtend, 'sed_lite_interface', 'zsedtend' )
      END IF

    END DO

    !$ACC END DATA

  END SUBROUTINE sed_lite_interface

END MODULE mo_hammoz_lite_sed