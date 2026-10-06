! 
! based on mo_hammoz_emissions.f90
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

MODULE mo_hammoz_lite_emi

  USE mo_kind_lite,              ONLY: dp 
  !USE mo_var_list,          ONLY: t_var_list => t_var_list_ptr 
  USE mo_subm_spec_lite,         ONLY: nmaxspec
  !USE mo_subm_diag,         ONLY: t_diag_list
  USE mo_hammoz_lite_emi_mat,    ONLY: maxsectors, EM_NONE, EM_SURFACE, EM_VOLUME, EM_LEVEL50M, EM_FIRE

  IMPLICIT NONE
  PRIVATE
 
  !--------------------
  ! module interface
  !--------------------

  PUBLIC  ::  &
              init_emi,               & 
              !init_emi_stream,        &
              emi_read_bc_interface,  &  
              emi_lite_interface,     &
              emi_lite_number 

  !--------------------
  ! module parameters
  !--------------------

  INTEGER, PARAMETER  :: nemivars = 1 

  CHARACTER(LEN=32), PARAMETER :: emivars(1:nemivars) = (/ 'emi' /) ! emissions mass flux as 2D field

  CHARACTER(len=*), PARAMETER :: thismodule = 'mo_ham_emissions'

  !--------------------
  ! module variables
  !--------------------
   
  INTEGER     :: nsectors
  INTEGER     :: emimod(maxsectors,nmaxspec)     ! mode of application
  INTEGER     :: ibc_emis(maxsectors,nmaxspec)   ! index to boundary condition
  REAL(dp)    :: emfactor(maxsectors,nmaxspec)   ! scaling factor
   
  LOGICAL     :: lhas_volcc, lhas_volce ! sectors for continuous and explosive volcanoes defined 
  LOGICAL     :: lhas_biogenic          ! interactive vegetation emissions active (organics)
 
  ! TYPE t_hammoz_emissions
  !   TYPE (t_diag_list) :: emi_diag                      ! emissions mass flux diagnostics (total)
  !   TYPE (t_diag_list) :: emi_diag_detail(maxsectors)   ! diagnostics per sector
  ! END TYPE t_hammoz_emissions

  !TYPE (t_hammoz_emissions), ALLOCATABLE :: emi_field(:) ! shape: ndom  
  
  !TYPE (t_var_list), POINTER   :: emi_field_list(:) ! shape: ndom  
  !TYPE (t_var_list), POINTER   :: emi_field_list1
 
  CONTAINS

!---------------------------------------------------------------------------------------------------
 
  ! read emi_spec.dat and set-up arrays

  SUBROUTINE init_emi( p_patch, datetime )

    USE mtime_lite,                    ONLY: t_datetime => datetime
    USE mo_mpi_lite,                   ONLY: p_pe, p_io  
    USE mo_model_domain_lite,          ONLY: t_patch
    USE mo_exception_lite,             ONLY: finish, message, message_text 
    USE mo_util_extfield_lite,         ONLY: EF_MODULE, EF_INACTIVE
    USE mo_subm_main_lite,             ONLY: lemissions 
    USE mo_subm_bcs_lite,              ONLY: bc_nml, BC_BOTTOM, bc_define
    USE mo_subm_spec_lite,             ONLY: speclist, query_species 
    USE mo_subm_trac_lite,             ONLY: trlist, ntrac
    USE mo_hammoz_lite_emi_vol,        ONLY: init_emi_volcano 
    USE mo_hammoz_lite_emi_mat,        ONLY: em_read, em_get_sector_info, em_get_bc_from_matrix
    USE mo_ham_lite_main,              ONLY: nclass, sizeclass, ibc_dust, ibc_seasalt 
    USE mo_ham_lite_spec,              ONLY: id_so2, id_so4
    USE mo_ham_lite_emi,               ONLY: ham_lite_init_emi

    IMPLICIT NONE

    TYPE(t_patch), INTENT(IN) :: p_patch(:)
    TYPE(t_datetime), POINTER, INTENT(IN) :: datetime
 
    INTEGER           ::  jsect, jspec, jmode, jname, jvar, &
                          nvars, nsect, idims, emtype, idum, jtrac, p_id
    REAL(dp)          ::  zfactor, zeps
    CHARACTER(LEN=64) ::  secname, shortname
    CHARACTER(LEN=3)  ::  cspec
    TYPE(bc_nml)      ::  bc_struc

    IF ( SIZE(p_patch) .NE. 1 ) THEN
      CALL finish ( 'init_emissions','nesting not supported')
    ELSE
      p_id = 1  
    END IF

    ! -- initialize
    zeps = EPSILON(1.0_dp)
    emimod(:,:) = EM_NONE
    emfactor(:,:) = 0._dp
    lhas_volcc = .FALSE.
    lhas_volce = .FALSE. 

    ! -- read emission matrix from emi_spec.txt file
    CALL em_read(nsectors)

    lemissions = .FALSE.
    DO jsect = 1, nsectors
      CALL em_get_sector_info(jsect, secname, nvars)
      DO jvar = 1,nvars
        CALL em_get_bc_from_matrix(jsect, jvar, secname, bc_struc, emtype, zfactor, shortname)
        ! determine geometry of emissions 
        IF (bc_struc%ef_type   == EF_MODULE) bc_struc%ef_actual_unit = 'kg m-2 s-1'
        IF (bc_struc%bc_domain == BC_BOTTOM) THEN
          idims = 2
        ELSE
          idims = 3
        END IF

        ! get species index
        CALL query_species(shortname=shortname, index=jspec)
        IF (jspec > 0) THEN 
          lemissions = .TRUE.
          write(cspec,'(i3)') jspec
          CALL message('', '')
          CALL message('init_lite_emissions ', &
            'jspec='//cspec//': varname='//TRIM(shortname)//', sector='//TRIM(secname))

          ! use emi_matrix information to define a boundary condition
          ibc_emis(jsect,jspec) = bc_define(p_patch(p_id)%id, TRIM(secname)// &
            ' emissions of '//shortname, bc_struc, idims, .TRUE.)

          ! mark dust/seasalt bcs for latter use 
          IF (TRIM(secname) == 'DUST') ibc_dust = ibc_emis(jsect,jspec)
          IF (TRIM(secname) == 'SEASALT') ibc_seasalt = ibc_emis(jsect,jspec)

          ! set emission handler
          emimod(jsect,jspec) = emtype

          ! set scaling factor for emissions
          emfactor(jsect,jspec) = zfactor    
          !IF ((abs((zfactor - 1.0_dp)) > zeps) .AND. (p_pe == p_io )) THEN
          !  write (message_text,'(a,e9.2)') 'emissions will be scaled with a factor of ', zfactor
          !  CALL message('init_emissions', message_text)
          !ENDIF

          ! set emission flag
          speclist(jspec)%lemis = .TRUE.
          ! if so2 is found, pretend that so4 is found as well 
          IF ((jspec == id_so2) .AND. (id_so4 > 0)) THEN
            speclist(id_so4)%lemis = .true.
            emimod(jsect,id_so4) = emtype
            emfactor(jsect,id_so4) = zfactor
          ENDIF

          DO jtrac = 1,ntrac 
            DO jmode = 1,nclass
              IF (sizeclass(jmode)%idt_no == jtrac) THEN
                nsect = sizeclass(jmode)%lite_nemi
                DO jname = 1,nsect
                  IF (sizeclass(jmode)%lite_iemi(jname) == TRIM(secname)) trlist%ti(jtrac)%nemis = 1    
                ENDDO
              ENDIF
            ENDDO
          ENDDO

          ! if dust or seasalt are defined by module, another bc is needed  
          IF (((TRIM(secname) == 'DUST') .or. (TRIM(secname) == 'SEASALT')) .and. &
            (bc_struc%ef_type == EF_MODULE)) THEN

            ! the second bc will get the next bc-index (therefore there is no need to store the index)
            idum = bc_define(p_patch(p_id)%id, TRIM(secname)//' emissions of '//TRIM(shortname)//'2', &
              bc_struc, idims, .TRUE.)
            IF (TRIM(secname) == 'SEASALT') THEN
              idum = bc_define(p_patch(p_id)%id, TRIM(secname)//' emissions of '//TRIM(shortname)//'3', &
                bc_struc, idims, .TRUE.)
              idum = bc_define(p_patch(p_id)%id, TRIM(secname)//' emissions of '//TRIM(shortname)//'4', &
                bc_struc, idims, .TRUE.)
            ENDIF  

          ENDIF 

          ! set flags for special emissions
          IF ((TRIM(secname) == 'VOLCC') .AND. (bc_struc%ef_type /= EF_INACTIVE)) lhas_volcc = .TRUE.
          IF ((TRIM(secname) == 'VOLCE') .AND. (bc_struc%ef_type /= EF_INACTIVE)) lhas_volce = .TRUE.
          IF ((TRIM(secname) == 'BIOGENIC') .AND. (bc_struc%ef_type /= EF_INACTIVE)) lhas_biogenic = .TRUE. 
        ELSE
            WRITE(message_text,'(6a)') 'Species ',TRIM(shortname),  &
              ' (requested in the emissions matrix by sector ',     &
              TRIM(secname),') is not defined by any submodel! ',   &
              'You may check and revise your namelists settings'   
            CALL message('init_emissions', TRIM(message_text)) 
        END IF
      END DO
    END DO

    IF (lhas_volcc) CALL init_emi_volcano( p_patch(p_id)%id, 1 )
    IF (lhas_volce) CALL init_emi_volcano( p_patch(p_id)%id, 2 )

    CALL ham_lite_init_emi( nsectors, p_patch(p_id), datetime )

    IF (.NOT. lemissions) CALL message('init_lite_emi', 'no emissions defined')

  END SUBROUTINE init_emi

!---------------------------------------------------------------------------------------------------

  ! define emi diagnostic stream

!   SUBROUTINE init_emi_stream( p_patch ) 

!     USE mo_model_domain,        ONLY: t_patch  
!     USE mo_impl_constants,      ONLY: SUCCESS, MAX_CHAR_LENGTH
!     USE mo_parallel_config,     ONLY: nproma
!     USE mo_util_str12,          ONLY: st1_in_st2_proof
!     USE mo_util_string,         ONLY: tolower
!     USE mo_exception,           ONLY: finish, message 
!     USE mo_subm_trac,           ONLY: ntrac, trlist, validate_traclist
!     USE mo_subm_trac_para,      ONLY: ln, GAS, AEROSOL 
!     USE mo_subm_spec,           ONLY: nspec, speclist 
!     USE mo_subm_streams,        ONLY: eminam, emi_keytype
!     USE mo_subm_diag,           ONLY: BYTRACER, BYSPECIES 
!     USE mo_ham_lite_main,       ONLY: nclass

!     IMPLICIT NONE

!     TYPE(t_patch),INTENT(IN), DIMENSION(:)  :: p_patch

!     INTEGER, PARAMETER            :: ndefault = 1
!     CHARACTER(LEN=32), PARAMETER  :: defnam(1:ndefault) = (/ 'emi' /) ! total emission mass flux
!     CHARACTER(len=ln), PARAMETER  :: defaultgas(1) = (/ ' ' /) ! default diagnostics for gas-phase tracers

!     INTEGER                         :: ierr, jt
!     INTEGER                         :: ndomain, jg, ist, nblks, nlev
!     LOGICAL                         :: tracflag(ntrac), specflag(nspec)
!     LOGICAL                         :: ldiagdetail(maxsectors)
!     CHARACTER(LEN=ln)               :: tracname(ntrac), specname(nspec) 
!     CHARACTER(LEN=64)               :: sectemp, secname, cdiagname
!     CHARACTER(LEN=256)              :: cdiaglongname  
!     CHARACTER(len=MAX_CHAR_LENGTH)  :: listname 
  
!     ldiagdetail(:) = .TRUE. ! submodels may switch off detailed diagnostics for specific sectors

!     !-- handle ALL and DEFAULT options for emi output variables
!     IF (TRIM(tolower(eminam(1))) == 'all')     eminam(1:nemivars) = emivars(:)
!     IF (TRIM(tolower(eminam(1))) == 'default') eminam(1:ndefault) = defnam(:)

!     !-- check that all variable names from namelist are valid
!     IF (.NOT. st1_in_st2_proof( eminam, emivars, ierr=ierr) ) THEN
!       IF (ierr > 0) CALL finish ( 'ini_emi_stream', 'variable '// &
!                                   eminam(ierr)//' does not exist in emi stream' )
!     END IF
 
!     !-- define the flags and names for the diagnostic lists. We need one set of flags and
!     !   names for each key_type (BYTRACER, BYSPECIES, BYMODE)
!     !   gas-phase tracers will always be defined BYTRACER, for aerosol tracers one of the
!     !   following lists will be empty.
!     !   Note: vddep uses BYTRACER or BYMODE, ddep uses BYTRACER or BYSPECIES
!     tracflag(:) = .FALSE.
!     DO jt = 1,ntrac
!       tracname(jt) = trlist%ti(jt)%fullname
!       IF (emi_keytype == BYTRACER .AND. nclass > 0) THEN
!         tracflag(jt) = trlist%ti(jt)%nemis > 0 
!       END IF
!     END DO
!     specflag(:) = .FALSE.
!     DO jt = 1,nspec
!       specname(jt) = speclist(jt)%shortname
!       IF (emi_keytype == BYSPECIES .AND. IAND(speclist(jt)%nphase, AEROSOL) /= 0 .AND. nclass > 0) THEN
!         specflag(jt) = speclist(jt)%lemis
!       END IF
!     END DO

!     ndomain = SIZE(p_patch)

!     CALL message(TRIM(thismodule),'construction of HAMMOZ emi started')

!     ALLOCATE( emi_field(ndomain), STAT=ist)
!     !$ACC ENTER DATA COPYIN( emi_field(ndomain) )

!     IF (ist/=SUCCESS) CALL finish(TRIM(thismodule), 'allocation of emi_field failed')

!     ALLOCATE( emi_field_list(ndomain), STAT=ist)
!     !$ACC ENTER DATA COPYIN( emi_field_list(ndomain) )

!     IF (ist/=SUCCESS) CALL finish(TRIM(thismodule), 'allocation of emi_field_list failed')
 
!     DO jg = 1,ndomain

!       nblks = p_patch(jg)%nblks_c
!       nlev  = p_patch(jg)%nlev

!       emi_field_list1 => emi_field_list(jg)

!       WRITE(listname,'(a,i2.2)') 'hammoz_emission_diag_of_domain', jg

!       CALL new_hammoz_emission_list(  jg, nproma, nlev, nblks,                          &
!                                       TRIM(listname), emi_field_list1, emi_field(jg),   &
!                                       tracflag, specflag, tracname, specname,           &
!                                       ldiagdetail                                       )

!    END DO

!    CALL message(TRIM(thismodule),'construction of HAMMOZ emi finished')

 
!  END SUBROUTINE init_emi_stream

!---------------------------------------------------------------------------------------------------

  ! SUBROUTINE new_hammoz_emission_list(  jg, kproma, klev, kblks,                  &
  !                                       listname, emi_field_list, emi_field,      &
  !                                       tracflag, specflag, tracname, specname,   &
  !                                       ldiagdetail                               ) 
  
  !   USE mo_master_control,      ONLY: get_my_process_name
  !   USE mo_var,                 ONLY: level_type_ml 
  !   USE mo_var_list_register,   ONLY: vlr_add
  !   USE mo_var_list,            ONLY: add_var, add_ref
  !   USE mo_util_str12,          ONLY: st1_in_st2_proof
  !   USE mo_util_string,         ONLY: tolower
  !   USE mo_subm_diag,           ONLY: new_diag_list, new_diag, new_diag_element
  !   USE mo_subm_streams,        ONLY: eminam, emi_lpost, emi_lpost_sector, emi_tinterval
  !   USE mo_subm_spec,           ONLY: speclist, nspec
  !   USE mo_subm_trac,           ONLY: ntrac 
  !   USE mo_subm_trac_para,      ONLY: ln
  !   USE mo_subm_diag,           ONLY: BYTRACER, BYSPECIES
  !   USE mo_subm_streams,        ONLY: emi_lpost  
  !   USE mo_hammoz_emi_mat,      ONLY: em_get_sector_info   

  !   IMPLICIT NONE

  !   INTEGER,INTENT(IN) :: jg, kproma, klev, kblks 
  !   CHARACTER(len=*), INTENT(IN) :: listname
  !   CHARACTER(LEN=ln), INTENT(IN) ::  tracname(ntrac), &
  !                                     specname(nspec)
  !   LOGICAL, INTENT(IN) ::  tracflag(ntrac),        &
  !                           specflag(nspec),        &
  !                           ldiagdetail(maxsectors)
  !   TYPE(t_var_list), POINTER, INTENT(INOUT) :: emi_field_list
  !   TYPE(t_hammoz_emissions), INTENT(INOUT) :: emi_field
 
  !   INTEGER             :: jsec, jspec, nvars
  !   INTEGER             :: shape2d(2), shape3d(3)
  !   INTEGER             :: table ! grib table
  !   CHARACTER(LEN=64)   :: sectemp, secname, cdiagname
  !   CHARACTER(LEN=256)  :: cdiaglongname
  !   CHARACTER(LEN=32)   :: cunit 
  !   CHARACTER(:), ALLOCATABLE :: model_type
  !   LOGICAL             :: lpost

  !   table=199

  !   shape2d  = (/kproma,       kblks/)
  !   shape3d  = (/kproma, klev, kblks/)

  !   model_type = get_my_process_name()

  !   CALL vlr_add( emi_field_list, TRIM(listname), patch_id=jg,        &
  !                 vlevel_type=level_type_ml, model_type=model_type,   & 
  !                 lrestart=.TRUE., loutput=emi_lpost                  )

  !   !-- add emission mass flux diagnostics (accumulated) 

  !   lpost = st1_in_st2_proof( 'emi', eminam) .AND. emi_lpost
  !   CALL new_diag_list( emi_field%emi_diag, emi_field_list, diagname='emi',       &
  !                       longname='accumulated emission mass flux',                &
  !                       units='m-2 s-1', ndims=2,                                 &
  !                       nmaxkey=(/ntrac, 0, 0, 0, 0 /), lpost=lpost, table=199,   &
  !                       lrestart=.FALSE. )

  !   ! add diagnostic elements only when output is activated
  !   IF (lpost .AND. (ANY(tracflag) .OR. ANY(specflag))) THEN
  !     CALL new_diag(emi_field%emi_diag, ntrac, tracflag, tracname, BYTRACER, shape2d=shape2d)
  !     CALL new_diag(emi_field%emi_diag, nspec, specflag, specname, BYSPECIES, shape2d=shape2d)
  !   END IF
 
  !   !-- add detailed emission mass flux diagnostics (per sector)
  !   ! for now only BYSPECIES
  !   IF (emi_lpost_sector) THEN 
  !     DO jsec = 1, nsectors
  !       CALL em_get_sector_info(jsec, secname, nvars)
  !       IF (nvars > 0) THEN
  !         sectemp=tolower(secname) 
  !         CALL new_diag_list (emi_field%emi_diag_detail(jsec), emi_field_list,                &
  !                             diagname='emi_'//TRIM(sectemp),                                 &
  !                             longname='accumulated '//TRIM(sectemp)//' emission mass flux',  &
  !                             units='kg m-2 s-1',                                             &
  !                             ndims=2, nmaxkey=(/0, nspec, 0, 0, 0 /)                         )
  !         DO jspec=1,nspec
  !           IF (ldiagdetail(jsec) .AND. emimod(jsec, jspec) /= EM_NONE) THEN
  !             cdiagname = 'emi_'//TRIM(speclist(jspec)%shortname)//'_'//TRIM(tolower(secname))
  !             cdiaglongname = 'accumulated emission mass flux of '//  &
  !               TRIM(speclist(jspec)%shortname) //' due to '//TRIM(tolower(secname))
  !             cunit = 'kg m-2 s-1'
  !             CALL new_diag_element( emi_field%emi_diag_detail(jsec),  cdiagname,     &
  !                                 & BYSPECIES, jspec,                                 &
  !                                 & longname=cdiaglongname,                           &
  !                                 & units=cunit, shape2d=shape2d, shape3d=shape3d     )
  !           END IF
  !         END DO
  !       END IF
  !     END DO

  !   END IF

  ! END SUBROUTINE new_hammoz_emission_list

!---------------------------------------------------------------------------------------------------

  SUBROUTINE get_emi_field( jg, jb, jcs, jce,                                         &
                            kblks_c, kproma, klev, klevp1,                            &
                            datetime,                                                 &                                   
                            ibc_emis, emimod,                                         & 
                            pmair, paphm1, pzf, pzh, pdz, lfirst, lo3d, pbc2d, pbc3d  )

    USE mo_exception_lite,   ONLY: finish, message, message_text
    USE mtime_lite,          ONLY: t_datetime => datetime
    USE mo_subm_bcs_lite,    ONLY: bc_apply, bc_modify, bc_query, &
                              BC_VERTICAL_WEIGHTED_INTERPOLATION 

    IMPLICIT NONE

    INTEGER, INTENT(IN) ::  jg,         &   ! grid index
                            jb,         &   ! index of block loop
                            jcs, jce,   &   ! start and end indices of columns loop
                            kblks_c,    &   ! number of geographic blocks
                            kproma,     &   ! maximum number of columns  
                            klev,       &   ! number of full levels
                            klevp1,     &   ! number of half levels
                            ibc_emis,   &   ! index of boundary_condition to store to emission field
                            emimod          ! mode of emission 
    REAL(dp), INTENT(IN) :: pmair(kproma,klev),     & ! air mass [kg/m2]
                            paphm1(kproma,klevp1),  & ! half level pressure [Pa]
                            pzf(kproma,klev),       & ! geometric height above sea level, full level
                            pzh(kproma,klevp1),     & ! geometric height above sea level, half level
                            pdz(kproma,klev)          ! geometric height thickness of layer [m]
    LOGICAL,  INTENT(IN)  :: lfirst                   ! flag to indicate whether it is the first timestep 
    TYPE(t_datetime), INTENT(IN), POINTER :: datetime   ! the current date 
    REAL(dp), INTENT(INOUT) ::  pbc2d(kproma),  &   ! for emission field
                                pbc3d(kproma,klev)  ! for emission field
    LOGICAL,  INTENT(INOUT) :: lo3d ! flag to indicate whether the emissions are applied in 3D

    INTEGER :: jk, jl
    CHARACTER(LEN=30)   :: strunit
    CHARACTER(LEN=128)  :: bc_name
    
    CALL bc_query(jg, ibc_emis, name=bc_name, ef_actual_unit=strunit)


    SELECT CASE (emimod)

      CASE (EM_SURFACE)
        IF ((index(strunit,'/m2') > 0) .OR. (index(strunit,'m-2') > 0) &
            .OR. (index(strunit,'/m**2') > 0) .OR. (index(strunit,'m**-2') > 0)) THEN
          CALL bc_apply (jg, jb, jcs, jce, ibc_emis, kblks_c, datetime, pbc2d)
          ! don't convert unit [kg m-2 s-1] --> it is needed like this
          lo3d = .FALSE.
        ELSE
          WRITE(message_text,'(3a)') 'Wrong unit for ',TRIM(bc_name), ' of type EM_SURFACE: '//strunit
          CALL finish('get_emi_field', message_text)
        ENDIF
      CASE (EM_VOLUME)
        IF ((index(strunit,'/m3') > 0) .OR. (index(strunit,'m-3') > 0) &
            .OR. (index(strunit,'/m**3') > 0) .OR. (index(strunit,'m**-3') > 0)) THEN
          IF (lfirst) THEN
            CALL bc_modify(jg, ibc_emis, bc_vertint=BC_VERTICAL_WEIGHTED_INTERPOLATION)
          ENDIF
          CALL bc_apply (jg, jb, jcs, jce, ibc_emis, kblks_c, datetime, paphm1, pzf, pzh, pbc3d)
          ! convert to [kg kg-1 s-1]
          DO jk = 1,klev
            DO jl = jcs,jce
              pbc3d(jl,jk) = pbc3d(jl,jk) * pdz(jl,jk) / pmair(jl,jk)    
            END DO
          END DO
        ELSEIF ((index(strunit,'/m2') > 0) .OR. (index(strunit,'m-2') > 0) &
            .OR. (index(strunit,'/m**2') > 0) .OR. (index(strunit,'m**-2') > 0)) THEN
          CALL bc_apply (jg, jb, jcs, jce, ibc_emis, kblks_c, datetime, paphm1, pzf, pzh, pbc3d)
          ! convert to [kg kg-1 s-1]
          DO jk = 1,klev
            DO jl = jcs,jce
              pbc3d(jl,jk) = pbc3d(jl,jk) / pmair(jl,jk)  
            END DO
          END DO
        ELSE
          WRITE(message_text,'(2a)') 'Wrong unit for emissions of type EM_VOLUME: '//strunit
          CALL finish('get_emi_field', message_text)
        ENDIF
      CASE (EM_LEVEL50M)
        IF ((index(strunit,'/m2') > 0) .OR. (index(strunit,'m-2') > 0) &
            .OR. (index(strunit,'/m**2') > 0) .OR. (index(strunit,'m**-2') > 0)) THEN
          CALL bc_apply (jg, jb, jcs, jce, ibc_emis, kblks_c, datetime, pbc2d)
          ! put emissions in second lowest model level (eehol: boxmodel has only 1 level)
          ! DO jl = jcs,jce
          !   pbc3d(jl,klev-1) = pbc2d(jl)
          ! END DO
          ! convert to [kg kg-1 s-1]
          DO jk = 1,klev
            DO jl = jcs,jce
              pbc3d(jl,jk) = pbc3d(jl,jk) / pmair(jl,jk)  
            END DO 
          END DO 
          DO jl = jcs,jce 
            pbc2d(jl) = 0._dp
          END DO 
        ELSE
          WRITE(message_text,'(2a)') 'Wrong unit for emissions of type EM_SURFACE: '//strunit
          CALL finish('get_emi_field', message_text)
        ENDIF
      CASE (EM_FIRE)
        IF ((index(strunit,'/m2') > 0) .OR. (index(strunit,'m-2') > 0) &
            .OR. (index(strunit,'/m**2') > 0) .OR. (index(strunit,'m**-2') > 0)) THEN
          CALL bc_apply (jg, jb, jcs, jce, ibc_emis, kblks_c, datetime, pbc2d) 
          ! put emissions in lowest model level 
          DO jl = jcs,jce
            pbc3d(jl,klev) = pbc2d(jl)
            pbc2d(jl) = 0._dp
          END DO 
          ! convert to [kg kg-1 s-1]
          DO jk = 1,klev
            DO jl = jcs,jce
              pbc3d(jl,jk) = pbc3d(jl,jk) / pmair(jl,jk) 
            END DO
          END DO
        ELSE
          WRITE(message_text,'(2a)') 'Wrong unit for emissions of type EM_SURFACE: '//strunit
          CALL finish('get_emi_field', message_text)
        ENDIF

    END SELECT

    IF ((jb == kproma) .AND. (ibc_emis == 2)) CALL message('', 'work on get_emi_field - mo_hammoz_emi')

  END SUBROUTINE get_emi_field

!---------------------------------------------------------------------------------------------------

  ! read emissions outside omp region 

  SUBROUTINE emi_read_bc_interface( p_patch, datetime, pareacella, pclon, pclat, pzh )

    USE mtime_lite,                  ONLY: t_datetime => datetime
    USE mo_model_domain_lite,        ONLY: t_patch
    USE mo_impl_constants_lite,      ONLY: max_dom
    !USE mo_util_timeconv,       ONLY: TC_get 
    USE mo_subm_bcs_lite,            ONLY: bc_list_read, bc_lists, nbc
    USE mo_ham_lite_main,            ONLY: idsec_dust
    USE mo_ham_lite_dust,            ONLY: bgc_dust_read_monthly, bgc_dust_set_nduscale
    !USE mo_hammoz_emi_vol,      ONLY: read_emi_volcano

    IMPLICIT NONE

    REAL(dp), INTENT(IN) :: pareacella(:,:),  & ! grid box area (nproma, nblk)
                            pclat(:,:),       & ! grid cell center latitide (nproma, nb)
                            pclon(:,:),       & ! grid cell center longtude (nproma, nblk)
                            pzh(:,:,:)          ! geometric height of half levels (nproma, klevp1, nblk)
    TYPE(t_datetime), POINTER, INTENT(IN) :: datetime
    TYPE(t_patch),  INTENT(IN) :: p_patch 
 
    INTEGER :: jg, jbc    
    INTEGER :: month_of_previous_timestep, month
    LOGICAL, SAVE, DIMENSION(max_dom) :: lfirst = .TRUE.
 
    jg = p_patch%id 

    CALL bc_list_read( jg, p_patch, datetime )

    !CALL TC_get( datetime,  month=month )
    !CALL TC_get( datetime,  month=month_of_previous_timestep, lprevious=.TRUE.)

    !IF ( month /= month_of_previous_timestep .OR. lfirst(jg) ) THEN
    IF ( lfirst(jg) ) THEN
      IF ( lfirst(jg)  .AND. idsec_dust > 0 ) THEN  
        CALL bgc_dust_set_nduscale(jg, p_patch, datetime)
      END IF
      ! dust emissions  
      IF (idsec_dust > 0) THEN
        !CALL bgc_dust_read_monthly(jg, p_patch, month)
        CALL bgc_dust_read_monthly(jg, p_patch, 1)
      END IF
    END IF

    ! volcanic emissions  
    ! IF  ( lfirst(jg) ) THEN
    !   ! continuous  
    !   IF ( lhas_volcc ) THEN
    !     CALL read_emi_volcano( jg, p_patch, pareacella, pclon, pclat, pzh, 1, lprint_info=.TRUE. )
    !   END IF
    !   ! explosive 
    !   IF ( lhas_volce ) THEN
    !     CALL read_emi_volcano( jg, p_patch, pareacella, pclon, pclat, pzh, 2 )
    !   END IF
    ! END IF
    
    DO jbc = 1,nbc 
      IF (bc_lists(jg)%bc_list(jbc)%bc_ndim == 2) THEN 
        !$ACC UPDATE DEVICE(bc_lists(jg)%bc_list(jbc)%bc_values_pointer%p_2d)
        !$ACC UPDATE DEVICE(bc_lists(jg)%bc_list(jbc)%bc_values_pointer_prior%p_2d)
        !$ACC UPDATE DEVICE(bc_lists(jg)%bc_list(jbc)%bc_values_pointer_later%p_2d)
      ELSE 
        !$ACC UPDATE DEVICE(bc_lists(jg)%bc_list(jbc)%bc_values_pointer%p_3d)
        !$ACC UPDATE DEVICE(bc_lists(jg)%bc_list(jbc)%bc_values_pointer_prior%p_3d)
        !$ACC UPDATE DEVICE(bc_lists(jg)%bc_list(jbc)%bc_values_pointer_later%p_3d)
      END IF
    END DO

    lfirst(jg) = .FALSE.

  END SUBROUTINE emi_read_bc_interface

!---------------------------------------------------------------------------------------------------

  SUBROUTINE emi_lite_interface(  jg, jb, jcs, jce,                       &
                                  kblks_c, kproma, ktrac, klev, klevp1,   &
                                  ksfc_type, idx_wtr, idx_ice, idx_lnd,   &
                                  datetime,                               &
                                  pzf, pzh, pdz, pclon, pclat,            & 
                                  plsmask, pglac, palake, pseaice,        &
                                  paphm1, ptm1, pmair, psfcWind,          &
                                  pz0m_tile, pustar_tile, ptsfc_tile,     & 
                                  pxtte, pxt_emis                         ) 
 
    USE mo_mpi_lite,                 ONLY: p_pe, p_io
    USE mtime_lite,                  ONLY: t_datetime => datetime 
    USE mo_exception_lite,           ONLY: message  
    USE mo_subm_spec_lite,           ONLY: nspec, nmaxtrspec 
    USE mo_subm_trac_lite,           ONLY: ntrac 
    !USE mo_subm_diag,           ONLY: get_diag_pointer
    USE mo_ham_lite_main,            ONLY: sizeclass 
    USE mo_ham_lite_misc,            ONLY: lcheck_emi, ham_lite_check_r1d, ham_lite_check_wcc
    USE mo_ham_lite_spec,            ONLY: id_so2, id_so4
    USE mo_ham_lite_emi,             ONLY: ham_lite_emi  

    INTEGER, INTENT(IN) ::  jg,         &   ! grid index
                            jb,         &   ! index of block loop
                            jcs, jce,   &   ! start and end indices of columns loop
                            kblks_c,    &   ! number of geographic blocks
                            kproma,     &   ! maximum number of columns
                            ktrac,      &   ! number of tracers
                            klev,       &   ! number of full levels
                            klevp1,     &   ! number of half levels (klev + 1) 
                            ksfc_type,  &   ! number of surface types 
                            idx_wtr,    &   ! index for water
                            idx_ice,    &   ! index for ice  
                            idx_lnd         ! index for land 
    REAL(dp), INTENT(IN) :: pzf         (kproma,klev),      & ! geometric height above sea level, full level   
                            pzh         (kproma,klevp1),    & ! geometric height above sea level, half level
                            pdz         (kproma,klev),      & ! geometric height thickness of layer [m]   
                            pclon       (kproma),           & ! longitude
                            pclat       (kproma),           & ! latitude      
                            plsmask     (kproma),           & ! land-sea mask (1. = land, 0. = sea/lakes)     
                            pglac       (kproma),           & ! fraction of land covered by glaciers
                            palake      (kproma),           & ! lake fraction 
                            pseaice     (kproma),           & ! fraction of ocean covered by sea ice
                            paphm1      (kproma,klevp1),    & ! half level pressure 
                            ptm1        (kproma,klev),      & ! air temperature
                            pmair       (kproma,klev),      & ! air mass [kg/m2]
                            psfcWind    (kproma),           & ! 10 m wind speed
                            pz0m_tile   (kproma,ksfc_type), & ! surface roughness length
                            pustar_tile (kproma,ksfc_type), & ! friction velocity 
                            ptsfc_tile  (kproma,ksfc_type)     ! surface temperature 
              
    TYPE(t_datetime), INTENT(IN), POINTER :: datetime 
    REAL(dp), INTENT(INOUT) :: pxtte(kproma,klev,ktrac) ! tracer tendencies
    REAL(dp), INTENT(INOUT) :: pxt_emis(kproma,ktrac)   ! surface emissions

    INTEGER           :: ispec, ierr, jk, jl, jsect, jspec, jtrac  
    INTEGER           :: ibc_extra(nmaxtrspec) ! flag for aerosol modes  
    LOGICAL           :: lo3d
    LOGICAL, SAVE     :: lfirst = .TRUE. 
    REAL(dp)          :: zbc2d(kproma), zbc3d(kproma,klev)  ! for boundary conditions
    REAL(dp), POINTER :: fld2d(:,:)                         ! for diagnostics
    REAL(dp)          :: zfactor(nmaxtrspec)                ! weighting factor for aerosol modes
    REAL(dp)          :: zemidiag(kproma)                   ! for diagnostics
    REAL(dp)          :: zxtems3d(kproma,klev)
  
    ! get diagnostics
    ! DO jsect = 1,nsectors  
    !   DO jtrac = 1,ntrac 

    !     CALL get_diag_pointer(emi_field(jg)%emi_diag, fld2d, jtrac, ierr=ierr)   
    !     IF (ierr == 0) THEN 
    !       DO jl = jcs,jce
    !         fld2d(jl,jb) = 0._dp
    !       END DO
    !     END IF
    !     CALL get_diag_pointer(emi_field(jg)%emi_diag_detail(jsect), fld2d, jtrac, ierr=ierr)
    !     IF (ierr == 0) THEN 
    !       DO jl = jcs,jce
    !         fld2d(jl,jb) = 0._dp 
    !       END DO
    !     END IF
    !   END DO
    ! END DO 

    ! distribute emissions to tracers
    DO jsect = 1,nsectors
  
      ! obtain 2D or 3D emission fluxes and distribute vertically where needed
      DO jspec = 1,nspec

        IF (emimod(jsect,jspec) == EM_NONE) CYCLE ! do nothing
 
        CALL ham_lite_emi(  jg, jb, jcs, jce,                           &
                            kblks_c, kproma, klev, ktrac, jsect, jspec, & 
                            ksfc_type, idx_wtr, idx_ice, idx_lnd,       & 
                            datetime,                                   &
                            plsmask, pglac, palake, pseaice, psfcWind,  &
                            pz0m_tile, pustar_tile, ptsfc_tile,         &
                            ibc_extra, zfactor                          )

        DO jl = jcs,jce
          zbc2d(jl) = 0._dp
        END DO
        DO jk = 1,klev
          DO jl = jcs,jce
            zbc3d(jl,jk) = 0._dp
          END DO
        END DO
        lo3d = .TRUE. ! process as 3D emissions 

        ispec = jspec
        IF (ispec == id_so4) ispec = id_so2 

        CALL get_emi_field( jg, jb, jcs, jce,                                         &
                            kblks_c, kproma, klev, klevp1,                            &
                            datetime,                                                 &
                            ibc_emis(jsect,ispec), emimod(jsect,ispec),               & 
                            pmair, paphm1, pzf, pzh, pdz, lfirst, lo3d, zbc2d, zbc3d  )

        ! we now have the total species emissions in either zbc2d or zbc3d
        ! next we need to distribute them among all tracers belonging to this species 
        ! effectively this means copying zbc2d in pxt_emis and zbc3d in zxtems3d 
        ! zbc2d unit: [#/m2/s], zbc3d unit: [#/kg/s]
  
        DO jtrac = 1,ntrac
          IF (ibc_extra(jtrac) /= 0) THEN 
            CALL get_emi_field( jg, jb, jcs, jce,                                         &
                                kblks_c, kproma, klev, klevp1,                            &
                                datetime,                                                 & 
                                ibc_extra(jtrac), emimod(jsect,ispec),                    & 
                                pmair, paphm1, pzf, pzh, pdz, lfirst, lo3d, zbc2d, zbc3d  )
          END IF

          IF (zfactor(jtrac) > 0._dp) THEN 
            DO jl = jcs,jce
              zemidiag(jl) = zfactor(jtrac) * zbc2d(jl) * emfactor(jsect,jspec)
              pxt_emis(jl,jtrac) = pxt_emis(jl,jtrac) + zemidiag(jl)
            END DO 
            IF (lo3d) THEN
              DO jk = 1,klev
                DO jl = jcs,jce
                  zxtems3d(jl,jk) = zfactor(jtrac) * zbc3d(jl,jk) * emfactor(jsect,jspec)
                  pxtte(jl,jk,jtrac) = pxtte(jl,jk,jtrac) + zxtems3d(jl,jk)
                END DO 
              END DO
              DO jk = 1,klev
                DO jl = jcs,jce
                  zemidiag(jl) = zemidiag(jl) + zxtems3d(jl,jk) * pmair(jl,jk)
                END DO
              ENDDO
            END IF
            
            ! get diagnostics
            ! CALL get_diag_pointer(emi_field(jg)%emi_diag, fld2d, jtrac, ierr=ierr)
            ! IF (ierr == 0) THEN 
            !   DO jl = jcs,jce
            !     fld2d(jl,jb) = fld2d(jl,jb) + zemidiag(jl) 
            !   END DO
            ! END IF
            ! CALL get_diag_pointer(emi_field(jg)%emi_diag_detail(jsect), fld2d, jtrac, ierr=ierr)
            ! IF (ierr == 0) THEN 
            !   DO jl = jcs,jce
            !     fld2d(jl,jb) = fld2d(jl,jb) + zemidiag(jl)
            !   END DO
            ! END IF
          END IF

        END DO  ! tracer loop 
        
      END DO  ! species loop 

    END DO  ! sector loop

    lfirst = .FALSE.

    ! print checks 
    IF (lcheck_emi) THEN  
      DO jtrac = 1,ntrac
        CALL ham_lite_check_wcc( jb, sizeclass(jtrac)%shortname, 'mode' )  
        CALL ham_lite_check_r1d( jb, jcs, jce, kproma, pxt_emis(:,jtrac), 'emi_lite_interface', 'pxt_emis' )
      END DO 
    END IF

  END SUBROUTINE emi_lite_interface

!---------------------------------------------------------------------------------------------------

  SUBROUTINE emi_lite_number( jg, jb, jcs, jce,                     &
                              kblks_c, kproma, klev, klevp1,        & 
                              datetime, pareacella, pzf, pzh, pdz,  &
                              pmair, paphm1, pnumb                  ) 

    USE mo_mpi_lite,                 ONLY: p_pe, p_io
    USE mtime_lite,                  ONLY: t_datetime => datetime  
    USE mo_subm_spec_lite,           ONLY: nmaxtrspec, nspec  
    USE mo_subm_trac_lite,           ONLY: ntrac 
    USE mo_ham_lite_main,            ONLY: sizeclass     
    USE mo_ham_lite_spec,            ONLY: id_so2, id_so4, id_bc, id_oc
    USE mo_ham_lite_emi,             ONLY: ham_lite_get_factor 

    INTEGER, INTENT(IN) ::  jg,       & ! grid index
                            jb,       & ! index of block loop
                            jcs, jce, & ! geographic block number of locations
                            kblks_c,  & ! number of geographic blocks
                            kproma,   & ! maximum number of columns
                            klev,     & ! number of levels
                            klevp1      ! number of levels + 1  
    REAL(dp), INTENT(IN) :: pareacella  (kproma),         & ! grid box area
                            pzf         (kproma,klev),    & ! geometric height above sea level, full level   
                            pzh         (kproma,klevp1),  & ! geometric height above sea level, half level
                            pdz         (kproma,klev),    & ! geometric height thickness of layer    
                            pmair       (kproma,klev),    & ! air mass [kg/m2] 
                            paphm1      (kproma,klevp1)     ! half level pressure  
    TYPE(t_datetime), INTENT(IN), POINTER :: datetime 
    REAL(dp), INTENT(INOUT) :: pnumb(nspec,ntrac)
 
    INTEGER   :: ispec, jk, jl, jsect, jspec, jtrac   
    REAL(dp)  :: zbc2d(kproma), zbc3d(kproma,klev)  ! for boundary conditions 
    REAL(dp)  :: zfactor(nmaxtrspec)                ! weighting factor for aerosol modes
    REAL(dp)  :: zemidiag(kproma)                   ! 2D field for diagnostics 
    REAL(dp)  :: zxtems3d(kproma,klev)
    LOGICAL   :: lo3d
    LOGICAL   :: lfirst 
 
    lfirst = .TRUE.

    ! distribute emissions to tracers
    DO jsect = 1,nsectors 
      DO jspec = 1,nspec
        
        IF (emimod(jsect,jspec) == EM_NONE) CYCLE ! do nothing
 
        CALL ham_lite_get_factor( jsect, jspec, zfactor ) 

        DO jl = jcs,jce
          zbc2d(jl) = 0._dp
        END DO
        DO jk = 1,klev
          DO jl = jcs,jce
            zbc3d(jl,jk) = 0._dp
          END DO
        END DO
        lo3d = .TRUE. ! process as 3D emissions 

        ispec = jspec
        IF (ispec == id_so4) ispec = id_so2 

        IF ((ispec == id_so2) .OR. (ispec == id_oc) .OR. (ispec == id_bc)) THEN 
          
          CALL get_emi_field( jg, jb, jcs, jce,                                         &
                              kblks_c, kproma, klev, klevp1,                            &
                              datetime,                                                 &
                              ibc_emis(jsect,ispec), emimod(jsect,ispec),               & 
                              pmair, paphm1, pzf, pzh, pdz, lfirst, lo3d, zbc2d, zbc3d  )

          ! we now have the total species emissions in either zbc2d or zbc3d
          ! next we need to distribute them among all tracers belonging to this species 
          ! effectively this means copying zbc2d in pxtems and zbc3d in zxtems3d  
          ! zbc2d unit: [#/m2/s], zbc3d unit: [#/kg/s]

          DO jtrac = 1,ntrac

            IF (zfactor(jtrac) > 0._dp) THEN 
              DO jl = jcs,jce
                zemidiag(jl) = zfactor(jtrac) * zbc2d(jl) * emfactor(jsect,jspec) 
              END DO
              IF (lo3d) THEN
                DO jk = 1,klev
                  DO jl = jcs,jce
                    zxtems3d(jl,jk) = zfactor(jtrac) * zbc3d(jl,jk) * emfactor(jsect,jspec) 
                  END DO 
                END DO
                DO jk = 1,klev
                  DO jl = jcs,jce
                    zemidiag(jl) = zemidiag(jl) + zxtems3d(jl,jk) * pmair(jl,jk)
                  END DO 
                END DO
              END IF 
          
              DO jl = jcs,jce
                pnumb(jspec,jtrac) = pnumb(jspec,jtrac) + zemidiag(jl) * pareacella(jl) 
              END DO
            END IF
            
          END DO  ! tracer loop 
          
        END IF

      END DO  ! species loop 
    END DO  ! sector loop

    lfirst = .FALSE.

  END SUBROUTINE emi_lite_number

END MODULE mo_hammoz_lite_emi