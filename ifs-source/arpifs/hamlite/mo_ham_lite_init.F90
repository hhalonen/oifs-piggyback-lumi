! 
! based on mo_ham_init.f90  
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

MODULE mo_ham_lite_init

  USE mo_kind_lite,          ONLY: dp
  USE mo_exception_lite,     ONLY: finish, message, message_text, print_value 
  USE mo_util_string_lite,   ONLY: separator 


  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: ham_lite_start,       &
            ham_lite_tracer,      &
            ham_lite_init,        &
!            ham_lite_init_memory, &
            ham_lite_init_modes,  &
            ham_lite_print_modes

  !--------------------
  ! module parameters
  !--------------------

  REAL(dp), PARAMETER :: zeps = EPSILON(1.0_dp)
 
  ! definition of carbonaceous and sulfuric mode

  ! number of sectors  
  INTEGER, PARAMETER :: &
    nqca_pd = 4, nqsu_pd = 9, & ! present-day 
    nqca_pi = 2, nqsu_pi = 2    ! pre-industrial 

  ! composition (bc, oc, su)
  REAL(dp), PARAMETER ::  &  ! present-day (gfas, ceds, and volcanoes)
    vqca_pd(3) = (/ 0.2463915E-01_dp, 0.9320115E+00_dp, 0.4334940E-01_dp /),  &
    vqsu_pd(3) = (/ 0.2735855E-01_dp, 0.8516174E-01_dp, 0.8874797E+00_dp /)

  REAL(dp), PARAMETER ::  & ! pre-industrial (gfas and volcanoes)
    vqca_pi(3) = (/ 0.2199481E-01_dp, 0.9379087E+00_dp, 0.4009650E-01_dp /),  &
    vqsu_pi(3) = (/ 0._dp, 0._dp, 0.1000000E+01_dp /)  

  ! sectors 
  CHARACTER(LEN=64), PARAMETER :: & ! present-day (gfas, ceds, and volcanoes)
    sqca_pd(nqca_pd) = (/ CHARACTER(len=10):: 'AGR', 'BIOGENIC', 'WFIRE', 'WST' /), &
    sqsu_pd(nqsu_pd) = (/ CHARACTER(len=10):: 'AIRC', 'DOM', 'ENE', 'IND', 'SHIPS', 'SLV', 'TRA', 'VOLCE', 'VOLCC' /)  

  CHARACTER(LEN=64), PARAMETER :: & ! pre-industrial (gfas and volcanoes)
    sqca_pi(nqca_pi) = (/ CHARACTER(len=10):: 'BIOGENIC', 'WFIRE' /), &
    sqsu_pi(nqsu_pi) = (/ CHARACTER(len=10):: 'VOLCE', 'VOLCC' /)


CONTAINS

!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_start ( p_patch )

    USE mo_model_domain_lite,      ONLY: t_patch  
    USE mo_ham_lite_main,          ONLY: set_ham_lite
    USE mo_ham_lite_dust,          ONLY: set_dust
    USE mo_ham_lite_spec,          ONLY: ham_lite_species 

    IMPLICIT NONE

    TYPE(t_patch),INTENT(IN), DIMENSION(:)  :: p_patch
   
    CALL set_ham_lite ( p_patch )   
    CALL set_dust     ( p_patch ) 

    CALL ham_lite_species   
    CALL ham_lite_modes   

  END SUBROUTINE ham_lite_start

!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_tracer
 
    ! uses local grib table 131 (tracer)  
    ! with grib codes 201 to 255 for aerosols

    USE mo_mpi_lite,               ONLY: p_pe, p_io  
    USE mo_subm_trac_lite,         ONLY: new_tracer, new_diag_burden 
    USE mo_subm_trac_para_lite,    ONLY: OK, ON, OFF, AEROSOLNUMBER  
    USE mo_ham_lite_main,          ONLY: ndrydep, nwetdep, sizeclass, nclass

    IMPLICIT NONE

    INTEGER :: jmode, igribc, idt, ierr
    INTEGER :: iburdenid, idrydep, iwetdep, isedi
 
    igribc = 201

    ! define tracers for aerosol numbers

    CALL message('','')
    CALL message('',separator)

    DO jmode = 1,nclass
    
      IF (sizeclass(jmode)%lsedi) THEN
        isedi = ON
      ELSE
        isedi = OFF
      END IF

      idrydep = ndrydep  
      iwetdep = nwetdep  
      
      iburdenid = new_diag_burden('num_'//sizeclass(jmode)%shortname, itype=1, lclobber=.false.)
 
      CALL new_tracer('num', ierr=ierr,                                   &
                      subname=sizeclass(jmode)%shortname,                 &
                      units='1 kg-1',                                     &
                      table=131,                                          &
                      code=igribc,                                        & 
                      burdenid=iburdenid,                                 &
                      nphase=AEROSOLNUMBER,                               &
                      mode=jmode,                                         &
                      ndrydep=idrydep,                                    &
                      nwetdep=iwetdep,                                    & 
                      nsedi=isedi,                                        &  
                      longname='number mixing ratio - aerosol mode ' //   &
                        TRIM(ADJUSTL(sizeclass(jmode)%longname)),         &
                      idx=idt                                             )

      IF (ierr == OK) THEN
        sizeclass(jmode)%idt_no = idt
      ELSE
        WRITE(message_text,'(a,i0)') 'new_tracer ' // &
          TRIM(sizeclass(jmode)%shortname) // ' returned error code ' , ierr
        CALL message('ham_define_tracer', message_text)
      END IF
    
      igribc = igribc + 1

    END DO

  END SUBROUTINE ham_lite_tracer

!---------------------------------------------------------------------------------------------------
  
  SUBROUTINE ham_lite_modes 
 
    USE mo_ham_lite_main,     ONLY: npath, rqdu, rqss, rqca, rqsu
    USE mo_ham_lite_spec,     ONLY: id_so4, id_bc, id_oc, id_ss, id_du 

    IMPLICIT NONE 

    INTEGER :: nqca, nqsu
    REAL(dp) :: vcad(3), vsud(3)
    CHARACTER(LEN=64) :: sqca(20), sqsu(20)

    IF (npath == 1) THEN 
      nqca = nqca_pd
      nqsu = nqsu_pd
      vcad(:) = vqca_pd(:)
      vsud(:) = vqsu_pd(:)
      sqca(1:nqca) = sqca_pd(1:nqca)
      sqsu(1:nqsu) = sqsu_pd(1:nqsu)
    ELSE IF (npath == 2) THEN 
      nqca = nqca_pi
      nqsu = nqsu_pi
      vcad(:) = vqca_pi(:)
      vsud(:) = vqsu_pi(:)
      sqca(1:nqca) = sqca_pi(1:nqca)
      sqsu(1:nqsu) = sqsu_pi(1:nqsu)
    END IF

    ! initialize modes

    CALL init_new_mode( imode       = 1,                            &
                        longname    = "dust",                       &
                        shortname   = "qdu",                        & 
                        lsoluble    = .TRUE.,                       &
                        lsedi       = .TRUE.,                       &
                        lacti       = .TRUE.,                       &
                        radius      = rqdu,                         &
                        msize       = 1,                            &
                        icomp       = (/ id_du /),                  & 
                        vcomp       = (/ 1.0_dp /),                 & 
                        esize       = 1,                            &
                        ename       = (/ 'DUST' /)                  )

    CALL init_new_mode( imode       = 2,                            &
                        longname    = "sea salt",                   & 
                        shortname   = "qss",                        & 
                        lsoluble    = .TRUE.,                       &
                        lsedi       = .TRUE. ,                      &
                        lacti       = .TRUE.,                       &
                        radius      = rqss,                         &
                        msize       = 1,                            &
                        icomp       = (/ id_ss  /),                 & 
                        vcomp       = (/ 1.0_dp /),                 & 
                        esize       = 1,                            &
                        ename       = (/ 'SEASALT' /)               )

    CALL init_new_mode( imode       = 3,                            &
                        longname    = "carbonaceous",               &  
                        shortname   = "qca",                        & 
                        lsoluble    = .TRUE.,                       &
                        lsedi       = .TRUE. ,                      &
                        lacti       = .TRUE.,                       &
                        radius      = rqca,                         &
                        msize       = 3,                            & 
                        icomp       = (/ id_bc, id_oc, id_so4 /),   &  
                        vcomp       = vcad,                         & 
                        esize       = nqca,                         &
                        ename       = sqca(1:nqca)                  ) 

    CALL init_new_mode( imode       = 4,                            &
                        longname    = "sulfuric",                   & 
                        shortname   = "qsu",                        & 
                        lsoluble    = .TRUE.,                       &
                        lsedi       = .TRUE. ,                      &
                        lacti       = .TRUE.,                       &
                        radius      = rqsu,                         &
                        msize       = 3,                            &
                        icomp       = (/ id_bc, id_oc, id_so4 /),   &  
                        vcomp       = vsud,                         & 
                        esize       = nqsu,                         & 
                        ename       = sqsu(1:nqsu)                  ) 
 
  END SUBROUTINE ham_lite_modes 

!---------------------------------------------------------------------------------------------------
  
  SUBROUTINE ham_lite_init
 
    USE mo_ham_lite_kappa,    ONLY: start_kappa, end_kappa
    USE mo_ham_lite_rad,      ONLY: ham_lite_rad_init, ham_lite_rad_calc 
    USE mo_ham_lite_seasalt,  ONLY: start_emi_seasalt  

    IMPLICIT NONE
      
    ! initialize radiation scheme  
    CALL start_kappa 
    CALL ham_lite_kappa
    CALL end_kappa
    CALL ham_lite_rad_init
    CALL ham_lite_rad_calc 

    ! initialize seasalt emissions
    CALL start_emi_seasalt

  END SUBROUTINE ham_lite_init

!---------------------------------------------------------------------------------------------------
  
  SUBROUTINE ham_lite_kappa

    ! calculate hygroscopicity parameter and wet radius
  
     
    USE mo_ham_lite_main,     ONLY: nclass, sizeclass 
    USE mo_ham_lite_misc,     ONLY: lite_rwet 
    USE mo_ham_lite_kappa

    IMPLICIT NONE

    INTEGER   :: ix_rd, ix_tm, ix_rh, ix_kp, jmode, ispec, jspec, nspec 
    REAL(dp)  :: var, zk_steps, zr_steps 
 
    IF (.NOT. ALLOCATED(lite_rwet)) ALLOCATE(lite_rwet(N_T,N_rh,nclass)) 

    zk_steps  = REAL((N_kappa-1),dp)  ! number of steps on kappa axis
    zr_steps  = REAL((N_Rd-1),dp)     ! number of steps on radius axis
 
    CALL message('','')
    CALL message('',separator)
    CALL message('lite init', 'print dry and wet radii of modes')
    CALL message('','---')

    ! compute wet radius as function of temperature and relative humidity
    DO jmode = 1,nclass 

      ! limit kappa to [kappa_min,kappa_max]
      var = MAX(sizeclass(jmode)%lite_kappa, kappa_min)     
      var = MIN(var, kappa_max)  
      ! get entry in look up table  
      ix_kp = 1 + NINT(zk_steps * (var - kappa_min) / (kappa_max - kappa_min))

      ! limit log(rd) to [ln_rd_min,ln_rd_max]  
      var = MAX(LOG(sizeclass(jmode)%lite_radi), ln_Rd_min)          
      var = MIN(var, ln_Rd_max)  
      ! get entry in look up table         
      ix_rd = 1 + NINT(zr_steps * (var - ln_Rd_min) / (ln_Rd_max - ln_Rd_min)) 

      DO ix_rh = 1,N_rh
        DO ix_tm = 1,N_T 

          ! wet radius = dry radius * growth factor
          lite_rwet(ix_tm, ix_rh, jmode) = sizeclass(jmode)%lite_radi * gf(ix_rd, ix_tm, ix_rh, ix_kp)  
            
        END DO
      END DO

      WRITE(message_text,'(a,5x,a)') 'lite mode:', sizeclass(jmode)%longname
      CALL message('',message_text) 
      WRITE(message_text,'(a)') 'dry radius [m]:' 
      CALL message('',message_text)
      WRITE(message_text,'(e10.3)') sizeclass(jmode)%lite_radi 
      CALL message('',message_text)
      WRITE(message_text,'(a,5x,e10.3)') 'wet radius [m]:'
      CALL message('',message_text)
      WRITE(message_text,'(e10.3,3x,a)') lite_rwet(1,1,jmode), '(1,1)' 
      CALL message('',message_text) 
      WRITE(message_text,'(e10.3,3x,a)') lite_rwet(1,N_rh,jmode), '(1,N_RH)' 
      CALL message('',message_text) 
      WRITE(message_text,'(e10.3,3x,a)') lite_rwet(N_T,1,jmode), '(N_T,1)'
      CALL message('',message_text) 
      WRITE(message_text,'(e10.3,3x,a)') lite_rwet(N_T,N_rh,jmode), '(N_T,N_RH)' 
      CALL message('',message_text) 
      CALL message('','')

    END DO     

  END SUBROUTINE ham_lite_kappa

!---------------------------------------------------------------------------------------------------

  ! SUBROUTINE ham_lite_init_memory ( p_patch ) 

  !   USE mo_model_domain,        ONLY: t_patch  
  !   USE mo_ham_lite_main,       ONLY: nclass, nrad 
  !   USE mo_ham_lite_streams,    ONLY: new_stream_ham, new_stream_ham_rad   

  !   IMPLICIT NONE

  !   TYPE(t_patch),INTENT(IN), DIMENSION(:)  :: p_patch

  !   CALL new_stream_ham ( p_patch, nclass )     

  !   IF (ANY(nrad(:) > 0)) CALL new_stream_ham_rad ( p_patch )   

  ! END SUBROUTINE ham_lite_init_memory

!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_init_modes( jg, p_patch, pareacella, pclon, pclat,  &
                                  pzf, pzh, pdz, pmair, paphm1, cur_time            )
 
    USE mo_math_constants_lite,      ONLY: pi 
    !USE mo_io_units,            ONLY: find_next_free_unit
    USE mo_mpi_lite,                 ONLY: p_pe, p_io  
    USE mo_model_domain_lite,        ONLY: t_patch
    !USE mo_loopindices,         ONLY: get_indices_c
    USE mo_impl_constants_grf_lite,  ONLY: grf_bdywidth_c
    USE mo_impl_constants_lite,      ONLY: min_rlcell_int
    !USE mo_sync,                ONLY: global_sum_array
    USE mtime_lite,                  ONLY: t_datetime => datetime!,                 &
                                      ! t_timedelta => timedelta,               &
                                      ! OPERATOR(+), OPERATOR(<=), OPERATOR(<), &
                                      ! newDatetime, newTimedelta
    !USE mo_master_config,       ONLY: experimentStartDate, experimentStopDate 
    USE mo_parallel_config_lite,     ONLY: nproma     
    USE mo_subm_spec_lite,           ONLY: nspec, speclist 
    USE mo_hammoz_lite_emi,          ONlY: emi_read_bc_interface, emi_lite_number
    USE mo_ham_lite_main,            ONLY: sizeclass, nclass  
    USE mo_ham_lite_misc,            ONLY: lcheck_init 
    USE mo_ham_lite_spec,            ONLY: id_so4, id_bc, id_oc 

    IMPLICIT NONE
  
    INTEGER, INTENT(IN)  :: jg 
    REAL(dp), INTENT(IN) :: pareacella  (:,:),    & ! grid box area  
                            pclon       (:,:),    & ! grid cell center longtude (nproma, nblk)
                            pclat       (:,:),    & ! grid cell center latitide (nproma, nblk)
                            pzf         (:,:,:),  & ! geopotential height above sea level, full level
                            pzh         (:,:,:),  & ! geopotential height above sea level, half level
                            pdz         (:,:,:),  & ! geometric height thickness of layer  
                            pmair       (:,:,:),  & ! air mass  
                            paphm1      (:,:,:)     ! half level pressure  
    TYPE(t_patch),  INTENT(IN) :: p_patch  

    !TYPE(t_timedelta) ::  del_time, tot_time
    !TYPE(t_datetime), POINTER :: cur_time, end_time
    TYPE(t_datetime), POINTER, INTENT(IN) :: cur_time
    INTEGER ::  nblks_c, nlev, nlevp1  
    INTEGER ::  jmode, jspec, funit
    INTEGER ::  jb, jbs, jbe, jcs, jce, ncd, rls, rle 
    REAL(dp)  ::  znumb(nspec,nclass,nproma),  &
                  zlsum(nspec,nclass),    &
                  zgsum(nspec,nclass),    &
                  ztsum(nspec,nclass),    & 
                  zmode(nclass),          &
                  zfrac(nspec,nclass)
    LOGICAL :: lexist
    CHARACTER(LEN=3)  :: sname
    CHARACTER(LEN=30) :: fname 

    nblks_c = p_patch%nblks_c
    nlev    = p_patch%nlev
    nlevp1  = nlev+1 
    
    rls = grf_bdywidth_c + 1
    rle = min_rlcell_int
    ncd = MAX(1,p_patch%n_childdom)
    jbs = p_patch%cells%start_blk(rls, 1)
    jbe = p_patch%cells%end_blk  (rle, ncd)
  
    ! del_time = newTimedelta( 'P1M' )
    ! ! tot_time = newTimedelta( 'P3M' ) 
    ! tot_time = newTimedelta( 'P3Y' ) 
    !cur_time => newDatetime( experimentStartDate )   
    ! end_time => newDatetime( experimentStartDate ) 
    ! end_time = end_time + tot_time   

    ! integrate volume fluxes of species over space and time
    ztsum(:,:) = 0.0_dp 

    CALL message('','')
    CALL message('',separator)
    CALL message('lite init', 'compute composition of mixed modes')
    CALL message('','---') 
    ! WRITE (message_text,'(a,i4.4,a,i2.2,a,i2.2)') 'end time: ', &
    !   end_time%date%year, '-', end_time%date%month, '-', end_time%date%day 
    CALL message('', message_text) 
    CALL message('','---')
     
    !DO WHILE (cur_time < end_time)

      CALL message('','---')
      !WRITE (message_text,'(a,i4.4,a,i2.2,a,i2.2)') 'current time: ', &
      !  cur_time%date%year, '-', cur_time%date%month, '-', cur_time%date%day 
      CALL message('', message_text) 
      CALL message('','---')

      zlsum(:,:) = 0.0_dp 
      zgsum(:,:) = 0.0_dp 
      znumb(:,:,:) = 0.0_dp

      CALL emi_read_bc_interface( p_patch, cur_time, pareacella, pclon, pclat, pzh )

      DO jb = jbs,jbe

        ! CALL get_indices_c(p_patch, jb, jbs, jbe, jcs, jce, rls, rle)
        ! IF (jcs > jce) CYCLE
        jcs=1
        jce=1

        CALL emi_lite_number( jg, jb, jcs, jce,           &
                              nblks_c,                    &
                              nproma, nlev, nlevp1,       & 
                              cur_time,                   &  
                              pareacella(:,jb),           & 
                              pzf(:,:,jb),                &
                              pzh(:,:,jb),                &
                              pdz(:,:,jb),                &
                              pmair(:,:,jb),              & 
                              paphm1(:,:,jb),             &  
                              znumb(:,:,jb)               ) 

      END DO 

      DO jmode = 1,nclass
        IF (sizeclass(jmode)%lite_nspe > 1) THEN 
          WRITE (message_text,'(a)') 'mode' 
          CALL message('', message_text) 
          WRITE (message_text,'(a)') sizeclass(jmode)%longname 
          CALL message('', message_text) 
          WRITE (message_text,'(a)') 'species, current flux [#/s], total flux [#/s]' 
          CALL message('', message_text) 
        END IF
        DO jspec = 1,nspec  
          DO jb = jbs,jbe
            zlsum(jspec,jmode) = zlsum(jspec,jmode) + znumb(jspec,jmode,jb)
          END DO 
          zgsum(jspec,jmode) = zlsum(jspec,jmode)!global_sum_array(zlsum(jspec,jmode)) 
          ztsum(jspec,jmode) = ztsum(jspec,jmode) + zgsum(jspec,jmode)
          IF (sizeclass(jmode)%lite_nspe > 1) THEN
            IF ((jspec == id_so4) .OR. (jspec == id_oc) .OR. (jspec == id_bc)) THEN 
              sname = TRIM(speclist(jspec)%shortname)
              WRITE (message_text,'(a,5x,e14.7,5x,e14.7)') sname, zgsum(jspec,jmode), ztsum(jspec,jmode)
              CALL message('', message_text) 
            END IF
          END IF
        END DO 
        ! IF ( p_pe .EQ. p_io ) THEN  
        !   IF (sizeclass(jmode)%lite_nspe > 1) THEN
        !     fname = 'emi_' // TRIM(sizeclass(jmode)%shortname) // '.txt'    
        !     funit = find_next_free_unit (30, 100)
        !     INQUIRE(file=fname, exist=lexist)
        !     OPEN(unit=funit, file=fname, action="write", position="append")  
        !     IF (.NOT. lexist) WRITE(funit,'(5x,a,4x,a3,14x,a3,14x,a3)') '[#/s]',      &
        !       speclist(id_so4)%shortname, speclist(id_oc)%shortname, speclist(id_bc)%shortname  
        !     WRITE(funit,'(i4.4,2(a,i2.2),3(3x,e14.7))')                               &
        !       cur_time%date%year, '-', cur_time%date%month, '-', cur_time%date%day,   &
        !       zgsum(id_so4,jmode), zgsum(id_oc,jmode), zgsum(id_bc,jmode)
        !     CLOSE(unit=funit)
        !   END IF
        ! END IF
      END DO 

      !cur_time = cur_time + del_time

    !END DO

    ! compute total volumes of modes 
    zmode(:) = 0.0_dp 

    DO jmode = 1,nclass  
      DO jspec = 1,nspec 
        IF ((jspec == id_so4) .OR. (jspec == id_oc) .OR. (jspec == id_bc)) THEN 
          zmode(jmode) = zmode(jmode) + ztsum(jspec,jmode)
        END IF
      END DO
    END DO

    ! compute volume fractions of species  
    zfrac(:,:) = 0.0_dp 

    DO jmode = 1,nclass  
      IF (zmode(jmode) > zeps) THEN
        DO jspec = 1,nspec   
          IF ((jspec == id_so4) .OR. (jspec == id_oc) .OR. (jspec == id_bc)) THEN 
            zfrac(jspec,jmode) = ztsum(jspec,jmode) / zmode(jmode)
          END IF 
        END DO
        CALL update_mixed_mode(jmode, zfrac(:,jmode))
      END IF 
    END DO

    ! print checks 
    IF (lcheck_init) THEN  
      IF (p_pe == p_io) THEN
        write(*,*) 'ham_lite_init_modes - checks' 
        !hhermann: Commented for global runs
        !DO jmode = 1,nclass
          !write(*,*) 'mode'
          !write(*,'(a,5x,e12.4,e12.4)') TRIM(sizeclass(jmode)%shortname), zmode(jmode)
          !DO jspec = 1,nspec 
            !IF ((jspec == id_so4) .OR. (jspec == id_oc) .OR. (jspec == id_bc)) THEN
              !write(*,*) 'species'
              !write(*,'(a,5x,e12.4,5x,e12.4,5x,e12.4)') TRIM(speclist(jspec)%shortname),  &
              !  speclist(jspec)%density, ztsum(jspec,jmode), zfrac(jspec,jmode)
            !END IF
          !END DO
        !END DO
      END IF
     
    END IF 

  END SUBROUTINE ham_lite_init_modes

!---------------------------------------------------------------------------------------------------

  SUBROUTINE init_new_mode(imode,                                           &
                           longname,                                        & 
                           shortname,                                       &  
                           lsoluble,                                        &
                           lsedi,                                           & 
                           lacti,                                           &
                           radius,                                          &
                           msize,                                           &
                           icomp,                                           &
                           vcomp,                                           & 
                           esize,                                           &
                           ename                                            )
  
    USE mo_subm_spec_lite,           ONLY: speclist 
    USE mo_ham_lite_main,            ONLY: sizeclass 

    
    IMPLICIT NONE

    INTEGER, INTENT(IN)          :: imode         ! index
    CHARACTER(LEN=*), INTENT(IN) :: longname      ! long name 
    CHARACTER(LEN=*), INTENT(IN) :: shortname     ! short name 
    LOGICAL, INTENT(IN)          :: lsoluble      ! soluble (t/f)
    LOGICAL, INTENT(IN)          :: lsedi         ! sedimentation (t/f) 
    LOGICAL, INTENT(IN)          :: lacti         ! activation to cloud droplets  
    REAL(dp), INTENT(IN)         :: radius        ! count median dry radius [m]  
    INTEGER, INTENT(IN)          :: msize         ! number of species
    INTEGER, INTENT(IN)          :: icomp(msize)  ! indices of species
    REAL(dp), INTENT(IN)         :: vcomp(msize)  ! volume fractions of species   
    INTEGER, INTENT(IN)          :: esize         ! number of emission sector
    CHARACTER(LEN=*), INTENT(IN) :: ename(esize)  ! names of emission sectors

    INTEGER           :: jspec, jsect, index
    REAL(dp)          :: bpar1, bpar2 
    CHARACTER(LEN=3)  :: sname

    sizeclass(imode)%longname     = TRIM(longname)
    sizeclass(imode)%shortname    = TRIM(shortname) 
    sizeclass(imode)%lsoluble     = lsoluble
    sizeclass(imode)%lsedi        = lsedi 
    sizeclass(imode)%lacti        = lacti
    sizeclass(imode)%lite_radi    = radius
    sizeclass(imode)%lite_dens    = 0.0_dp
    sizeclass(imode)%lite_kappa   = 0.0_dp 
    sizeclass(imode)%lite_nspe    = msize
    sizeclass(imode)%lite_ispe(:) = 0
    sizeclass(imode)%lite_vspe(:) = 0.0_dp 
    sizeclass(imode)%lite_nemi    = esize 
    sizeclass(imode)%lite_iemi(:) = ' '

    DO jspec = 1,msize 
      sizeclass(imode)%lite_ispe(jspec) = icomp(jspec)
      sizeclass(imode)%lite_vspe(jspec) = vcomp(jspec)
      
      sizeclass(imode)%lite_dens = sizeclass(imode)%lite_dens + &
        sizeclass(imode)%lite_vspe(jspec) * speclist(icomp(jspec))%density

      sizeclass(imode)%lite_kappa = sizeclass(imode)%lite_kappa + &
        sizeclass(imode)%lite_vspe(jspec) * speclist(icomp(jspec))%kappa 
    END DO
 
    DO jsect = 1,esize
      sizeclass(imode)%lite_iemi(jsect) = TRIM(ename(jsect)) 
    END DO

  END SUBROUTINE init_new_mode

!---------------------------------------------------------------------------------------------------

  SUBROUTINE update_mixed_mode( imode, vcomp )
  
    USE mo_subm_spec_lite,           ONLY: nspec, speclist 
    USE mo_ham_lite_main,            ONLY: sizeclass 

    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: imode     ! index   
    REAL(dp), INTENT(IN)  :: vcomp(:)  ! volume fractions of species    

    INTEGER :: jspec, kspec, msize  
  
    msize = sizeclass(imode)%lite_nspe  

    sizeclass(imode)%lite_dens  = 0.0_dp
    sizeclass(imode)%lite_kappa = 0.0_dp

    DO jspec = 1,msize 
      kspec = sizeclass(imode)%lite_ispe(jspec)
      
      sizeclass(imode)%lite_vspe(jspec) = vcomp(kspec)
     
      sizeclass(imode)%lite_dens = sizeclass(imode)%lite_dens + &
        sizeclass(imode)%lite_vspe(jspec) * speclist(kspec)%density

      sizeclass(imode)%lite_kappa = sizeclass(imode)%lite_kappa + &
        sizeclass(imode)%lite_vspe(jspec) * speclist(kspec)%kappa
    END DO

  END SUBROUTINE update_mixed_mode

!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_print_modes
  
    USE mo_subm_spec_lite,           ONLY: nspec, speclist  
    USE mo_ham_lite_main,            ONLY: nclass, sizeclass 

    IMPLICIT NONE
 
    INTEGER           ::  imode, jspec, jsect, kspec, esize, msize 
    CHARACTER(LEN=3)  ::  sname

    CALL message('','')
    CALL message('',separator)
    CALL message('lite init', 'print properties of modes')
    CALL message('','---')

    DO imode = 1, nclass
  
      esize = sizeclass(imode)%lite_nemi
      msize = sizeclass(imode)%lite_nspe  

      WRITE(message_text,'(a,5x,a)') 'lite mode:', sizeclass(imode)%longname
      CALL message('', message_text)
      CALL message('', '')
      WRITE(message_text,'(a)') 'short name, radius [m], density [kg/m^3], hygroscopicity (kappa) [-]'
      CALL message('', message_text)
      sname = TRIM(sizeclass(imode)%shortname)
      WRITE(message_text,'(a,5x,e14.7,5x,e14.7,5x,e14.7)') sname, sizeclass(imode)%lite_radi,  &
        sizeclass(imode)%lite_dens, sizeclass(imode)%lite_kappa
      CALL message('', message_text)

      WRITE(message_text,'(a)') 'species, volume fraction [-], density [kg/m^3], hygroscopicity (kappa) [-]'
      CALL message('', message_text)
      DO jspec = 1,msize
        kspec = sizeclass(imode)%lite_ispe(jspec)
        sname = TRIM(speclist(kspec)%shortname)
        WRITE(message_text,'(a,5x,e14.7,5x,e14.7,5x,e14.7)') sname, sizeclass(imode)%lite_vspe(jspec), &
          speclist(kspec)%density, speclist(kspec)%kappa
        CALL message('', message_text)
      END DO 

      WRITE(message_text,'(a)') 'emission sectors'
      CALL message('', message_text)
      DO jsect = 1,esize
        WRITE(message_text,'(a,5x)') sizeclass(imode)%lite_iemi(jsect)
        CALL message('', message_text) 
      END DO
      
      CALL message('', '')

    END DO 

    CALL message('',separator)

  END SUBROUTINE ham_lite_print_modes
  
END MODULE mo_ham_lite_init