! 
! based on mo_ham_dust.f90 
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

MODULE mo_ham_lite_dust
 
  USE mo_kind_lite,            ONLY: dp
!  USE mo_mpi_lite,             ONLY: p_bcast, p_io, p_pe,  p_comm_work
  USE mo_ham_lite_main,        ONLY: ndust  

!# include "add_var_acc_macro.inc"

  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: set_dust,                 &
            bgc_dust_init,            & 
            bgc_dust_read_monthly,    &
            bgc_dust_calc_emis,       & 
            bgc_dust_set_nduscale

  PUBLIC :: ntrace, &
            rough_diag, ustar_diag     

  !--------------------
  ! module parameters
  !--------------------
  
  INTEGER, PARAMETER      :: ntrace   = 8             ! number of tracers
  INTEGER, PARAMETER      :: nbin     = 24            ! number of bins per tracer 
  INTEGER, PARAMETER      :: nclass   = ntrace*nbin   ! number of particle classes
  INTEGER, PARAMETER      :: nmode    = 4
  INTEGER, PARAMETER      :: nspe     = nmode*3+2
  INTEGER, PARAMETER      :: nsall    = 17            ! number of soil types
  INTEGER, PARAMETER      :: nsoil    = 9

  REAL(dp), PARAMETER     :: Dmin     = 0.00002_dp              ! minimum particules diameter (cm)
  REAL(dp), PARAMETER     :: Dmax     = 0.13_dp                 ! maximum particules diameter (cm)
  REAL(dp), PARAMETER     :: Dstep    = 0.0460517018598807_dp   ! diameter increment (cm)
  REAL(dp), PARAMETER     :: aeff     = 0.35_dp                 ! efficient fraction
  REAL(dp), PARAMETER     :: xeff     = 10._dp                  ! efficient fraction
  REAL(dp), PARAMETER     :: a_rnolds = 1331.647_dp             ! Reynolds constant
  REAL(dp), PARAMETER     :: b_rnolds = 0.38194_dp              ! Reynolds constant
  REAL(dp), PARAMETER     :: x_rnolds = 1.561228_dp             ! Reynolds constant
  REAL(dp), PARAMETER     :: d_thrsld = 0.00000231_dp           ! thresold value
  REAL(dp), PARAMETER     :: roa      = 0.001227_dp             ! air density (g cm-3)
  REAL(dp), PARAMETER     :: rop      = 2.65_dp                 ! particle density (g cm-3) 
  REAL(dp), PARAMETER     :: wrt_max  = 0.99_dp                 ! threshold of relative soil humidity 
  REAL(dp), PARAMETER     :: ZZ       = 1000._dp                ! wind measurement height (cm) 

  LOGICAL, PARAMETER  ::  lsurficon = .FALSE. ! roughness length and friction velocity from ICON 

  REAL(dp), DIMENSION(:,:), ALLOCATABLE :: rough_diag, ustar_diag ! diagnostics

  CHARACTER(LEN=2),PARAMETER :: &
    jtypes(nsoil) = (/ ' 2',' 3',' 4',' 6','13','14','15','16','17' /) ! soil labels

  CHARACTER(LEN=*), PARAMETER :: thismodule='mo_ham_lite_dust'

  !--------------------
  ! module variables
  !--------------------

  INTEGER :: ibc_regint ! boundary condition indices for regional mask file

  INTEGER :: k_dust_easo  = 1            ! Including the new East-Asia soil type 
                                        !    1: off
                                        !    2: on 
                                        ! default 1
                                        !
  REAL(dp):: ndurough     = 1.E-03_dp   ! surface roughness length (cm) 
                                        ! default 1.E-03_dp
                                        ! 
  REAL(dp):: nduscale_1d(8) = 8.6E-01_dp  ! Regional scale factor for threshold wind friction velocity
                                          !  
                                          ! The indices correspond to the following regions
                                          !     1       All the other locations than the following regions
                                          !     2       North america
                                          !     3       South America 
                                          !     4       North Africa 
                                          !     5       South Africa 
                                          !     6       Middle East 
                                          !     7       Asia
                                          !     8       Australia 
  REAL(dp):: r_dust_lai   = 1.0E-10_dp    ! Parameter for the threshold lai value (unitless)
                                          ! default 1.E-10
                                          !
  REAL(dp):: r_dust_umin  = 2.1E+01_dp    ! Minimum threshold friction windspeed (cm/s)
                                          ! default 21. cm/s
                                          !
  REAL(dp):: r_dust_z0s   = 1.0E-03_dp    ! z0s (cm)
                                          ! default 0.001 cm
                                          !
  REAL(dp):: r_dust_scz0  = 1.0E+00_dp    ! Scale factor of satellite z0 (unitless)
                                          ! default 1.
                                          !
  REAL(dp):: r_dust_z0min = 1.0E-05_dp    ! Parameter for minimum of z0 (cm)
                                          ! default 1.E-05 cm
                                          !
  REAL(dp):: r_dust_sf13  = 1.0E+00_dp    ! duscale over Takelimakan desert (unitless)
  REAL(dp):: r_dust_sf14  = 1.0E+00_dp    ! duscale over Loess (unitless)
  REAL(dp):: r_dust_sf15  = 1.0E+00_dp    ! duscale over Gobi desert (unitless)
  REAL(dp):: r_dust_sf16  = 1.0E+00_dp    ! duscale over other mixture soils (unitless)
  REAL(dp):: r_dust_sf17  = 1.0E+00_dp    ! duscale over desert and sand land (unitless)
                                          ! default 1.
                                          !
  REAL(dp):: r_dust_af13  = 1.9E-06_dp    ! Parameter for the alfa value over Takelimakan desert (unitless)
  REAL(dp):: r_dust_af14  = 1.9E-04_dp    ! Parameter for the alfa value over Loess (unitless)
  REAL(dp):: r_dust_af15  = 3.9E-05_dp    ! Parameter for the alfa value over Gobi desert  (unitless)
  REAL(dp):: r_dust_af16  = 3.1E-05_dp    ! Parameter for the alfa value over other mixture soils (unitless)
  REAL(dp):: r_dust_af17  = 2.8E-06_dp    ! Parameter for the alfa value over desert and sand land  (unitless)
                                          ! default values taken from Cheng et al.(2008)
  
  REAL(dp), ALLOCATABLE :: k_fpar_eff(:,:)  
  REAL(dp), ALLOCATABLE :: nduscale_2d(:,:) ! 2D nduscale  
  REAL(dp), ALLOCATABLE :: mat_s1(:,:)      ! soil type #1 area (in relative percentage)
  REAL(dp), ALLOCATABLE :: mat_s2(:,:)      ! "          
  REAL(dp), ALLOCATABLE :: mat_s3(:,:)      ! "          
  REAL(dp), ALLOCATABLE :: mat_s4(:,:)      ! "          
  REAL(dp), ALLOCATABLE :: mat_s6(:,:)      ! "          
  REAL(dp), ALLOCATABLE :: mat_s13(:,:)     ! "          
  REAL(dp), ALLOCATABLE :: mat_s14(:,:)     ! "          
  REAL(dp), ALLOCATABLE :: mat_s15(:,:)     ! "         
  REAL(dp), ALLOCATABLE :: mat_s16(:,:)     ! "          
  REAL(dp), ALLOCATABLE :: mat_s17(:,:)     ! "          
  REAL(dp), ALLOCATABLE :: mat_psa(:,:)     ! preferential source area (in relative percentage)
  REAL(dp), ALLOCATABLE :: mat_msg(:,:)     ! Saharan preferential source area (in relative percentage) 

  REAL(dp)              :: srel(nsall,nclass)     ! relative surface 
  REAL(dp)              :: srelV(nsall,nclass)
  REAL(dp)              :: su_srelV(nsall,nclass) 
  REAL(dp)              :: Uth(nclass)            ! threshold friction velocity
 
  ! soil properties
  REAL(dp) :: solspe(17,14)

  ! 'boundary conditions' from JSBACH 
  INTEGER, SAVE :: ibc_ssf     = -1,   &  ! index of surface snow fraction 'boundary condition' 
                   ibc_ws_l1   = -1,   &  ! index of soil water content [m] 'boundary condition' 
                   ibc_ws_fc_l1 = -1      ! index of field capacity [m] 'boundary condition'  

  CONTAINS

!---------------------------------------------------------------------------------------------------

  SUBROUTINE set_dust_data
 
  !     1       Tropical evergreen broadleaf forest
  !     2       Tropical semi-evergreen broadleaf forest
  !     3       Tropical deciduous broadleaf forest and woodland
  !     4       Temperate deciduous broadleaf forest
  !     5       Temperate evergreen needleleaf forest
  !     6       Warm-temperate evergreen broadleaf and mixed forest
  !     7       Cool mixed forest
  !     8       Cool evergreen needleleaf forest
  !     9       Cool-temperate evergreen needleleaf and mixed forest
  !     10      Cold evergreen needleleaf forest
  !     11      Cold deciduous forest
  !     12      Tropical savanna
  !     13      Tropical xerophytic shrubland
  !     14      Temperate xerophytic shrubland
  !     15      Temperate sclerophyll woodland and shrubland
  !     16      Temperate deciduous broadleaf savanna
  !     17      Temperate evergreen needleleaf open woodland
  !     18      Cold parkland
  !     19      Tropical grassland
  !     20      Temperate grassland
  !     21      Desert
  !     22      Graminoid and forb tundra
  !     23      Low and high shrub tundra
  !     24      Erect dwarf-shrub tundra
  !     25      Prostrate dwarf-shrub tundra
  !     26      Cushion-forb tundra
  !     27      Barren
  !     28      Ice
  !    (29)     Water (this is implied)
  
  !   Populations: Coarse sand, medium/fine sand, Silt, Clay 
  !
  !     soil type 1 : Coarse     
  !     soil type 2 : Medium     
  !     soil type 3 : Fine     
  !     soil type 4 : Coarse Medium      
  !     soil type 5 : Coarse Fine 
  !     soil type 6 : Medium Fine 
  !     soil type 7 : Coarse_dp, Medium_dp, Fine 
  !     soil type 8 : Organic
  !     soil type 9 : Ice
  !     soil type 10 : Potential Lakes (additional)
  !     soil type 11 : Potential Lakes (clay)
  !     soil type 12 : Potential Lakes Australia
  !     soil type 13 : Taklimakan desert
  !     soil type 14 : Asian Loess
  !     soil type 15 : Asian Gobi
  !     soil type 16 : Asian other mixture
  !     soil type 17 : Asian desert
  !----------------------------------------------------------------------------

  solspe = &
       RESHAPE ( (/ &
       0.0707_dp, 2.0_dp, 0.43_dp, 0.0158_dp, 2.0_dp, 0.40_dp, 0.0015_dp, &!1 
       2.0_dp, 0.17_dp, 0.0002_dp, 2.0_dp, 0.00_dp, 2.1e-06_dp, 0.20_dp,  &  
       0.0707_dp, 2.0_dp, 0.00_dp, 0.0158_dp, 2.0_dp, 0.37_dp, 0.0015_dp, &!2 
       2.0_dp, 0.33_dp, 0.0002_dp, 2.0_dp, 0.30_dp, 4.0e-06_dp, 0.25_dp,  &  
       0.0707_dp, 2.0_dp, 0.00_dp, 0.0158_dp, 2.0_dp, 0.00_dp, 0.0015_dp, &!3 
       2.0_dp, 0.33_dp, 0.0002_dp, 2.0_dp, 0.67_dp, 1.0e-07_dp, 0.50_dp,  &   
       0.0707_dp, 2.0_dp, 0.10_dp, 0.0158_dp, 2.0_dp, 0.50_dp, 0.0015_dp, &!4 
       2.0_dp, 0.20_dp, 0.0002_dp, 2.0_dp, 0.20_dp, 2.7e-06_dp, 0.23_dp,  &   
       0.0707_dp, 2.0_dp, 0.00_dp, 0.0158_dp, 2.0_dp, 0.50_dp, 0.0015_dp, &!5 
       2.0_dp, 0.12_dp, 0.0002_dp, 2.0_dp, 0.38_dp, 2.8e-06_dp, 0.25_dp,  &   
       0.0707_dp, 2.0_dp, 0.00_dp, 0.0158_dp, 2.0_dp, 0.27_dp, 0.0015_dp, &!6 
       2.0_dp, 0.25_dp, 0.0002_dp, 2.0_dp, 0.48_dp, 1.0e-07_dp, 0.36_dp,  &   
       0.0707_dp, 2.0_dp, 0.23_dp, 0.0158_dp, 2.0_dp, 0.23_dp, 0.0015_dp, &!7 
       2.0_dp, 0.19_dp, 0.0002_dp, 2.0_dp, 0.35_dp, 2.5e-06_dp, 0.25_dp,  &   
       0.0707_dp, 2.0_dp, 0.25_dp, 0.0158_dp, 2.0_dp, 0.25_dp, 0.0015_dp, &!8 
       2.0_dp, 0.25_dp, 0.0002_dp, 2.0_dp, 0.25_dp, 0.0e-00_dp, 0.50_dp,  &   
       0.0707_dp, 2.0_dp, 0.25_dp, 0.0158_dp, 2.0_dp, 0.25_dp, 0.0015_dp, &!9 
       2.0_dp, 0.25_dp, 0.0002_dp, 2.0_dp, 0.25_dp, 0.0e-00_dp, 0.50_dp,  &   
       0.0707_dp, 2.0_dp, 0.00_dp, 0.0158_dp, 2.0_dp, 0.00_dp, 0.0015_dp, &!10: 100% silt 
       2.0_dp, 1.00_dp, 0.0002_dp, 2.0_dp, 0.00_dp, 1.0e-05_dp, 0.25_dp,  &
       0.0707_dp, 2.0_dp, 0.00_dp, 0.0158_dp, 2.0_dp, 0.00_dp, 0.0015_dp, &!11: 100% clay
       2.0_dp, 0.00_dp, 0.0002_dp, 2.0_dp, 1.00_dp, 1.0e-05_dp, 0.25_dp,  & 
       0.0707_dp, 2.0_dp, 0.00_dp, 0.0158_dp, 2.0_dp, 0.00_dp, 0.0027_dp, &!12: 100% silt 
       2.0_dp, 1.00_dp, 0.0002_dp, 2.0_dp, 0.00_dp, 1.0e-05_dp, 0.25_dp,  & 
       0.0442_dp, 1.5_dp, 0.03_dp, 0.0084_dp, 1.5_dp, 0.85_dp, 0.0015_dp, &!13: taklimakan
       2.0_dp, 0.11_dp, 0.0002_dp, 2.0_dp, 0.02_dp, 1.9e-06_dp, 0.12_dp,  & 
       0.0450_dp, 1.5_dp, 0.00_dp, 0.0070_dp, 1.5_dp, 0.33_dp, 0.0015_dp, &!14: loess 
       2.0_dp, 0.50_dp, 0.0002_dp, 2.0_dp, 0.17_dp, 1.9e-04_dp, 0.15_dp,  & 
       0.0457_dp, 1.8_dp, 0.31_dp, 0.0086_dp, 1.5_dp, 0.22_dp, 0.0015_dp, &!15: gobi 
       2.0_dp, 0.34_dp, 0.0002_dp, 2.0_dp, 0.12_dp, 3.9e-05_dp, 0.13_dp,  & 
       0.0293_dp, 1.8_dp, 0.39_dp, 0.0090_dp, 1.5_dp, 0.16_dp, 0.0015_dp, &!16: other mixture soils 
       2.0_dp, 0.35_dp, 0.0002_dp, 2.0_dp, 0.10_dp, 3.1e-05_dp, 0.13_dp,  & 
       0.0305_dp, 1.5_dp, 0.46_dp, 0.0101_dp, 1.5_dp, 0.41_dp, 0.0015_dp, &!17: desert and sand land
       2.0_dp, 0.10_dp, 0.0002_dp, 2.0_dp, 0.03_dp, 2.8e-06_dp, 0.12_dp   &
        /), (/ 17, 14 /), ORDER=(/2,1/))


  END SUBROUTINE set_dust_data

!---------------------------------------------------------------------------------------------------

  SUBROUTINE set_dust_scale ( p_patch )
 
    !     1       All the other locations than the following regions
    !     2       North america
    !     3       South America 
    !     4       North Africa 
    !     5       South Africa 
    !     6       Middle East 
    !     7       Asia
    !     8       Australia 

    USE mo_model_domain_lite,        ONLY: t_patch
    USE mo_parallel_config_lite,     ONLY: nproma
    USE mo_util_extfield_lite,       ONLY: EF_FILE, EF_LONLAT, EF_IGNOREYEAR, EF_NOINTER
    USE mo_subm_bcs_lite,            ONLY: bc_nml, BC_BOTTOM, BC_REPLACE, bc_define, BC_BOTTOM,BC_REPLACE

    TYPE(t_patch), INTENT(IN)  :: p_patch

    INTEGER :: nblks_c
    TYPE(bc_nml) :: bc_regint            

    nblks_c = p_patch%nblks_c
 
    IF (.NOT. ALLOCATED(nduscale_2d)) THEN
      ALLOCATE (nduscale_2d(nproma, nblks_c))
      !$ACC ENTER DATA COPYIN(nduscale_2d)   
    END IF
 
    ibc_regint   = -1
 
    bc_regint%bc_domain = BC_BOTTOM
    bc_regint%ef_type = EF_FILE
    bc_regint%ef_template = '/scratch/project_462000807/hhermann/' // &
        'hamlite_piggyback/orig_nc/dust_regions.nc' 
    bc_regint%ef_varname = 'regions'
    bc_regint%ef_geometry = EF_LONLAT
    bc_regint%ef_timedef = EF_IGNOREYEAR
    bc_regint%ef_interpolate = EF_NOINTER 
    bc_regint%bc_mode = BC_REPLACE
 
!    CALL p_bcast_bc (bc_regint, p_io)
     
    ibc_regint   = bc_define( p_patch%id, 'regions for different nduscale', bc_regint, 2, .TRUE.)
    
  END SUBROUTINE set_dust_scale
     
!---------------------------------------------------------------------------------------------------

  SUBROUTINE bgc_dust_set_nduscale( jg, p_patch, datetime )  
 
    USE mtime_lite,                  ONLY: t_datetime => datetime
    USE mo_model_domain_lite,        ONLY: t_patch
    USE mo_parallel_config_lite,     ONLY: nproma
    USE mo_impl_constants_lite,      ONLY: min_rlcell_int, grf_bdywidth_c
    ! USE mo_loopindices,         ONLY: get_indices_c
    USE hamlite_driver_input,        ONLY: jcs,jce
    USE mo_subm_bcs_lite,            ONLY: bc_apply

    IMPLICIT NONE

    INTEGER,                    INTENT(IN) :: jg
    TYPE(t_datetime), POINTER,  INTENT(IN) :: datetime      
    TYPE(t_patch),              INTENT(IN) :: p_patch
 
    INTEGER, PARAMETER :: nregion = 8  
    !$ACC DECLARE COPYIN(nregion) 

    INTEGER   ::  jb, jl, jr, jbs, jbe, ncd, rls, rle       
    INTEGER   ::  nblks_c  
    REAL(dp)  ::  ztemp(nproma) 

    nblks_c = p_patch%nblks_c
    
    rls = grf_bdywidth_c+1
    rle = min_rlcell_int 
    ncd = MAX(1,p_patch%n_childdom)
    jbs = p_patch%cells%start_blk(rls,1)
    jbe = p_patch%cells%end_blk(rle,ncd)
    
    !$ACC DATA CREATE(ztemp) &
    !$ACC   COPYIN(nduscale_1d)
    DO jb = jbs,jbe

      !CALL get_indices_c(p_patch, jb, jbs, jbe, jcs, jce, rls, rle)

      IF (jcs > jce) CYCLE

      CALL bc_apply(jg, jb, jcs, jce, ibc_regint, nblks_c, datetime, ztemp)
      !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
      !$ACC LOOP SEQ
      DO jr = 1,nregion
        !$ACC LOOP GANG VECTOR
        DO jl = jcs,jce
          nduscale_2d(jl,jb) = MERGE(nduscale_1d(jr), nduscale_2d(jl,jb), INT(ztemp(jl)) .EQ. jr)
        END DO
        !$ACC END LOOP
      END DO
      !$ACC END PARALLEL
    END DO
    !$ACC WAIT(1) 
    !$ACC END DATA    
  END SUBROUTINE bgc_dust_set_nduscale

!---------------------------------------------------------------------------------------------------

  SUBROUTINE bgc_dust_read_yearly( p_patch )
 
    !eehol: this is modified for boxmodel approach
  
    USE mo_exception_lite,         ONLY: message, message_text 
    USE mo_util_string_lite,       ONLY: separator 
    USE mo_parallel_config_lite,   ONLY: nproma
    USE mo_model_domain_lite,      ONLY: t_patch 
    !USE mo_io_config,         ONLY: default_read_method
    !USE mo_read_interface,    ONLY: openInputFile, closeFile, on_cells, &
    !                                t_stream_id, read_2D_time, read_2D
    USE mo_util_netcdf77_lite,     ONLY: read_var_hs_nf77_2d, &
                                    read_var_nf77_2d

    IMPLICIT NONE

    TYPE(t_patch), INTENT(IN) :: p_patch

    INTEGER           :: nblks_c
    INTEGER           :: jb, jl, js, ierr
    CHARACTER(512)    :: cfile                            
    REAL(dp)          :: zeastasia           
    REAL(dp), POINTER :: zvar2d(:,:), zvar3d(:,:,:)
    !TYPE(t_stream_id) :: stream_id
                          
    nblks_c = p_patch%nblks_c
 
    IF (.NOT. ALLOCATED(mat_s1))  ALLOCATE (mat_s1(nproma, nblks_c))
    IF (.NOT. ALLOCATED(mat_s2))  ALLOCATE (mat_s2(nproma, nblks_c))
    IF (.NOT. ALLOCATED(mat_s3))  ALLOCATE (mat_s3(nproma, nblks_c))
    IF (.NOT. ALLOCATED(mat_s4))  ALLOCATE (mat_s4(nproma, nblks_c))
    IF (.NOT. ALLOCATED(mat_s6))  ALLOCATE (mat_s6(nproma, nblks_c))
    IF (.NOT. ALLOCATED(mat_s13)) ALLOCATE (mat_s13(nproma, nblks_c))    
    IF (.NOT. ALLOCATED(mat_s14)) ALLOCATE (mat_s14(nproma, nblks_c))
    IF (.NOT. ALLOCATED(mat_s15)) ALLOCATE (mat_s15(nproma, nblks_c))
    IF (.NOT. ALLOCATED(mat_s16)) ALLOCATE (mat_s16(nproma, nblks_c))
    IF (.NOT. ALLOCATED(mat_s17)) ALLOCATE (mat_s17(nproma, nblks_c)) 
    IF (.NOT. ALLOCATED(mat_psa)) ALLOCATE (mat_psa(nproma, nblks_c))

    IF (ndust == 5) THEN
      IF (.NOT. ALLOCATED(mat_msg))  ALLOCATE (mat_msg(nproma, nblks_c))
    END IF
 
    mat_s1(:,:) = 1._dp       
  
    CALL message('',separator)
    CALL message('bgc_dust_read_yearly','reading soil type distributions')

    cfile='/scratch/project_462000807/hhermann/hamlite_piggyback/orig_nc/' // &
        'soil_type_all.nc'
    CALL check_input_file_exist('bgc_dust_read_yearly',cfile)

    !CALL openInputFile(stream_id, cfile, p_patch, default_read_method)

    DO js = 1,nsoil

      !eehol: Allocate your pointer based on dimension sizes (NEED TO BE SAME AS INCOMING FILE!!!! JUST HARDCODED FOR NOW!!)
      ALLOCATE(zvar2d(2, 2))

      WRITE(message_text,'(a,a,a,a)') &
        'read soil type #', ADJUSTL(TRIM(jtypes(js))), ' distribution from: ', TRIM(cfile)
      CALL message('',message_text)

      ! CALL read_2D_time(  stream_id=stream_id, location=on_cells,             &
      !                     variable_name="type" // ADJUSTL(TRIM(jtypes(js))),  &
      !                     return_pointer=zvar3d                               )
      ! CALL shape_check_fields(  SHAPE(mat_s1(:,:)), SHAPE(zvar3d(:,:,1)),     &
      !                           cfile,"type" // ADJUSTL(TRIM(jtypes(js))),    &
      !                           'bgc_dust_read_yearly','mo_ham_lite_dust'     ) 
      !eehol: read netcdf
      call read_var_hs_nf77_2d(cfile, "lat", "lon", "time", 1, "type" // ADJUSTL(TRIM(jtypes(js))), zvar2d, ierr)

      IF (js == 1) mat_s2 (:,:) = zvar2d(nproma, nblks_c)
      IF (js == 2) mat_s3 (:,:) = zvar2d(nproma, nblks_c)
      IF (js == 3) mat_s4 (:,:) = zvar2d(nproma, nblks_c)
      IF (js == 4) mat_s6 (:,:) = zvar2d(nproma, nblks_c)
      IF (js == 5) mat_s13(:,:) = zvar2d(nproma, nblks_c)
      IF (js == 6) mat_s14(:,:) = zvar2d(nproma, nblks_c)
      IF (js == 7) mat_s15(:,:) = zvar2d(nproma, nblks_c)
      IF (js == 8) mat_s16(:,:) = zvar2d(nproma, nblks_c)
      IF (js == 9) mat_s17(:,:) = zvar2d(nproma, nblks_c)
      DEALLOCATE(zvar2d)

    END DO

    !CALL closeFile(stream_id)

    !eehol: Allocate your pointer based on dimension sizes (NEED TO BE SAME AS INCOMING FILE!!!! JUST HARDCODED FOR NOW!!)
    ALLOCATE(zvar2d(2, 2))

    !--- Read preferential sources distribution:
    cfile='/scratch/project_462000807/hhermann/hamlite_piggyback/orig_nc/' // &
        'dust_preferential_sources.nc'  

    WRITE(message_text,'(a,a)') 'reading potential sources distribution ', TRIM(cfile)
    CALL message('',message_text)

    CALL check_input_file_exist('bgc_dust_read_yearly',cfile)

    ! CALL openInputFile(stream_id, cfile, p_patch, default_read_method)

    ! CALL read_2D_time(stream_id=stream_id, location=on_cells, variable_name="source", &
    !                   return_pointer=zvar3d                                           )

    ! CALL shape_check_fields(SHAPE(mat_psa),SHAPE(zvar3d(:,:,1)),cfile,"source", &
    !                         'bgc_dust_read_yearly','mo_ham_lite_dust'           )
    call read_var_hs_nf77_2d(cfile, "lat", "lon", "time", 1, "source", zvar2d, ierr)

    mat_psa(:,:) = zvar2d(nproma,nblks_c)

    DEALLOCATE(zvar2d)
    !CALL closeFile(stream_id)
 
    IF (ndust == 5) THEN

      !eehol: Allocate your pointer based on dimension sizes (NEED TO BE SAME AS INCOMING FILE!!!! JUST HARDCODED FOR NOW!!)
      ALLOCATE(zvar2d(2, 2))

      cfile='/scratch/project_462000807/hhermann/hamlite_piggyback/orig_nc/' // &
        'dust_msg_pot_sources.nc'

      WRITE(message_text,'(a,a)') 'reading potential Saharan sources', TRIM(cfile)
      CALL message('',message_text)

      CALL check_input_file_exist('bgc_dust_read_yearly',cfile)

      ! CALL openInputFile(stream_id, cfile, p_patch, default_read_method)

      ! CALL read_2D(stream_id=stream_id, location=on_cells, variable_name="dsaf",  &
      !              return_pointer=zvar2d                                          )

      ! CALL shape_check_fields(SHAPE(mat_msg),SHAPE(zvar2d(:,:)),cfile,"dasf", &
      !                         'bgc_dust_read_yearly', 'mo_ham_lite_dust'      )
      call read_var_nf77_2d(cfile, "lat", "lon", "dsaf", zvar2d, ierr)

      mat_msg=zvar2d(nproma,nblks_c)

      DEALLOCATE(zvar2d)
      !CALL closeFile(stream_id)

    END IF 

    CALL message('',separator)
  
    IF (k_dust_easo.eq.2) THEN 

      DO jb = 1,nblks_c 
        DO jl = 1,nproma  
          zeastasia = mat_s13(jl,jb) + mat_s14(jl,jb) + mat_s15(jl,jb) + mat_s16(jl,jb) + mat_s17(jl,jb)
          IF (zeastasia .GT. 0) THEN
             mat_s1  (jl,jb) = zeastasia 
             mat_s2  (jl,jb) = 0._dp
             mat_s3  (jl,jb) = 0._dp
             mat_s4  (jl,jb) = 0._dp
             mat_s6  (jl,jb) = 0._dp
             mat_psa (jl,jb) = 0._dp
          END IF
        END DO  
      END DO  

    END IF 
 
    IF (ndust == 5) THEN

       DO jb = 1,nblks_c  
         DO jl = 1,nproma   

           IF (mat_msg(jl,jb) .GT. 0._dp) THEN
             mat_s1  (jl,jb) = 0._dp
             mat_s2  (jl,jb) = 0._dp
             mat_s3  (jl,jb) = 0._dp
             mat_s4  (jl,jb) = 0._dp
             mat_s6  (jl,jb) = 0._dp
             mat_psa (jl,jb) = 0._dp
             IF (mat_msg(jl,jb) .GE. 0.01_dp) THEN
               mat_psa (jl,jb) = 1._dp 
             END IF
           END IF

         END DO
       END DO

    END IF
  
    !$ACC ENTER DATA COPYIN(mat_s1, mat_s2, mat_s3, mat_s4, mat_s6) &
    !$ACC   COPYIN(mat_s13, mat_s14, mat_s15, mat_s16, mat_s17, mat_psa, mat_msg)

  END SUBROUTINE bgc_dust_read_yearly

! !---------------------------------------------------------------------------------------------------

  SUBROUTINE bgc_dust_read_monthly( jg, p_patch, pmonth )

    !eehol: this is modified for boxmodel approach

    USE mo_model_domain_lite,        ONLY: t_patch
    USE mo_exception_lite,           ONLY: finish, message, message_text  
    USE mo_util_string_lite,         ONLY: separator
    USE mo_parallel_config_lite,     ONLY: nproma
    !USE mo_io_config,           ONLY: default_read_method
    !USE mo_read_interface,      ONLY: openInputFile, closeFile, on_cells, &
    !                                  t_stream_id, read_2D_time, read_2D
    USE mo_util_netcdf77_lite,     ONLY: read_var_hs_nf77_2d

    IMPLICIT NONE

    INTEGER,        INTENT(IN)  :: jg         
    TYPE(t_patch),  INTENT(IN)  :: p_patch    
    INTEGER,        INTENT(IN)  :: pmonth 

    INTEGER           :: nblks_c, ierr
    CHARACTER(512)    :: cfile  
    REAL(dp), POINTER :: p2d(:,:)
    !TYPE(t_stream_id) :: stream_id
  
    ! --- read effective monthly fields
 
    nblks_c = p_patch%nblks_c

    IF (.NOT. ALLOCATED(k_fpar_eff)) THEN
      ALLOCATE (k_fpar_eff(nproma, nblks_c))
      !$ACC ENTER DATA COPYIN(k_fpar_eff)
    END IF

    CALL message('',separator)
    CALL message('','')

    !eehol: Allocate your pointer based on dimension sizes (NEED TO BE SAME AS INCOMING FILE!!!! JUST HARDCODED FOR NOW!!)
    ALLOCATE(p2d(2, 2))

    cfile='/scratch/project_462000807/hhermann/hamlite_piggyback/orig_nc/' // &
    'dust_potential_sources.nc'

    WRITE(message_text,'(a,a)') 'reading dust potential sources field ', TRIM(cfile)
    CALL message('bgc_read_fpar_field',message_text)

    CALL check_input_file_exist('bgc_read_fpar_field',cfile)

    ! CALL openInputFile(stream_id, cfile, p_patch, default_read_method)

    ! CALL read_2D_time(stream_id=stream_id, location=on_cells, variable_name="pot_source", &
    !                   return_pointer=p3d, start_timestep=pmonth, end_timestep=pmonth      ) 

    ! CALL shape_check_fields(SHAPE(k_fpar_eff),SHAPE(p3d(:,:,1)),cfile,"pot_source", &
    !                         'bgc_read_fpar_field','mo_ham_lite_dust'                )

    call read_var_hs_nf77_2d(cfile, "lat", "lon", "time", 1, "pot_source", p2d, ierr)

    k_fpar_eff=p2d(nproma,nblks_c)

    DEALLOCATE(p2d)
    !CALL closeFile(stream_id)
 
    CALL message('','')
    CALL message('',separator)

    !$ACC UPDATE DEVICE(k_fpar_eff) ASYNC(1)
  END SUBROUTINE bgc_dust_read_monthly
 
!---------------------------------------------------------------------------------------------------

  SUBROUTINE set_dust ( p_patch )

    USE mo_model_domain_lite,            ONLY: t_patch 
    USE mo_exception_lite,               ONLY: finish, message, message_text, print_value 
    USE mo_util_string_lite,             ONLY: separator
    USE mo_util_config_lite,             ONLY: set_config
    USE mo_lite_dust_list_config,        ONLY: lite_dust_list

    TYPE(t_patch),INTENT(IN), DIMENSION(:)  :: p_patch

    CALL get_dust_namelist_defaults ( p_patch )
    
    ! import values from namelist

    CALL message('','')
    CALL message('',separator)
    CALL message('set_dust', 'import parameters from lite_dust_list')
    CALL message('','---')

    CALL set_config( ndurough     , lite_dust_list % ndurough       )
    CALL set_config( nduscale_1d  , lite_dust_list % nduscale_reg   )
    CALL set_config( r_dust_lai   , lite_dust_list % r_dust_lai     )
    CALL set_config( r_dust_umin  , lite_dust_list % r_dust_umin    )
    CALL set_config( r_dust_z0s   , lite_dust_list % r_dust_z0s     ) 
    CALL set_config( r_dust_scz0  , lite_dust_list % r_dust_scz0    )
    CALL set_config( r_dust_z0min , lite_dust_list % r_dust_z0min   )
    CALL set_config( r_dust_sf13  , lite_dust_list % r_dust_sf13    )
    CALL set_config( r_dust_sf14  , lite_dust_list % r_dust_sf14    )
    CALL set_config( r_dust_sf15  , lite_dust_list % r_dust_sf15    )
    CALL set_config( r_dust_sf16  , lite_dust_list % r_dust_sf16    )
    CALL set_config( r_dust_sf17  , lite_dust_list % r_dust_sf17    )
    CALL set_config( r_dust_af13  , lite_dust_list % r_dust_af13    )
    CALL set_config( r_dust_af14  , lite_dust_list % r_dust_af14    )
    CALL set_config( r_dust_af15  , lite_dust_list % r_dust_af15    )
    CALL set_config( r_dust_af16  , lite_dust_list % r_dust_af16    )
    CALL set_config( r_dust_af17  , lite_dust_list % r_dust_af17    ) 
    CALL set_config( k_dust_easo  , lite_dust_list % k_dust_easo    ) 

    ! print and check parameters

    CALL message('','')
    CALL message('set_dust', 'Parameter settings for BGC dust emission model')
    CALL message('','')
    CALL print_value('Surface roughness length (ndurough)  = ', ndurough) 

    CALL message('', 'Regional threshold wind friction velocity:')
    CALL print_value('nduscale_reg(2) (North America) = ', nduscale_1d(2)) 
    CALL print_value('nduscale_reg(3) (South America) = ', nduscale_1d(3)) 
    CALL print_value('nduscale_reg(4) (North Africa)  = ', nduscale_1d(4)) 
    CALL print_value('nduscale_reg(5) (South Africa)  = ', nduscale_1d(5)) 
    CALL print_value('nduscale_reg(6) (Middle East)   = ', nduscale_1d(6))
    CALL print_value('nduscale_reg(7) (Asia)          = ', nduscale_1d(7)) 
    CALL print_value('nduscale_reg(8) (Australia)     = ', nduscale_1d(8))
    CALL print_value('nduscale_reg(1) (elsewhere)     = ', nduscale_1d(1)) 
      
    CALL message('', '')
    CALL print_value(' r_dust_lai      = ', r_dust_lai)
    CALL print_value(' r_dust_umin     = ', r_dust_umin)
    CALL print_value(' r_dust_z0s      = ', r_dust_z0s)
    CALL print_value(' r_dust_scz0     = ', r_dust_scz0)
    CALL print_value(' r_dust_z0min    = ', r_dust_z0min)
    CALL print_value(' r_dust_sf13     = ', r_dust_sf13)
    CALL print_value(' r_dust_sf14     = ', r_dust_sf14)
    CALL print_value(' r_dust_sf15     = ', r_dust_sf15)
    CALL print_value(' r_dust_sf16     = ', r_dust_sf16)
    CALL print_value(' r_dust_sf17     = ', r_dust_sf17)
    CALL print_value(' r_dust_af13     = ', r_dust_af13)
    CALL print_value(' r_dust_af14     = ', r_dust_af14)
    CALL print_value(' r_dust_af15     = ', r_dust_af15)
    CALL print_value(' r_dust_af16     = ', r_dust_af16)
    CALL print_value(' r_dust_af17     = ', r_dust_af17)
    CALL message('', '') 
    CALL print_value(' k_dust_easo     = ', k_dust_easo)

    CALL message('',separator)
 
  END SUBROUTINE set_dust

!---------------------------------------------------------------------------------------------------

  SUBROUTINE get_dust_namelist_defaults( p_patch )

    USE mo_kind_lite,                ONLY: dp
    USE mo_model_domain_lite,        ONLY: t_patch
    USE mo_exception_lite,           ONLY: finish, message 
    USE mo_util_string_lite,         ONLY: separator

    TYPE(t_patch),INTENT(IN), DIMENSION(:) :: p_patch
 
    IF (SIZE(p_patch) .GT. 1) THEN
      CALL finish('mo_lite_ham_dust','nesting not supported')
    END IF 

    SELECT CASE (ndust) 
      CASE (3)
        !Stier et al. (2005)
        ndurough     = 0.001_dp
        r_dust_lai   = 1.E-10_dp
        r_dust_umin  = 21._dp
        r_dust_z0s   = 0.001_dp
        r_dust_scz0  = 1._dp
        r_dust_z0min = 1.E-05_dp 
        k_dust_easo  = 1
 
        nduscale_1d(:) = 0.86_dp 

      CASE (4, 5)
        ! Stier et al. (2005) with East Asia soil properties 
        ! or
        ! Stier et al. (2005) with Saharan dust source  
        ndurough     = 0.001_dp
        r_dust_lai   = 1.E-1_dp
        r_dust_umin  = 21._dp
        r_dust_z0s   = 0.001_dp
        r_dust_scz0  = 1._dp
        r_dust_z0min = 1.E-05_dp
        r_dust_af14  = 1.E-06_dp
        r_dust_sf13  = 0.6 
        k_dust_easo  = 2
 
        nduscale_1d(:) = 0.9_dp  
 
    END SELECT

  END SUBROUTINE get_dust_namelist_defaults

!---------------------------------------------------------------------------------------------------

  SUBROUTINE bgc_dust_init( jg, p_patch )
 
    USE mo_model_domain_lite,        ONLY: t_patch 
    USE mo_exception_lite,           ONLY: finish, message 
    USE mo_math_constants_lite,      ONLY: pi
    USE mo_physical_constants_lite,  ONLY: grav 
    USE mo_parallel_config_lite,     ONLY: nproma
    USE mo_util_extfield_lite,       ONLY: EF_MODULE 
    USE mo_subm_bcs_lite,            ONLY: bc_find, bc_nml, bc_define, BC_REPLACE

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: jg   
    TYPE(t_patch), INTENT(IN) :: p_patch 

    INTEGER ::  jc, jk, jm, jr, js, &
                nd, n1, n2, ierr 
    REAL(dp)  ::  za, zb, zc, zd, ze, zf, &    
                  xnV, rdp,               &
                  xk, xl, xm, xn,         &         
                  stotal,                 &   ! total surface
                  stotalV,                &
                  su,                     &   ! surface 
                  suV,                    &
                  su_loc,                 &   ! surface
                  su_locV,                &
                  su_class(nclass),       &   ! surface occupied by each granulometric class
                  su_classV(nclass),      & 
                  utest(nsall)    

    TYPE(bc_nml)  ::  bc_ssf,      &    ! surface snow fraction 'boundary condition'
                      bc_ws_l1,    &    ! soil water content 'boundary condition' 
                      bc_ws_fc_l1       ! soil field capacity 'boundary condition' 

    IF (.NOT. ALLOCATED(rough_diag)) THEN
       ALLOCATE (rough_diag(nproma, p_patch%nblks_c))
       !$ACC ENTER DATA COPYIN(rough_diag)
    END IF

    IF (.NOT. ALLOCATED(ustar_diag)) THEN
       ALLOCATE (ustar_diag(nproma, p_patch%nblks_c))
       !$ACC ENTER DATA COPYIN(ustar_diag)
    END IF

    CALL set_dust_data
    CALL set_dust_scale ( p_patch )

    CALL bgc_dust_read_yearly( p_patch )
  
    srel    (:,:) = 0._dp
    srelV   (:,:) = 0._dp 
    su_srelV(:,:) = 0._dp
    Uth       (:) = 0._dp
    utest     (:) = 0._dp

    ! calculate threshold friction velocity, formula (6) and (7) in mat-95

    jr = 0 
    rdp = Dmin

    DO WHILE (rdp .LE. Dmax + 1.E-5_dp)

      jr = jr + 1
 
      zb = a_rnolds * rdp ** x_rnolds + b_rnolds    ! formula (5) in mat-95 
      za = SQRT(rop * grav*100._dp * rdp / roa)     ! formula after (4) in mat-95 
      zc = SQRT(1._dp + d_thrsld / (rdp ** 2.5_dp)) ! formula after (4) in mat-95

      IF (zb .LT. 10._dp) THEN                      ! formula (6) in mat-95
        zd = SQRT(1.928_dp * (zb ** 0.092_dp) - 1._dp)
        Uth(jr) = 0.129_dp * za * zc / zd
      ELSE                                          ! formula (7) in mat-95
        ze = -0.0617_dp * (zb - 10._dp) 
        zf = 1._dp - 0.0858_dp * EXP(ze)   
        Uth(jr) = 0.129_dp * za * zc * zf      
      END IF

      rdp = rdp * EXP(Dstep)   

    END DO      

    ! calculate soil particle distribution and related surfaces

    DO js = 1,nsall ! loop over all soil types
       
      rdp = Dmin                                       
      jk = 0
      stotal  = 0._dp
      stotalV = 0._dp
      su_class (:) = 0._dp
      su_classV(:) = 0._dp

      DO WHILE (rdp .LE. Dmax + 1.E-5_dp) ! surface calculations

        jk = jk + 1
        su  = 0._dp
        suV = 0._dp
          
        DO jm = 1,nmode
          
          nd  = (jm-1) * 3 + 1
          n1 = nd + 1
          n2 = nd + 2

          IF (solspe(js,nd).EQ.0._dp) THEN            
            su_loc  = 0._dp
            su_locV = 0._dp
          ELSE
            xk  = solspe(js,n2) / (sqrt(2._dp * pi) * log(solspe(js,n1)))
            xl  = ((log(rdp) - log(solspe(js,nd)))**2) / (2._dp*(log(solspe(js,n1)))**2)
            xm  = xk * exp(-xl)                     ! formula (29) in mat-95
            xn  = rop * (2._dp/3._dp) * (rdp/2._dp) ! formula (30) in mat-95 
            xnV = 1._dp  
            su_loc  = xm * Dstep / xn        
            su_locV = xm * Dstep / xnV      
          END IF  

          su  = su + su_loc
          suV = suV + su_locV

        END DO  

        su_class (jk) = su
        su_classV(jk) = suV
        stotal  = stotal  + su
        stotalV = stotalV + suV
 
        rdp = rdp * exp(Dstep)

      END DO 
           
      DO jc = 1,nclass ! formula (32) in mat-95 

        IF (stotal .eq. 0._dp) THEN
          srel(js,jc) = 0._dp
          srelV(js,jc) = 0._dp
        ELSE
          srel (js,jc) = su_class (jc) / stotal
          srelV(js,jc) = su_classV(jc) / stotalV
          utest(js)       = utest(js) + srelV(js,jc)
          su_srelV(js,jc) = utest(js) ! sum of nclasses
        END IF

      END DO ! class 
      
    END DO ! soil type 
 
    ! set soil properties

    solspe(13,nmode*3+1) = r_dust_af13
    solspe(14,nmode*3+1) = r_dust_af14
    solspe(15,nmode*3+1) = r_dust_af15
    solspe(16,nmode*3+1) = r_dust_af16
    solspe(17,nmode*3+1) = r_dust_af17
  
    CALL bc_find(jg, 'surface snow fraction', ibc_ssf, ierr=ierr)
    IF (ierr /= 0) THEN
      bc_ssf%ef_type = EF_MODULE
      bc_ssf%bc_mode = BC_REPLACE
      bc_ssf%ef_actual_unit='m2 m-2'
      ibc_ssf = bc_define(jg, 'surface snow fraction', bc_ssf, 2, .TRUE.)
    ELSE
      CALL message(thismodule,'ssf bc already defined - will use previous definition')
    END IF

    CALL bc_find(jg, 'soil water content in first layer', ibc_ws_l1, ierr=ierr)
    IF (ierr /= 0) THEN
      bc_ws_l1%ef_type = EF_MODULE
      bc_ws_l1%bc_mode = BC_REPLACE
      bc_ws_l1%ef_actual_unit = 'm'
      ibc_ws_l1 = bc_define(jg, 'soil water content in first layer', bc_ws_l1, 2, .TRUE.)
    ELSE
      CALL message(thismodule,'ws_l1 bc already defined - will use previous definition')
    END IF

    CALL bc_find(jg, 'field capacity in first soil layer', ibc_ws_fc_l1, ierr=ierr)
    IF (ierr /= 0) THEN
      bc_ws_fc_l1%ef_type = EF_MODULE
      bc_ws_fc_l1%bc_mode = BC_REPLACE
      bc_ws_fc_l1%ef_actual_unit = 'm'
      ibc_ws_fc_l1 = bc_define(jg, 'field capacity in first soil layer', bc_ws_fc_l1, 2, .TRUE.)
    ELSE
      CALL message(thismodule,'ws_fc_l1 bc already defined -  will use previous definition')
    END IF

    !$ACC ENTER DATA COPYIN(Uth, srel, srelV, su_srelV, solspe)
  END SUBROUTINE bgc_dust_init
  
!---------------------------------------------------------------------------------------------------

  SUBROUTINE bgc_dust_calc_emis(  jg, jb, jcs, jce,               & 
                                  kblks_c, kproma,                & 
                                  datetime,                       & 
                                  plsmask, pglac, psfcWind,       &
                                  pz0m_tile, pustar_tile, pflux   )
 
    USE mo_model_domain_lite,          ONLY: t_patch
    USE mtime_lite,                    ONLY: t_datetime => datetime
    USE mo_physical_constants_lite,    ONLY: grav
    USE mo_turb_vdiff_params_lite,     ONLY: ckap
    USE mo_run_config_lite,            ONLY: dtime 
    USE mo_subm_bcs_lite,              ONLY: bc_apply
    USE mo_subm_jsbach_lite,           ONLY: get_from_jsbach 
    USE mo_ham_lite_misc,         ONLY: lcheck_dust, ham_lite_check_r1d 
  
    IMPLICIT NONE

    INTEGER,  INTENT(IN)  ::  jg, jb, jcs, jce, kblks_c, kproma
    REAL(dp), INTENT(IN)  ::  plsmask     (kproma),   & ! land-sea mask (1. = land, 0. = sea/lakes)     
                              pglac       (kproma),   & ! fraction of land covered by glaciers
                              psfcWind    (kproma),   & ! 10m surface wind 
                              pz0m_tile   (kproma),   & ! surface roughness length
                              pustar_tile (kproma)      ! friction velocity 
    TYPE(t_datetime), POINTER, INTENT(IN)  :: datetime
    REAL(dp), INTENT(INOUT) :: pflux(kproma,ntrace)   
 
    INTEGER   ::  jl, jn, js,             &             
                  jk, jkk, kkmin, kkmax    
    INTEGER   ::  dn(kproma),             &   ! number of dislocated particle classes
                  dk(kproma,nclass),      &   ! dislocated particle classes
                  dust_mask(kproma)           ! grid point mask for dust emissions (0: no emission)       
    REAL(dp)  ::  rdp, rla, fdp1, fdp2,   &      
                  tmpa, tmpb, tmpc, tmpd
    REAL(dp)  ::  zrough(kproma), zustar(kproma),                               &
                  sfc_wind(kproma), fract_snow(kproma),                         &
                  wtr_soil_sl(kproma), wtr_soil_fc(kproma), wtr_soil(kproma),   &             
                  c_eff(kproma), utsc(kproma),                                  &             
                  mat_tmp(nsall,kproma),                                        & 
                  fluxtyp(kproma,nclass), fluxbin(kproma,ntrace), fluxdia(nsall,kproma,nclass)            
    
    !$ACC DATA PRESENT(plsmask, pglac, psfcWind, pz0m_tile, pustar_tile) &
    !$ACC   CREATE(dust_mask, zrough, zustar, sfc_wind, fract_snow, wtr_soil_sl) &
    !$ACC   CREATE(wtr_soil_fc, wtr_soil, c_eff, utsc, mat_tmp, fluxtyp, fluxbin, fluxdia)

    CALL get_from_jsbach(jg, jb, jcs, jce, kblks_c, kproma, ibc_ssf, "hydro_fract_snow", 2)
    CALL get_from_jsbach(jg, jb, jcs, jce, kblks_c, kproma, ibc_ws_l1, "hydro_wtr_soil_sl", 3)
    CALL get_from_jsbach(jg, jb, jcs, jce, kblks_c, kproma, ibc_ws_fc_l1, "hydro_wtr_soil_fc_sl", 3) 

    CALL bc_apply(jg, jb, jcs, jce, ibc_ssf, kblks_c, datetime, fract_snow)
    CALL bc_apply(jg, jb, jcs, jce, ibc_ws_l1, kblks_c, datetime, wtr_soil_sl)
    CALL bc_apply(jg, jb, jcs, jce, ibc_ws_fc_l1, kblks_c, datetime, wtr_soil_fc)
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR
    DO jl = jcs,jce

      IF ((plsmask(jl) .GT. 0.5_dp) .AND. (pglac(jl) .LT. 0.5_dp)) THEN     ! land surface but not glacier
        wtr_soil(jl) = MIN(wtr_soil_sl(jl) / MAX(wtr_soil_fc(jl),EPSILON(1.0_dp)), 1._dp)   !TODO: div by zero 
      ELSEIF((plsmask(jl) .GT. 0.5_dp) .AND. (pglac(jl) .GT. 0.5_dp)) THEN  ! glacier on land surface
        wtr_soil(jl) = 1._dp
        fract_snow(jl) = 1._dp
      ELSE                                                                  ! ocean  
        wtr_soil(jl) = -9.e9_dp
        fract_snow(jl) = -9.e9_dp      
      END IF

      wtr_soil(jl)    = MAX(wtr_soil(jl), 0._dp)  
      fract_snow(jl)  = MAX(fract_snow(jl), 0._dp) 
      sfc_wind(jl)    = MAX(psfcWind(jl), 0._dp) 

    END DO 
    !$ACC END LOOP
    IF (lsurficon) THEN
      !$ACC LOOP GANG VECTOR
      DO jl = jcs,jce
        zrough(jl)  = MAX(pz0m_tile(jl) * 100._dp, 0._dp)
        zustar(jl)  = MAX(pustar_tile(jl) * 100._dp, 0._dp)   
      END DO
      !$ACC END LOOP
    ELSE 
      !$ACC LOOP GANG VECTOR
      DO jl = jcs,jce
        zrough(jl)  = ndurough    
        zustar(jl)  = (ckap * sfc_wind(jl) * 100._dp) / log(ZZ / zrough(jl)) ! formula (15) in mat-95
      END DO
      !$ACC END LOOP
    END IF
    !$ACC LOOP GANG VECTOR
    DO jl = jcs,jce
      rough_diag(jl,jb) = zrough(jl)
      ustar_diag(jl,jb) = zustar(jl)
    END DO
    !$ACC END LOOP

    !$ACC LOOP GANG VECTOR
    DO jl = jcs,jce
      mat_tmp(1,jl) = mat_s1(jl,jb)  
      mat_tmp(2,jl) = mat_s2(jl,jb)  
      mat_tmp(3,jl) = mat_s3(jl,jb)  
      mat_tmp(4,jl) = mat_s4(jl,jb)  
      mat_tmp(6,jl) = mat_s6(jl,jb)  
      mat_tmp(13,jl) = mat_s13(jl,jb)  
      mat_tmp(14,jl) = mat_s14(jl,jb)  
      mat_tmp(15,jl) = mat_s15(jl,jb) 
      mat_tmp(16,jl) = mat_s16(jl,jb) 
      mat_tmp(17,jl) = mat_s17(jl,jb)
      mat_tmp(10,jl) = mat_psa(jl,jb)  
      mat_tmp(11,jl) = mat_psa(jl,jb)   
    END DO 
    !$ACC END LOOP

    !$ACC LOOP GANG VECTOR
    DO jl = jcs,jce
      utsc(jl) = 1._dp 
      utsc(jl) = MERGE(r_dust_sf13, utsc(jl), (mat_tmp(13,jl) > 0._dp))
      utsc(jl) = MERGE(r_dust_sf14, utsc(jl), (mat_tmp(14,jl) > 0._dp))
      utsc(jl) = MERGE(r_dust_sf15, utsc(jl), (mat_tmp(15,jl) > 0._dp))
      utsc(jl) = MERGE(r_dust_sf16, utsc(jl), (mat_tmp(16,jl) > 0._dp))
      utsc(jl) = MERGE(r_dust_sf17, utsc(jl), (mat_tmp(17,jl) > 0._dp))
    END DO
    !$ACC END LOOP

  
    IF (k_dust_easo .eq. 1) THEN
      !$ACC LOOP GANG VECTOR
      DO jl = jcs,jce
        mat_tmp(13,jl) = 0._dp
        mat_tmp(14,jl) = 0._dp
        mat_tmp(15,jl) = 0._dp
        mat_tmp(16,jl) = 0._dp
        mat_tmp(17,jl) = 0._dp
      END DO
      !$ACC END LOOP
    END IF
    !$ACC END PARALLEL
    ! calculate efficient friction velocity ratio, formula (17) in mat-95
    
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR PRIVATE(tmpa, tmpb, tmpc)
    DO jl = jcs,jce
     
      IF (ndurough .EQ. 0._dp) THEN
        tmpc = 0._dp 
      ELSE
        tmpa = log(ndurough / r_dust_z0s)
        tmpb = log(aeff * (xeff / r_dust_z0s)**0.8_dp)
        tmpc = 1._dp - tmpa / tmpb    
        tmpc = MAX(tmpc, 0._dp)
        tmpc = MIN(tmpc, 1._dp) 
      END IF 

      c_eff(jl) = tmpc
    
    END DO     
    !$ACC END LOOP

    !$ACC LOOP GANG VECTOR
    DO jl = jcs,jce

      dust_mask(jl)  = 0
      
      IF (c_eff(jl) .GT. 0._dp) THEN      
        IF ((k_fpar_eff(jl,jb) .GT. r_dust_lai) .AND. (zustar(jl) .GT. 0._dp) .AND. &
          (zustar(jl) .GE. r_dust_umin * nduscale_2d(jl,jb) / c_eff(jl))) THEN    
          dust_mask(jl) = 1        
        END IF   
      END IF  

    END DO 
    !$ACC END LOOP
    !$ACC END PARALLEL
   
    ! calculate horizontal flux G(Dp), formula (28) in mat-95
    

    DO jl = jcs,jce 
      dn(jl) = 0 
    END DO
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR COLLAPSE(2)
    DO jk = 1,nclass  
      DO jl = jcs,jce  
        dk(jl,jk) = 0
        fluxtyp(jl,jk) = 0._dp
      END DO
    END DO
    !$ACC END LOOP

    !$ACC LOOP GANG VECTOR COLLAPSE(3)
    DO jk = 1,nclass  
      DO jl = jcs,jce
        DO js = 1,nsall  
          fluxdia(js,jl,jk) = 0._dp  
        END DO
      END DO
    END DO
    !$ACC END LOOP
    !$ACC END PARALLEL

    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR COLLAPSE(2) PRIVATE(tmpd, fdp1, fdp2, tmpa, tmpb, tmpc, js)
    DO jk = 1,nclass  
      DO jl = jcs,jce

        IF (c_eff(jl) .GT. 0._dp .AND. dust_mask(jl) .EQ. 1) THEN   
  
          tmpd = Uth(jk) * nduscale_2d(jl,jb)
        
          fdp1 = (1._dp + (tmpd * utsc(jl) / (c_eff(jl) * zustar(jl))))**2
          fdp2 = (1._dp - (tmpd * utsc(jl) / (c_eff(jl) * zustar(jl))))  
       
          ! formula (33) in mat-95, formula (3) in tegen-2002

          tmpa = 1.00_dp * roa / (grav*100._dp) * zustar(jl)**3
 
          IF (fdp2 .gt. 0._dp) THEN
            !$ACC LOOP SEQ
            DO js = 1,nsall
              tmpb = solspe(js,nmode*3+1) 
              fluxdia(js,jl,jk) = srel(js,jk) * fdp1 * fdp2 * tmpa * tmpb 
            END DO 
          END IF 

          IF (jk .eq. 1) THEN 
              
            tmpb = 1._dp - mat_tmp(10,jl)
            tmpc =  mat_tmp(1,jl) - mat_tmp(2,jl) - mat_tmp(3,jl) -   &
                    mat_tmp(4,jl) - mat_tmp(6,jl) - mat_tmp(13,jl) -  &
                    mat_tmp(14,jl) - mat_tmp(15,jl) - mat_tmp(16,jl) - mat_tmp(17,jl) 

            fluxtyp(jl,jk) = fluxtyp(jl,jk)                   +   &
              fluxdia(1,jl,jk)   * tmpb  * tmpc               +   &
              fluxdia(2,jl,jk)   * tmpb  * mat_tmp(2,jl)      +   &
              fluxdia(3,jl,jk)   * tmpb  * mat_tmp(3,jl)      +   &
              fluxdia(4,jl,jk)   * tmpb  * mat_tmp(4,jl)      +   &
              fluxdia(6,jl,jk)   * tmpb  * mat_tmp(6,jl)      +   &
              fluxdia(13,jl,jk)  * tmpb  * mat_tmp(13,jl)     +   &
              fluxdia(14,jl,jk)  * tmpb  * mat_tmp(14,jl)     +   &
              fluxdia(15,jl,jk)  * tmpb  * mat_tmp(15,jl)     +   &
              fluxdia(16,jl,jk)  * tmpb  * mat_tmp(16,jl)     +   &
              fluxdia(17,jl,jk)  * tmpb  * mat_tmp(17,jl)     +   &
              fluxdia(10,jl,jk)  * mat_tmp(10,jl)

          ELSE

            dn(jl) = dn(jl) + 1   ! increase number of dislocated particle classes
            dk(jl,dn(jl)) = jk    ! store dislocated particle class

          ENDIF 
 
        END IF  

      END DO
      !$ACC END LOOP

    END DO 
    !$ACC END PARALLEL

    ! calculate horizontal flux distribution

    kkmin = 1

    DO jl = jcs,jce

      tmpa = 1._dp - mat_tmp(10,jl)

      tmpb =  mat_tmp(1,jl) - mat_tmp(2,jl) - mat_tmp(3,jl) -   & 
              mat_tmp(4,jl) - mat_tmp(6,jl) - mat_tmp(13,jl) -  &
              mat_tmp(14,jl) - mat_tmp(15,jl) - mat_tmp(16,jl) - mat_tmp(17,jl)

      IF (dust_mask(jl) .EQ. 1) THEN

        DO jn = 1,dn(jl) ! loop over dislocated dust particle classes

          kkmax = dk(jl,jn)

          DO jkk = 1,kkmax  ! scaling with relative contribution of dust size fraction
            DO js = 1,nsall

              IF (js == 1) THEN 
                tmpc = tmpa * tmpb * fluxdia(js,jl,kkmax)
              ELSE IF ((js == 2) .OR. (js == 3) .OR. (js == 4) .OR. (js == 6)) THEN 
                tmpc = tmpa * mat_tmp(js,jl) * fluxdia(js,jl,kkmax)
              ELSE IF ((js == 10) .AND. (sfc_wind(jl) <= 10._dp)) THEN
                tmpc = mat_tmp(js,jl) * fluxdia(10,jl,kkmax)
              ELSE IF ((js == 11) .AND. (sfc_wind(jl) > 10._dp)) THEN
                tmpc = mat_tmp(js,jl) * fluxdia(10,jl,kkmax)
              ELSE IF ((js >= 13) .AND. (js <= 17)) THEN
                tmpc = tmpa * mat_tmp(js,jl) * fluxdia(js,jl,kkmax)
              ELSE 
                CYCLE 
              END IF 

              tmpd = srelV(js,jkk) / ((su_srelV(js,kkmax) - su_srelV(js,kkmin)))
              fluxtyp(jl,jkk) = fluxtyp(jl,jkk) + tmpc * tmpd

            END DO 
          END DO 

        END DO  

      END IF  

    END DO  


    ! calculate vertical dust particle flux, formula (2) in tegen-2002

    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR COLLAPSE(2)
    DO jn = 1,ntrace
      DO jl = jcs,jce

        fluxbin(jl,jn)  = 0._dp 
        pflux(jl,jn)    = 0._dp

        IF (dust_mask(jl) .EQ. 1) THEN
 
          fluxbin(jl,jn) = fluxbin(jl,jn) + SUM(fluxtyp(jl,(jn-1)*nbin+1:jn*nbin))

          ! mask out dust fluxes, where soil moisture threshold reached
          IF (wtr_soil(jl) .GT. wrt_max) fluxbin(jl,jn) = 0._dp             

          ! convert units from g/cm2/s to kg/m2/s
          pflux(jl,jn) = fluxbin(jl,jn) * 10._dp * (1._dp - fract_snow(jl)) * k_fpar_eff(jl,jb)  
        END IF 

      END DO  
    END DO  
    !$ACC END LOOP 
    !$ACC END PARALLEL
    ! print checks 
    IF (lcheck_dust) THEN
      
      !$ACC UPDATE HOST(zrough, zustar) ASYNC(1)
      !$ACC WAIT(1)  
      CALL ham_lite_check_r1d( jb, jcs, jce, kproma, zrough, 'bgc_dust_calc_emis', 'zrough' )     
      CALL ham_lite_check_r1d( jb, jcs, jce, kproma, zustar, 'bgc_dust_calc_emis', 'zustar' )     
    END IF

    !$ACC WAIT(1)
    !$ACC END DATA
     
  END SUBROUTINE bgc_dust_calc_emis
 
!---------------------------------------------------------------------------------------------------

  SUBROUTINE shape_check_fields(kdim_icon,kdim_file,cfname,cvarname,croutine_name,cmodule_name)

    USE mo_exception_lite,             ONLY: finish

    INTEGER,INTENT(IN)            :: kdim_icon(:), kdim_file(:)
    CHARACTER(LEN=*), INTENT(IN)  :: cfname, cvarname, croutine_name, cmodule_name
    
    INTEGER                       :: idim
    CHARACTER(LEN=2)              :: cidim 
    CHARACTER(LEN=32)             :: cidim_len_file, cidim_len_icon

    IF (SIZE(kdim_icon) /= SIZE(kdim_file)) THEN
      CALL finish(TRIM(ADJUSTL(croutine_name))//' of '//TRIM(ADJUSTL(cmodule_name )),     &
        'variable '//TRIM(ADJUSTL(cvarname))//' has wrong number of dimensions in file '  &
        //TRIM(ADJUSTL(cfname)))
    END IF

    DO idim = 1,SIZE(kdim_icon)
      IF (kdim_icon(idim) /= kdim_file(idim)) THEN
        WRITE(cidim,'(i2)') idim
        WRITE(cidim_len_icon,'(i32)') kdim_icon(idim)
        WRITE(cidim_len_file,'(i32)') kdim_file(idim)
        CALL finish(TRIM(ADJUSTL(croutine_name))//' of '//TRIM(ADJUSTL(cmodule_name )),         &
          'variable '//TRIM(ADJUSTL(cvarname))//' has wrong length in dimension '               &
          //TRIM(ADJUSTL(cidim))//' length in icon model: '//TRIM(ADJUSTL(cidim_len_icon))      &
          //' but length in file '//TRIM(ADJUSTL(cfname))//' is '//TRIM(ADJUSTL(cidim_len_file)))
      END IF
    END DO

  END SUBROUTINE shape_check_fields

!---------------------------------------------------------------------------------------------------

  SUBROUTINE check_input_file_exist(caller, cfile)

!    USE mo_mpi_lite,       ONLY: my_process_is_mpi_workroot
    USE mo_exception_lite, ONLY: finish

    CHARACTER(LEN=*), INTENT(IN) :: caller, cfile

    LOGICAL :: lex    

!    IF ( my_process_is_mpi_workroot() ) THEN
      INQUIRE (file=TRIM(cfile), exist=lex)
      IF ( .NOT. lex ) THEN
        CALL finish(caller,'missing input file '//TRIM(cfile))
      END IF
!    END IF

  END SUBROUTINE check_input_file_exist

END MODULE mo_ham_lite_dust
