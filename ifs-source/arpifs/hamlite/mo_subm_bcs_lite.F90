! 
! based on mo_boundary_condition.f90 
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

MODULE mo_subm_bcs_lite

  USE mo_kind_lite,                    ONLY: dp
  !USE mo_var_list,                ONLY: t_var_list => t_var_list_ptr
  USE mo_fortran_tools_lite,           ONLY: t_ptr_2d3d
  USE mo_impl_constants_lite,          ONLY: max_dom
  USE mo_util_extfield_lite,           ONLY: external_field, EF_INACTIVE, &
                                         EF_UNDEFINED, EF_TIMERESOLVED, &
                                         EF_NOINTER, EF_INACTIVE, &
                                         EF_UNDEFINED, EF_TIMERESOLVED, &
                                         EF_NOINTER
                                         
#ifndef _OPENACC
#define __acc_attach(ptr)
#endif

  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: start_boundary_condition,   &
            ! bc_lists_init,              & 
            bc_list_read,               &
            bc_define,                  &
            bc_set,                     &
            bc_apply,                   &
            bc_query,                   &
            bc_modify,                  &     
            bc_find
            !p_bcast_bc 

  PUBLIC :: nbc, bc_nml, bc_lists

  ! flags of bc_domain
  PUBLIC :: BC_EVERYWHERE,  &  
            BC_BOTTOM,      &
            BC_TOP,         &
            BC_LEVEL,       &
            BC_ALTITUDE,    &
            BC_PRESSURE

  ! flags of bc_mode
  PUBLIC :: BC_REPLACE, &  
            BC_ADD,     &
            BC_RELAX,   &
            BC_SPECIAL                 

  ! flags of bc_vertint
  PUBLIC :: BC_VERTICAL_NONE,                   &  
            BC_VERTICAL_INTERPOLATION,          &
            BC_VERTICAL_WEIGHTED_INTERPOLATION, &
            BC_VERTICAL_INTEGRATION  

  !--------------------
  ! module parameters
  !--------------------

  INTEGER, PARAMETER :: MAXNBC = 800  

  !$ACC DECLARE COPYIN(MAXNBC)  

  ! flags of bc_domain
  INTEGER, PARAMETER :: BC_EVERYWHERE = 0
  INTEGER, PARAMETER :: BC_BOTTOM     = 1
  INTEGER, PARAMETER :: BC_TOP        = 2
  INTEGER, PARAMETER :: BC_LEVEL      = 3
  INTEGER, PARAMETER :: BC_ALTITUDE   = 4
  INTEGER, PARAMETER :: BC_PRESSURE   = 5

  !$ACC DECLARE COPYIN(BC_EVERYWHERE, BC_BOTTOM, BC_TOP) &  
  !$ACC   COPYIN(BC_LEVEL, BC_ALTITUDE, BC_PRESSURE)  

  ! flags of bc_mode
  INTEGER, PARAMETER :: BC_REPLACE  = 1
  INTEGER, PARAMETER :: BC_ADD      = 2
  INTEGER, PARAMETER :: BC_RELAX    = 3
  INTEGER, PARAMETER :: BC_SPECIAL  = 4

  !$ACC DECLARE COPYIN(BC_REPLACE, BC_ADD, BC_RELAX, BC_SPECIAL)  

  ! flags of bc_vertint
  INTEGER, PARAMETER :: BC_VERTICAL_NONE                   = 0
  INTEGER, PARAMETER :: BC_VERTICAL_INTERPOLATION          = 1
  INTEGER, PARAMETER :: BC_VERTICAL_WEIGHTED_INTERPOLATION = 2
  INTEGER, PARAMETER :: BC_VERTICAL_INTEGRATION            = 3

  !$ACC DECLARE COPYIN(BC_VERTICAL_NONE, BC_VERTICAL_INTERPOLATION) &
  !$ACC   COPYIN(BC_VERTICAL_WEIGHTED_INTERPOLATION, BC_VERTICAL_INTEGRATION)

  CHARACTER(len=*), PARAMETER :: thismodule = 'mo_subm_bcs'

  !--------------------
  ! module variables
  !--------------------

  INTEGER, SAVE :: nbc = 0

  LOGICAL, SAVE, DIMENSION(max_dom) :: lfirst = .TRUE.  
  LOGICAL, SAVE, DIMENSION(max_dom) :: called_bc_lists_init = .FALSE.

  TYPE :: bc_nml
    ! external field properties:
    ! source of the boundary condition
    INTEGER               :: ef_type = EF_INACTIVE   
                             ! possible values:
                             ! EF_INACTIVE = 0
                             ! EF_VALUE = 1 
                             ! EF_FILE = 2
                             ! EF_MODULE = 3
    ! information for type=ef_file
    ! template for filename. Example: bc_ch4.%Y4.%T0.nc
    CHARACTER(LEN=512)    :: ef_template = ''
    ! variable name in file
    CHARACTER(LEN=512)    :: ef_varname= ''
    ! geometry of the file variable
    INTEGER               :: ef_geometry = EF_UNDEFINED
                             ! possible values:
                             ! EF_UNDEFINED = 0
                             ! EF_3D        = 1
                             ! EF_LONLAT    = 2
                             ! EF_LEV       = 3
                             ! EF_SINGLE    = 4
    ! definition of the time values in the file
    INTEGER               :: ef_timedef = EF_TIMERESOLVED  
                             ! possible values:
                             ! EF_TIMERESOLVED = 1
                             ! EF_IGNOREYEAR   = 2
                             ! EF_CONSTANT     = 3
    ! offset to time values (e.g. to shift from mid-month to 1st day)
    REAL(dp)              :: ef_timeoffset = 0.0_dp  ! (time unit)
    ! fixed record number to select a specific time in the file
    INTEGER               :: ef_timeindex = 0
    ! time interpolation
    INTEGER               :: ef_interpolate = EF_NOINTER
                             ! possible values:
                             ! EF_NOINTER = 0
                             ! EF_LINEAR  = 1
                             ! EF_CUBIC   = 2
    ! scaling factor
    REAL(dp)              :: ef_factor = 1.0_dp 
    ! actual unit string in netcdf file
    CHARACTER(LEN=30)     :: ef_actual_unit = ''
    ! information for type=ef_value
    ! globally uniform boundary condition value
    REAL(dp)              :: ef_value       = 0.0_dp
    ! define application of boundary condition
    ! (vertical) domain where field should be applied
    INTEGER               :: bc_domain       = BC_EVERYWHERE
                             ! possible values:
                             ! BC_EVERYWHERE = 0
                             ! BC_BOTTOM     = 1
                             ! BC_TOP        = 2
                             ! BC_LEVEL      = 3
                             ! BC_ALTITUDE   = 4
                             ! BC_PRESSURE   = 5
    ! minimum and maximum model level where field shall be applied
    INTEGER               :: bc_minlev       = -1
    INTEGER               :: bc_maxlev       = 10000
    ! mode of application
    INTEGER               :: bc_mode         = BC_REPLACE
                             ! possible values:
                             ! BC_REPLACE  = 1
                             ! BC_ADD      = 2
                             ! BC_RELAX    = 3
                             ! BC_SPECIAL  = 4
    ! relaxation time for mode = bc_relax
    REAL(dp)              :: bc_relaxtime    = 0._dp
    ! number of vertical levels in input file
    INTEGER               :: ef_nlev = -1
  END TYPE bc_nml
 
  TYPE :: boundary_condition
    CHARACTER(LEN=128)    :: bc_name                  = '' ! just for logging interests
    TYPE(external_field)  :: bc_ef
    INTEGER               :: bc_domain                = BC_EVERYWHERE
    INTEGER               :: bc_minlev                = -1
    INTEGER               :: bc_maxlev                = -1
    INTEGER               :: bc_mode                  = BC_REPLACE
    INTEGER               :: bc_ndim                  = -1
    INTEGER               :: bc_vertint               = BC_VERTICAL_NONE
    REAL(dp)              :: bc_relaxtime             = 0._dp
    TYPE (t_ptr_2d3d)     :: bc_values_pointer
    TYPE (t_ptr_2d3d)     :: bc_values_pointer_prior
    TYPE (t_ptr_2d3d)     :: bc_values_pointer_later
    LOGICAL               :: bc_ldefined              = .false.
    LOGICAL               :: bc_lfirst                = .TRUE.
  END TYPE boundary_condition

  TYPE boundary_conditions  
     TYPE(boundary_condition) :: bc_list(MAXNBC) 
  END TYPE boundary_conditions 

  TYPE(boundary_conditions), ALLOCATABLE, TARGET :: bc_lists(:) ! shape: (n_dom)

  !TYPE (t_var_list), ALLOCATABLE :: bc_stream(:)  ! shape: (n_dom)
  !TYPE (t_var_list), ALLOCATABLE :: bc_stream_prior(:)  ! shape: (n_dom)
  !TYPE (t_var_list), ALLOCATABLE :: bc_stream_later(:)  ! shape: (n_dom)

  INTERFACE bc_set
    MODULE PROCEDURE bc_set1d
    MODULE PROCEDURE bc_set2d
  END INTERFACE

  INTERFACE bc_apply
    MODULE PROCEDURE bc_apply1d
    MODULE PROCEDURE bc_apply2d
  END INTERFACE
 
  CONTAINS

!---------------------------------------------------------------------------------------------------

  SUBROUTINE start_boundary_condition( p_patch )

    USE mo_model_domain_lite,         ONLY: t_patch   
    USE mo_impl_constants_lite,       ONLY: SUCCESS 
    USE mo_exception_lite,            ONLY: finish

    IMPLICIT NONE

    TYPE(t_patch), DIMENSION(:), INTENT(IN) :: p_patch

    INTEGER :: ndomain, ist

    ndomain = SIZE(p_patch)
    ALLOCATE( bc_lists(ndomain), STAT=ist)
    !$ACC ENTER DATA COPYIN( bc_lists(ndomain) )

    IF (ist/=SUCCESS) CALL finish(TRIM(thismodule), 'allocation of bc_lists failed')   

  END SUBROUTINE start_boundary_condition

!---------------------------------------------------------------------------------------------------
 
  ! SUBROUTINE bc_lists_init( p_patch )
    
  !   USE mo_model_domain,      ONLY: t_patch   
  !   USE mo_parallel_config,   ONLY: nproma 
  !   USE mo_impl_constants,    ONLY: SUCCESS, MAX_CHAR_LENGTH
  !   USE mo_exception,         ONLY: message, finish
  !   USE mo_util_string,       ONLY: separator 

  !   IMPLICIT NONE

  !   TYPE(t_patch), DIMENSION(:), INTENT(IN)  :: p_patch
 
  !   INTEGER :: ndomain, jg, ist, nblks, nlev 
  !   CHARACTER(LEN=MAX_CHAR_LENGTH) :: listname(3)

  !   CALL message('','')
  !   CALL message('',separator)
  !   CALL message(TRIM(thismodule), 'construction of hammoz bc started')

  !   ndomain = SIZE(p_patch)

  !   ALLOCATE( bc_stream(ndomain), bc_stream_prior(ndomain), bc_stream_later(ndomain), STAT=ist)
  !   !$ACC ENTER DATA COPYIN( bc_stream(ndomain), bc_stream_prior(ndomain), bc_stream_later(ndomain) )

  !   IF (ist /= SUCCESS) CALL finish(TRIM(thismodule), 'allocation of bc streams failed')

  !   DO jg = 1,ndomain
  !     nblks = p_patch(jg)%nblks_c
  !     nlev  = p_patch(jg)%nlev

  !     WRITE(listname(1),'(a,i2.2)') 'hammoz_boundary_condition_of_domain', jg
  !     WRITE(listname(2),'(a,i2.2)') 'hammoz_boundary_condition_prior_of_domain', jg
  !     WRITE(listname(3),'(a,i2.2)') 'hammoz_boundary_condition_later_of_domain', jg

  !     CALL new_bc_list( jg, nproma, nlev, nblks, listname,  &
  !                     & bc_stream(jg), bc_stream_prior(jg), &
  !                     & bc_stream_later(jg), bc_lists(jg)   )

  !   END DO

  !   called_bc_lists_init(jg) = .TRUE.
 
  !   CALL message('',separator)

  ! END SUBROUTINE bc_lists_init

!---------------------------------------------------------------------------------------------------

  ! SUBROUTINE new_bc_list( jg, kproma, klev, kblks, listname,              &
  !                         bc_stream, bc_stream_prior, bc_stream_later, bc )
 
  !   USE mo_master_control,        ONLY: get_my_process_name
  !   USE mo_var,                   ONLY: level_type_ml 
  !   USE mo_var_list_register,     ONLY: vlr_add
  !   USE mo_var_list,              ONLY: add_var 
  !   USE mo_cf_convention,         ONLY: t_cf_var
  !   USE mo_grib2,                 ONLY: t_grib2_var, grib2_var
  !   USE mo_cdi,                   ONLY: DATATYPE_PACK16, DATATYPE_PACK24,  &
  !                                       DATATYPE_FLT32,  DATATYPE_FLT64,   &
  !                                       GRID_UNSTRUCTURED,                 &
  !                                       TSTEP_INSTANT, TSTEP_AVG 
  !   USE mo_cdi_constants,         ONLY: GRID_UNSTRUCTURED_CELL, GRID_CELL
  !   USE mo_zaxis_type,            ONLY: ZA_HYBRID, ZA_HYBRID_HALF, ZA_SURFACE
  !   USE mo_io_config,             ONLY: lnetcdf_flt64_output
  !   USE mo_util_extfield,         ONLY: EF_FILE, EF_VALUE, EF_CONSTANT

  !   INTEGER, INTENT(IN) :: jg ! grid index
  !   INTEGER, INTENT(IN) :: kproma, klev, kblks ! dimensions
  !   CHARACTER(len=*), INTENT(IN) :: listname(3)
  !   TYPE(t_var_list),  INTENT(INOUT) :: bc_stream, bc_stream_prior, bc_stream_later
  !   TYPE(boundary_conditions), INTENT(INOUT) :: bc
 
  !   INTEGER :: ibits, iextbits
  !   INTEGER :: datatype_flt
  !   INTEGER :: shape2d(2), shape3d(3)
  !   INTEGER :: jbc
  !   CHARACTER(len=5)  :: cbc = "bc000"
  !   CHARACTER(:), ALLOCATABLE :: model_type
  !   TYPE(t_cf_var)    :: cf_desc
  !   TYPE(t_grib2_var) :: grib2_desc
 
  !   ibits = DATATYPE_PACK16
  !   iextbits = DATATYPE_PACK24

  !   IF ( lnetcdf_flt64_output ) THEN
  !     datatype_flt = DATATYPE_FLT64
  !   ELSE
  !     datatype_flt = DATATYPE_FLT32
  !   END IF

  !   model_type = get_my_process_name()

  !   CALL vlr_add( bc_stream, TRIM(listname(1)), patch_id=jg,          &
  !                 vlevel_type=level_type_ml, model_type=model_type    )
  !   CALL vlr_add( bc_stream_prior, TRIM(listname(2)), patch_id=jg,    &
  !                 vlevel_type=level_type_ml, model_type=model_type    )
  !   CALL vlr_add( bc_stream_later, TRIM(listname(3)), patch_id=jg,    &
  !                 vlevel_type=level_type_ml, model_type=model_type    )

  !   DO jbc = 1,nbc

  !     IF ( bc%bc_list(jbc)%bc_lfirst ) THEN

  !       write(cbc(3:5),'(i3.3)') jbc

  !       IF (bc%bc_list(jbc)%bc_ndim == 2) THEN ! time interpolation is only done for type EF_FILE (ef_interpolate)

  !         shape2d  = (/ kproma, kblks /)

  !         cf_desc    = t_cf_var(cbc, 'unknown', TRIM(bc%bc_list(jbc)%bc_name), datatype_flt)
  !         grib2_desc = grib2_var(255, 255, 255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
  !         CALL add_var( bc_stream, cbc,  bc%bc_list(jbc)%bc_values_pointer%p_2d,                    &
  !                     & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc, ldims=shape2d,     &
  !                     & lrestart=.FALSE., loutput=.FALSE., lopenacc=.TRUE. )
  !         __acc_attach(bc%bc_list(jbc)%bc_values_pointer%p_2d)
                              
  !         IF ( bc%bc_list(jbc)%bc_ef%ef_type == EF_FILE .AND. bc%bc_list(jbc)%bc_ef%ef_interpolate /= EF_NOINTER) THEN

  !           CALL add_var( bc_stream_prior, cbc,  bc%bc_list(jbc)%bc_values_pointer_prior%p_2d,      &
  !                       & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc, ldims=shape2d,   &
  !                       & lrestart=.FALSE., loutput=.FALSE., lopenacc=.TRUE. )
  !           __acc_attach(bc%bc_list(jbc)%bc_values_pointer_prior%p_2d)
  !           CALL add_var( bc_stream_later, cbc,  bc%bc_list(jbc)%bc_values_pointer_later%p_2d,      &
  !                       & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc, ldims=shape2d,   &
  !                       & lrestart=.FALSE., loutput=.FALSE., lopenacc=.TRUE. )
  !           __acc_attach(bc%bc_list(jbc)%bc_values_pointer_later%p_2d)

  !         END IF

  !       ELSE
        
  !         shape3d  = (/kproma, bc%bc_list(jbc)%bc_ef%ef_nzval , kblks/)

  !         cf_desc    = t_cf_var(cbc, 'unknown', TRIM(bc%bc_list(jbc)%bc_name), datatype_flt)
  !         grib2_desc = grib2_var(255, 255, 255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
  !         CALL add_var( bc_stream, cbc, bc%bc_list(jbc)%bc_values_pointer%p_3d,                     &
  !                     & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc, ldims=shape3d,      &
  !                     & lrestart=.FALSE., loutput=.FALSE., lopenacc=.TRUE. )
  !         __acc_attach(bc%bc_list(jbc)%bc_values_pointer%p_3d)

  !         IF ( bc%bc_list(jbc)%bc_ef%ef_type == EF_FILE .AND. bc%bc_list(jbc)%bc_ef%ef_interpolate /= EF_NOINTER) THEN

  !           CALL add_var( bc_stream_prior, cbc, bc%bc_list(jbc)%bc_values_pointer_prior%p_3d,       &
  !                       & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc, ldims=shape3d,    &
  !                       & lrestart=.FALSE., loutput=.FALSE., lopenacc=.TRUE. )
  !           __acc_attach(bc%bc_list(jbc)%bc_values_pointer_prior%p_3d)

  !           CALL add_var( bc_stream_later, cbc, bc%bc_list(jbc)%bc_values_pointer_later%p_3d,       &
  !                       & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc, ldims=shape3d,    &
  !                       & lrestart=.FALSE., loutput=.FALSE., lopenacc=.TRUE. )
  !           __acc_attach(bc%bc_list(jbc)%bc_values_pointer_later%p_3d)

  !            END IF

  !       END IF

  !       bc%bc_list(jbc)%bc_lfirst = .FALSE.

  !     END IF

  !   END DO  

  ! END SUBROUTINE new_bc_list

!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE bc_list_read( jg, p_patch, current_date )
   
    USE mo_exception_lite,         ONLY: finish 
    USE mtime_lite,                ONLY: datetime!, OPERATOR(==), OPERATOR(>)
    USE mo_model_domain_lite,      ONLY: t_patch 
    USE mo_util_extfield_lite,     ONLY: EF_FILE, EF_CONSTANT, ef_get_first_timestep!, ef_get_next_timestep

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: jg 
    TYPE(t_patch), INTENT(IN) :: p_patch    
    TYPE(datetime),  POINTER, INTENT(IN) :: current_date

    INTEGER :: jbc, ntimes, ntimes_prior
    LOGICAL :: lread
    TYPE(boundary_conditions), POINTER :: bc 
    TYPE(datetime) :: znexttime 

    bc=>bc_lists(jg)

    DO jbc = 1,nbc

      IF ( bc%bc_list(jbc)%bc_ef%ef_type == EF_FILE ) THEN 

        lread = .FALSE.

        IF (lfirst(jg)) CALL ef_get_first_timestep(current_date, jg, p_patch, bc%bc_list(jbc)%bc_ef)

        IF (bc%bc_list(jbc)%bc_ef%ef_timeindex /= 0) THEN

          !znexttime = bc%bc_list(jbc)%bc_ef%ef_times(bc%bc_list(jbc)%bc_ef%ef_timeindex)%datetime
          IF (lfirst(jg)) THEN !.OR. ((current_date > znexttime) .OR. (current_date == znexttime))) THEN
            lread = .TRUE.
            CALL bc_in_geom(jg ,p_patch, bc, jbc)
            IF (bc%bc_list(jbc)%bc_ef%ef_timedef /= EF_CONSTANT) THEN
              IF (bc%bc_list(jbc)%bc_ef%ef_interpolate /= EF_NOINTER) THEN
                bc%bc_list(jbc)%bc_ef%ef_timeindex_prior = bc%bc_list(jbc)%bc_ef%ef_timeindex
                ntimes_prior = size(bc%bc_list(jbc)%bc_ef%ef_times)
                IF (ALLOCATED(bc%bc_list(jbc)%bc_ef%ef_times_prior))  &
                  DEALLOCATE(bc%bc_list(jbc)%bc_ef%ef_times_prior)
                ALLOCATE(bc%bc_list(jbc)%bc_ef%ef_times_prior(ntimes_prior))
                bc%bc_list(jbc)%bc_ef%ef_times_prior = bc%bc_list(jbc)%bc_ef%ef_times
              ENDIF
              !CALL ef_get_next_timestep(current_date, jg, p_patch, bc%bc_list(jbc)%bc_ef)
              ntimes = size(bc%bc_list(jbc)%bc_ef%ef_times)
            ENDIF
          ENDIF

        ENDIF 

        bc%bc_list(jbc)%bc_ldefined = .true.  

      END IF

    END DO

    lfirst(jg) = .FALSE.

  END SUBROUTINE bc_list_read

!---------------------------------------------------------------------------------------------------

  SUBROUTINE bc_in_geom( jg, p_patch, bc, jbc )

    USE mo_model_domain_lite,    ONLY: t_patch
    USE mo_util_extfield_lite,   ONLY: EF_SINGLE, EF_LEV, EF_LONLAT, EF_3D   

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: jg   
    TYPE(t_patch), INTENT(IN) :: p_patch 
    TYPE(boundary_conditions), POINTER, INTENT(INOUT) :: bc

    INTEGER, INTENT(IN) :: jbc
    REAL(dp) :: value
    LOGICAL :: lispressure, llevel

    SELECT CASE(bc%bc_list(jbc)%bc_ef%ef_geometry)

      CASE (EF_SINGLE)
        CALL bc_read_0d(bc, jbc)
      CASE (EF_LEV)
        stop 554
      CASE (EF_LONLAT)
        CALL bc_read_2d (p_patch, jbc, bc) 
        !CALL bc_read_0d(bc, jbc)  
      CASE (EF_3D)
        CALL bc_read_3d (jg, p_patch, jbc, bc, lispressure, llevel) 
        IF (llevel) THEN
          bc%bc_list(jbc)%bc_domain = BC_LEVEL
          bc%bc_list(jbc)%bc_minlev = 1
          bc%bc_list(jbc)%bc_maxlev = p_patch%nlev
        ELSE
          IF (bc%bc_list(jbc)%bc_domain == BC_EVERYWHERE) THEN
              bc%bc_list(jbc)%bc_minlev = 1
              bc%bc_list(jbc)%bc_maxlev = p_patch%nlev
          ENDIF
          IF (lispressure) THEN
            bc%bc_list(jbc)%bc_domain = BC_PRESSURE
          ELSE
            bc%bc_list(jbc)%bc_domain = BC_ALTITUDE
          ENDIF
        ENDIF

    END SELECT

  END SUBROUTINE bc_in_geom

!---------------------------------------------------------------------------------------------------

  SUBROUTINE bc_read_0d( bc, jbc )

!    USE mo_mpi_lite,             ONLY: my_process_is_mpi_workroot, p_pe, p_io, p_bcast, p_comm_work
    USE mo_util_extfield_lite,   ONLY: ef_read_single

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: jbc
    REAL(dp) :: val  
    TYPE(boundary_conditions), POINTER  :: bc

!    IF ( p_pe == p_io ) THEN
      CALL ef_read_single (bc%bc_list(jbc)%bc_ef, val)
!    END IF

!    CALL p_bcast(val, p_io, p_comm_work)

    SELECT CASE(bc%bc_list(jbc)%bc_domain)
      CASE (BC_BOTTOM, BC_TOP)
        bc%bc_list(jbc)%bc_values_pointer%p_2d(:,:) = val
      CASE (BC_LEVEL)
        bc%bc_list(jbc)%bc_values_pointer%p_3d(:,:,:) = val  
    END SELECT

    !$ACC UPDATE DEVICE(bc%bc_list(jbc)%bc_values_pointer%p_2d) &
    !$ACC   DEVICE(bc%bc_list(jbc)%bc_values_pointer%p_3d) ASYNC(1)
    !$ACC WAIT(1)

  END SUBROUTINE bc_read_0d

! !---------------------------------------------------------------------------------------------------

  SUBROUTINE bc_read_2d( p_patch, jbc, bc )

    !eehol: This routine is modified to apply some BC to the boxmodel...

    USE mo_model_domain_lite,      ONLY: t_patch
    USE mo_kind_lite,              ONLY: dp
    USE mo_exception_lite,         ONLY: finish, message
    !USE mo_read_interface,    ONLY: openInputFile, closeFile, read_2D_time, t_stream_id, on_cells
    !USE mo_io_config,         ONLY: default_read_method
    USE mo_util_netcdf77_lite,     ONLY: read_var_hs_nf77_2d
    USE mo_util_extfield_lite,     ONLY: EF_LONLAT

    IMPLICIT NONE

    INTEGER, INTENT (IN) :: jbc
    TYPE(t_patch), INTENT(IN) :: p_patch 
    TYPE (boundary_conditions), POINTER,  INTENT(INOUT) :: bc
  
    !REAL(dp), POINTER :: p3d(:,:,:)
    INTEGER                             :: ierr
    REAL(dp), POINTER :: p2d(:,:)
    TYPE (external_field) :: efield
    !TYPE (t_stream_id) :: stream_id

    efield = bc%bc_list(jbc)%bc_ef

    !eehol: Allocate your pointer based on dimension sizes (NEED TO BE SAME AS INCOMING FILE!!!! JUST HARDCODED FOR NOW!!)
    ALLOCATE(p2d(2, 2))

    IF (.NOT. ASSOCIATED(bc%bc_list(jbc)%bc_values_pointer%p_2d)) THEN
      ALLOCATE(bc%bc_list(jbc)%bc_values_pointer%p_2d(SIZE(p2d,1), SIZE(p2d,2)))
    ENDIF


    IF (efield%ef_geometry .eq. EF_LONLAT) THEN
      !CALL openInputFile(stream_id, efield%ef_file, p_patch, default_read_method)      
      !CALL read_2D_time(  stream_id=stream_id, location=on_cells, &
      !                  & variable_name=trim(efield%ef_varname),  &
      !                  & return_pointer=p3d,                     &
      !                  & start_timestep=efield%ef_timeindex,     &
      !                  & end_timestep=efield%ef_timeindex        )
      call read_var_hs_nf77_2d(efield%ef_file, "lat", "lon", "time", efield%ef_timeindex, trim(efield%ef_varname), p2d, ierr)
      !value=value*efield%ef_factor
      bc%bc_list(jbc)%bc_values_pointer%p_2d(:,:)=p2d(:,:)
      DEALLOCATE(p2d)
      !CALL closeFile(stream_id)

    ELSE
 
      CALL finish(thismodule, "bc_read_2d - unsupported geometry")

    ENDIF

    bc%bc_list(jbc)%bc_values_pointer%p_2d(:,:) = &
      bc%bc_list(jbc)%bc_values_pointer%p_2d(:,:) * efield%ef_factor

    CALL message('ef_read_2d', 'new 2d-field read for variable ' // TRIM(efield%ef_varname) &
                // ' from file ' // TRIM(efield%ef_file))

    !$ACC UPDATE DEVICE(bc%bc_list(jbc)%bc_values_pointer%p_2d) ASYNC(1)
    !$ACC WAIT(1)

  END SUBROUTINE bc_read_2d

! !---------------------------------------------------------------------------------------------------

  SUBROUTINE bc_read_3d( jg, p_patch, jbc, bc, lispressure, llevel )

    !eehol: This routine is modified to apply some BC to the boxmodel...

    USE mo_model_domain_lite,      ONLY: t_patch
    USE mo_kind_lite,              ONLY: dp
    USE mo_exception_lite,         ONLY: finish, message, message_text
    !USE mo_read_interface,    ONLY: openInputFile, closeFile, read_3D_time, t_stream_id, on_cells
    !USE mo_io_config,         ONLY: default_read_method
    USE mo_util_extfield_lite,     ONLY: EF_3D!ef_get_levname_and_size, EF_3D,  &
                                    !ef_get_full_and_half_level_information
    USE mo_util_netcdf77_lite,     ONLY: read_var_hs_nf77_3d

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: jg, jbc
    TYPE(t_patch), INTENT(IN) :: p_patch 
    TYPE (boundary_conditions), POINTER, INTENT(INOUT) :: bc
    LOGICAL, OPTIONAL, INTENT(OUT) :: lispressure, llevel

    INTEGER :: nzval, ilev, irev
    INTEGER                             :: ierr
    !REAL(dp), POINTER :: p4d(:,:,:,:)
    REAL(dp), POINTER :: p3d(:,:,:)
    LOGICAL :: lreverse, lpres, llev
    CHARACTER(LEN=4)  :: cnzval
    CHARACTER(LEN=5)  :: clevname
    !TYPE(t_stream_id) :: stream_id
    TYPE(external_field), POINTER :: efield 

    efield => bc%bc_list(jbc)%bc_ef

    !eehol: Allocate your pointer based on dimension sizes (NEED TO BE SAME AS INCOMING FILE!!!! JUST HARDCODED FOR NOW!!)
    ALLOCATE(p3d(25, 2, 2))

    IF (.NOT. ASSOCIATED(bc%bc_list(jbc)%bc_values_pointer%p_3d)) THEN
      ALLOCATE(bc%bc_list(jbc)%bc_values_pointer%p_3d(SIZE(p3d,1), SIZE(p3d,2), SIZE(p3d,3)))
    ENDIF
  
    !CALL ef_get_levname_and_size(efield%ef_file,clevname,nzval)

    ! IF ( bc%bc_list(jbc)%bc_ef%ef_nzval /= nzval ) THEN

    !   WRITE(cnzval,FMT='(I3)') nzval
    !   message_text="Specify bc%ef_nlev (or nlev in emi_spec.txt) for reading "//efield%ef_file//"  "//cnzval
    !   CALL finish(thismodule, message_text)

    ! END IF 

    IF (efield%ef_geometry .eq. EF_3D) THEN

      !CALL openInputFile(stream_id, efield%ef_file, p_patch, default_read_method)
      !CALL read_3D_time(  stream_id=stream_id, location=on_cells, &
      !                  & variable_name=trim(efield%ef_varname),  &
      !                  & return_pointer=p4d,                     &
      !                  & start_timestep=efield%ef_timeindex,     &
      !                  & end_timestep=efield%ef_timeindex        )
      !CALL closeFile(stream_id)
      call read_var_hs_nf77_3d(efield%ef_file, "lev", "lat", "lon", "time", efield%ef_timeindex, trim(efield%ef_varname), p3d, ierr)
      p3d(:,:,:) = efield%ef_factor * p3d(:,:,:)
      !CALL ef_get_full_and_half_level_information(jg, p_patch, efield, nzval, clevname, lreverse, lpres, llev)

      !WRITE(*,*) 'lreverse =', lreverse

      ! IF (lreverse) THEN
      !   DO ilev = 1,nzval
      !     irev = nzval - ilev + 1
      !     !bc%bc_list(jbc)%bc_values_pointer%p_3d(:,ilev,:) = p4d(:,irev,:,1)
      !     bc%bc_list(jbc)%bc_values_pointer%p_3d(:,ilev,:) = p3d(:,irev,:)
      !   END DO
      ! ELSE
        !bc%bc_list(jbc)%bc_values_pointer%p_3d(:,:,:) = p4d(:,:,:,1)
        bc%bc_list(jbc)%bc_values_pointer%p_3d(:,:,:) = p3d(:,:,:)
      !END IF

      DEALLOCATE(p3d)

    ELSE

      CALL finish(thismodule, "bc_read_3d - unsupported geometry")

    ENDIF
  
    IF ( PRESENT(lispressure) ) lispressure = lpres
    IF ( PRESENT(llevel) )      llevel = llev 

    CALL message('ef_read_3d', 'new 3d-field read for variable ' // TRIM(efield%ef_varname) &
                // ' from file ' // TRIM(efield%ef_file))

    !$ACC UPDATE DEVICE(bc%bc_list(jbc)%bc_values_pointer%p_3d) ASYNC(1)
    !$ACC WAIT(1)

  END SUBROUTINE bc_read_3d

!---------------------------------------------------------------------------------------------------
 
!  SUBROUTINE p_bcast_bc ( bc_struc, p_source, comm )
!
!    USE mo_mpi_lite, ONLY: p_bcast, p_comm_work    
!
!    IMPLICIT NONE
!
!    INTEGER, INTENT(IN) :: p_source
!    INTEGER, OPTIONAL, INTENT(IN) :: comm
!    TYPE(bc_nml), INTENT(INOUT) :: bc_struc
!
!    INTEGER :: p_comm
!
!    IF (PRESENT(comm)) THEN
!      p_comm = comm
!    ELSE
!      p_comm = p_comm_work
!    ENDIF
!
!    CALL p_bcast(bc_struc%ef_type,        p_source, p_comm)
!    CALL p_bcast(bc_struc%ef_template,    p_source, p_comm)
!    CALL p_bcast(bc_struc%ef_varname,     p_source, p_comm)
!    CALL p_bcast(bc_struc%ef_geometry,    p_source, p_comm)
!    CALL p_bcast(bc_struc%ef_timedef,     p_source, p_comm)
!    CALL p_bcast(bc_struc%ef_timeoffset,  p_source, p_comm)
!    CALL p_bcast(bc_struc%ef_timeindex,   p_source, p_comm)
!    CALL p_bcast(bc_struc%ef_interpolate, p_source, p_comm)
!    CALL p_bcast(bc_struc%ef_value,       p_source, p_comm)
!    CALL p_bcast(bc_struc%ef_factor,      p_source, p_comm)
!    CALL p_bcast(bc_struc%ef_actual_unit, p_source, p_comm)
!    CALL p_bcast(bc_struc%bc_domain,      p_source, p_comm)
!    CALL p_bcast(bc_struc%bc_mode,        p_source, p_comm)
!    CALL p_bcast(bc_struc%bc_maxlev,      p_source, p_comm)
!    CALL p_bcast(bc_struc%bc_minlev,      p_source, p_comm)

!  END SUBROUTINE p_bcast_bc

!---------------------------------------------------------------------------------------------------

  SUBROUTINE bc_printstat ( bc, out )
      
    USE mo_io_units_lite,        ONLY: nerr
    USE mo_exception_lite,       ONLY: message, message_text
    USE mo_util_string_lite,     ONLY: separator   
    USE mo_util_extfield_lite,   ONLY: EF_INACTIVE, EF_VALUE, EF_FILE, EF_MODULE, &
                                  EF_UNDEFINED, EF_3D, EF_LONLAT, EF_LEV, EF_SINGLE, &
                                  EF_TIMERESOLVED, EF_IGNOREYEAR, EF_CONSTANT, &
                                  EF_LINEAR, EF_CUBIC

    IMPLICIT NONE

    INTEGER, OPTIONAL, INTENT(IN) :: out  
    TYPE(boundary_condition), INTENT(IN) :: bc
 
    CHARACTER(LEN=24) :: cc, cc2
 
    CALL message('','')
    CALL message('',separator)
    WRITE (message_text, '(a)') 'Boundary condition:'
    CALL message('',message_text)

    SELECT CASE (bc%bc_mode)
      CASE (BC_REPLACE)
        cc = ' BC_REPLACE'
      CASE (BC_ADD)
        cc = ' BC_ADD'
      CASE (BC_RELAX)
        cc = ' BC_RELAX'
      CASE (BC_SPECIAL)
        cc = ' BC_SPECIAL'
    END SELECT

    WRITE (message_text, '(3a,i1,2a)') 'name: ',TRIM(bc%bc_name),', ndims: ',bc%bc_ndim,', mode: ',TRIM(cc)
    CALL message('',message_text)

    SELECT CASE (bc%bc_domain)
      CASE (BC_EVERYWHERE)
        cc = ' BC_EVERYWHERE'
      CASE (BC_BOTTOM)
        cc = ' BC_BOTTOM'
      CASE (BC_TOP)
        cc = ' BC_TOP'
      CASE (BC_LEVEL)
        cc = ' BC_LEVEL'
      CASE (BC_ALTITUDE)
        cc = ' BC_ALTITUDE'
      CASE (BC_PRESSURE)
        cc = ' BC_PRESSURE'
    END SELECT

    WRITE (message_text, '(a,L1,3a,2i4)') 'ldefined: ',bc%bc_ldefined, ', domain: ',TRIM(cc),      &
                  ' minlev/maxlev : ',bc%bc_minlev,bc%bc_maxlev
    CALL message('',message_text)
    
    IF (bc%bc_mode == BC_RELAX) THEN
      WRITE (message_text, '(a,E11.4)') 'relaxtime:', bc%bc_relaxtime
      CALL message('',message_text)
    ENDIF

    SELECT CASE (bc%bc_ef%ef_type)
      CASE (EF_INACTIVE) 
        cc = ' EF_INACTIVE'
      CASE (EF_VALUE)    
        cc = ' EF_VALUE'
      CASE (EF_FILE)     
        cc = ' EF_FILE'
      CASE (EF_MODULE)   
        cc = ' EF_MODULE'
    END SELECT

    WRITE (message_text, '(2a)') 'external field type:', cc
    CALL message('',message_text)

    IF (bc%bc_ef%ef_type == EF_FILE) THEN

      WRITE (message_text, '(2a)') 'ef_template: ', TRIM(bc%bc_ef%ef_template)
      CALL message('',message_text)
      WRITE (message_text, '(5a,e11.4)') 'ef_varname:  ', TRIM(bc%bc_ef%ef_varname),   &
                    ', units: ', TRIM(bc%bc_ef%ef_actual_unit),            &
                    ', conversion factor : ',bc%bc_ef%ef_factor
      CALL message('',message_text)

      SELECT CASE (bc%bc_ef%ef_geometry)
        CASE (EF_UNDEFINED)
          cc = ' EF_UNDEFINED'
        CASE (EF_3D)
          cc = ' EF_3D'
        CASE (EF_LONLAT)
          cc = ' EF_LONLAT'
        CASE (EF_LEV)
          cc = ' EF_LEV'
        CASE (EF_SINGLE)
          cc = ' EF_SINGLE'
      END SELECT

      WRITE (message_text, '(2a)') 'ef_geometry: ', cc
      CALL message('',message_text)

      SELECT CASE (bc%bc_ef%ef_timedef)
        CASE (EF_TIMERESOLVED)
          cc = ' EF_TIMERESOLVED'
        CASE (EF_IGNOREYEAR)
          cc = ' EF_IGNOREYEAR'
        CASE (EF_CONSTANT)
          cc = ' EF_CONSTANT'
      END SELECT

      SELECT CASE (bc%bc_ef%ef_interpolate)
        CASE (EF_NOINTER)
          cc2 = ' EF_NOINTER'
        CASE (EF_LINEAR)
          cc2 = ' EF_LINEAR'
        CASE (EF_CUBIC)
          cc2 = ' EF_CUBIC'
      END SELECT

      WRITE (message_text, '(3a,e11.4,a,i4,2a)') 'ef_timedef: ', TRIM(cc), ', ef_timeoffset: ',bc%bc_ef%ef_timeoffset,  &
                    ', ef_timeindex: ', bc%bc_ef%ef_timeindex, ', time interpolation: ', TRIM(cc2)
      CALL message('',message_text)

    ELSE IF (bc%bc_ef%ef_type == EF_VALUE) THEN

      WRITE (message_text, '(a,e11.4)') 'constant BC value: ', bc%bc_ef%ef_value
      CALL message('',message_text)

    END IF

    CALL message('','')

  END SUBROUTINE bc_printstat

! !---------------------------------------------------------------------------------------------------

  SUBROUTINE bc_find ( jg, name, ibc, ierr )

    USE mo_exception_lite,     ONLY: finish, message, message_text
    USE mo_util_string_lite,   ONLY: tolower

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: jg  
    CHARACTER(len=*), INTENT(IN) :: name
    INTEGER, INTENT(OUT) :: ibc ! index in bc_list
    INTEGER, OPTIONAL, INTENT(OUT) :: ierr ! error status

    INTEGER :: idx
    TYPE(boundary_conditions), POINTER :: bc

    bc => bc_lists(jg)
  
    ibc = 0
    IF (PRESENT(ierr)) ierr = 0 
    IF (TRIM(name) == '') CALL finish('bc_find', 'invalid (empty) name string')

    DO idx = 1,nbc
      IF (TRIM(tolower(name)) == TRIM(tolower(bc%bc_list(idx)%bc_name))) ibc = idx
    END DO

    IF (ibc > 0) THEN
      WRITE(message_text,'(a,i4)') 'located boundary condition for "'//TRIM(name)//'" as index ', ibc
      CALL message('bc_find', message_text)
    ELSE
      IF (PRESENT(ierr)) THEN
        ierr = 1
      ELSE
        CALL message('bc_find', 'cannot find index for boundary condition '//TRIM(name))
      END IF
    END IF

  END SUBROUTINE bc_find

! !---------------------------------------------------------------------------------------------------

  SUBROUTINE bc_query ( jg, ibc, name, ef_type, ef_actual_unit )

    USE mo_exception_lite,   ONLY: finish

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: jg   
    INTEGER, INTENT(IN) :: ibc    
    INTEGER, OPTIONAL, INTENT(OUT) :: ef_type
    CHARACTER(len=*), OPTIONAL, INTENT(OUT) :: name
    CHARACTER(len=*), OPTIONAL, INTENT(OUT) :: ef_actual_unit

    TYPE(boundary_conditions), POINTER :: bc

    bc => bc_lists(jg)
 
    IF (ibc < 0 .OR. ibc > nbc) CALL finish('bc_query', 'invalid index to bc_list')

    IF (PRESENT(name))            name = bc%bc_list(ibc)%bc_name
    IF (PRESENT(ef_type))         ef_type = bc%bc_list(ibc)%bc_ef%ef_type
    IF (PRESENT(ef_actual_unit))  ef_actual_unit = bc%bc_list(ibc)%bc_ef%ef_actual_unit

  END SUBROUTINE bc_query

! !---------------------------------------------------------------------------------------------------

  SUBROUTINE bc_modify ( jg, ibc, name, ef_type, bc_domain, bc_ndims, bc_vertint, ef_actual_unit )

    USE mo_exception_lite, ONLY: finish

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: jg  
    INTEGER, INTENT(IN) :: ibc   
    INTEGER, OPTIONAL, INTENT(IN) :: ef_type
    INTEGER, OPTIONAL, INTENT(IN) :: bc_domain
    INTEGER, OPTIONAL, INTENT(IN) :: bc_ndims
    INTEGER, OPTIONAL, INTENT(IN) :: bc_vertint
    CHARACTER(len=*), OPTIONAL, INTENT(IN) :: name
    CHARACTER(len=*), OPTIONAL, INTENT(IN) :: ef_actual_unit

    TYPE(boundary_conditions), POINTER :: bc
 
    IF (ibc < 0 .OR. ibc > nbc) CALL finish('bc_query', 'Invalid index to bc_list!')

    bc => bc_lists(jg)

    IF (PRESENT(name))            bc%bc_list(ibc)%bc_name = TRIM(name) 
    IF (PRESENT(ef_type))         bc%bc_list(ibc)%bc_ef%ef_type = ef_type      
    IF (PRESENT(bc_domain))       bc%bc_list(ibc)%bc_domain = bc_domain        
    IF (PRESENT(bc_ndims))        bc%bc_list(ibc)%bc_ndim = bc_ndims           
    IF (PRESENT(bc_vertint))      bc%bc_list(ibc)%bc_vertint = bc_vertint 
    IF (PRESENT(ef_actual_unit))  bc%bc_list(ibc)%bc_ef%ef_actual_unit = ef_actual_unit
  
  END SUBROUTINE bc_modify
 
! !---------------------------------------------------------------------------------------------------
 
  FUNCTION bc_define( jg, bc_name, bc_struc, bc_ndim, lverbose ) RESULT( ibc )

    USE mo_run_config_lite,      ONLY: nlev
!    USE mo_mpi_lite,             ONLY: p_pe, p_io
    USE mo_exception_lite,       ONLY: finish, message, message_text
    USE mo_util_extfield_lite,   ONLY: EF_LEV, EF_INACTIVE, EF_VALUE, EF_FILE, EF_MODULE

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: jg  
    INTEGER, INTENT(IN) :: bc_ndim
    CHARACTER(LEN=*), INTENT(IN) :: bc_name
    LOGICAL, OPTIONAL, INTENT(IN) :: lverbose
    TYPE(bc_nml), INTENT(IN) :: bc_struc 

    INTEGER :: ibc
    CHARACTER(LEN=24) :: cc
    TYPE(boundary_conditions), POINTER :: bc
  
    bc => bc_lists(jg)
  
    ibc = -1

    IF ( called_bc_lists_init(jg) ) CALL finish(thismodule, "bc_define must be called before bc_lists_init")

    SELECT CASE (bc_struc%ef_type)
      CASE (EF_INACTIVE) 
        cc = ' EF_INACTIVE'
      CASE (EF_VALUE)    
        cc = ' EF_VALUE'
      CASE (EF_FILE)     
        cc = ' EF_FILE'
      CASE (EF_MODULE)   
        cc = ' EF_MODULE'
      CASE DEFAULT       
        cc = ' INVALID'
        CALL message('bc_define', 'invalid value of EF_TYPE for boundary condition ' // TRIM(bc_name) //'!')
        RETURN
    END SELECT

    IF (bc_struc%ef_type == EF_INACTIVE) RETURN  

    nbc = nbc + 1

    IF (nbc > MAXNBC) THEN
      WRITE (message_text, '(a,i4,a)') 'too many boundary conditions (MAXNBC=', MAXNBC, ')'
      CALL finish('bc_define', message_text)
    END IF

    ibc = nbc
 
    IF (bc_struc%bc_mode < BC_REPLACE .OR. bc_struc%bc_mode > BC_SPECIAL) THEN
      WRITE (message_text, '(a,i2)') 'Invalid value for bc_mode : ', bc_struc%bc_mode
      CALL message('bc_define', message_text)
    END IF
 
    IF (bc_struc%bc_domain < BC_EVERYWHERE .OR. bc_struc%bc_domain > BC_PRESSURE) THEN
      WRITE (message_text, '(a,i2)') 'Invalid value for bc_domain : ', bc_struc%bc_domain
      CALL message('bc_define', message_text)
    END IF
 
    IF (bc_struc%bc_relaxtime < 0._dp) THEN
      WRITE (message_text, '(a,e11.4)') 'Invalid value for bc_relaxtime : ', bc_struc%bc_relaxtime
      CALL message('bc_define', message_text)
    END IF

    IF ((bc_struc%bc_mode == BC_RELAX) .AND. (bc_struc%bc_relaxtime == 0._dp)) &
      CALL message('bc_define', 'value for bc_relaxtime must be set if bc_mode=BC_RELAX!')

    ! settings for all ef_type values
    bc%bc_list(nbc)%bc_name               = bc_name
    bc%bc_list(nbc)%bc_ef%ef_type         = bc_struc%ef_type
    bc%bc_list(nbc)%bc_domain             = bc_struc%bc_domain
    bc%bc_list(nbc)%bc_minlev             = MAX(bc_struc%bc_minlev, 1)  
    bc%bc_list(nbc)%bc_maxlev             = MIN(bc_struc%bc_maxlev, nlev)
    IF (bc%bc_list(nbc)%bc_maxlev < 1) bc%bc_list(nbc)%bc_maxlev = nlev   
    bc%bc_list(nbc)%bc_mode               = bc_struc%bc_mode  
    bc%bc_list(nbc)%bc_relaxtime          = bc_struc%bc_relaxtime
    IF ( bc_struc%ef_nlev .EQ. -1 .AND. bc_ndim /= 2 ) THEN
      bc%bc_list(nbc)%bc_ef%ef_nzval      = nlev 
    ELSE
      bc%bc_list(nbc)%bc_ef%ef_nzval      = bc_struc%ef_nlev  
    END IF 
    bc%bc_list(nbc)%bc_ef%ef_actual_unit  = bc_struc%ef_actual_unit

    SELECT CASE (bc_struc%ef_type)
      CASE (EF_VALUE)    
        bc%bc_list(nbc)%bc_ef%ef_value       = bc_struc%ef_value
        bc%bc_list(nbc)%bc_ldefined = .true.
        IF (bc_struc%ef_factor /= 1.0_dp) CALL message ('bc_define',  &
          ' ef_factor is set but ef_type is EF_VALUE (the factor is ignored)')
      CASE (EF_FILE)     
        bc%bc_list(nbc)%bc_ef%ef_template    = bc_struc%ef_template
        bc%bc_list(nbc)%bc_ef%ef_varname     = bc_struc%ef_varname
        bc%bc_list(nbc)%bc_ef%ef_geometry    = bc_struc%ef_geometry
        bc%bc_list(nbc)%bc_ef%ef_timedef     = bc_struc%ef_timedef
        bc%bc_list(nbc)%bc_ef%ef_timeoffset  = bc_struc%ef_timeoffset
        bc%bc_list(nbc)%bc_ef%ef_timeindex   = bc_struc%ef_timeindex
        bc%bc_list(nbc)%bc_ef%ef_interpolate = bc_struc%ef_interpolate
        bc%bc_list(nbc)%bc_ef%ef_factor      = bc_struc%ef_factor
        IF ( ( ((bc_struc%bc_domain == BC_BOTTOM) .OR. (bc_struc%bc_domain == BC_TOP)) &
            .AND. ((bc_struc%ef_geometry == EF_LEV) ) ) .OR. ( (bc_struc%bc_domain == BC_LEVEL) ) ) &
          CALL message ('bc_define', ' bc_domain and ef_geometry do not fit!')
      CASE (EF_MODULE)   
    END SELECT

    bc%bc_list(nbc)%bc_ndim = bc_ndim

    IF (PRESENT(lverbose)) THEN
!      IF (lverbose .AND. p_pe .EQ. p_io  ) CALL bc_printstat(bc%bc_list(nbc))
    ENDIF

  END FUNCTION bc_define

! !---------------------------------------------------------------------------------------------------
 
  SUBROUTINE bc_set1d( jg, jb, jcs, jce, jbc, kblks_c, kproma, values )

    USE mo_util_extfield_lite,   ONLY: EF_MODULE

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: jg, jb, jcs, jce, jbc, kblks_c, kproma
    REAL(dp), INTENT(IN) :: values(:)

    INTEGER :: jl
    TYPE(boundary_conditions), POINTER :: bc

    bc => bc_lists(jg)
    !$ACC DATA PRESENT(values, bc)

    IF (bc%bc_list(jbc)%bc_ef%ef_type /= EF_MODULE) RETURN

    IF (.NOT. ASSOCIATED(bc%bc_list(jbc)%bc_values_pointer%p_2d)) THEN
      ALLOCATE(bc%bc_list(jbc)%bc_values_pointer%p_2d(kproma, kblks_c))
    END IF

    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR    
    DO jl = jcs,jce
      bc%bc_list(jbc)%bc_values_pointer%p_2d(jl,jb) = values(jl)
    END DO
    !$ACC END PARALLEL LOOP 

    bc%bc_list(jbc)%bc_ldefined = .true.
  
    !$ACC WAIT(1)
    !$ACC END DATA

  END SUBROUTINE bc_set1d

! !---------------------------------------------------------------------------------------------------
 
  SUBROUTINE bc_set2d( jg, jb, jcs, jce, jbc, kblks_c, kproma, values )

    USE mo_util_extfield_lite,   ONLY: EF_MODULE

    IMPLICIT NONE

    INTEGER,  INTENT(IN) :: jg, jb, jcs, jce, jbc, kblks_c, kproma
    REAL(dp), INTENT(IN) :: values(:,:)

    INTEGER :: jl
    TYPE(boundary_conditions), POINTER :: bc

    bc=>bc_lists(jg)

    !$ACC DATA PRESENT(values, bc)
     
    IF (bc%bc_list(jbc)%bc_ef%ef_type /= EF_MODULE) RETURN
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR  
    DO jl = jcs,jce
      bc%bc_list(jbc)%bc_values_pointer%p_3d(jl,:,jb) = values(jl,:)
    END DO
    !$ACC END PARALLEL LOOP 

    bc%bc_list(jbc)%bc_ldefined = .true.
 
    !$ACC WAIT(1)
    !$ACC END DATA

  END SUBROUTINE bc_set2d

! !---------------------------------------------------------------------------------------------------
 
  SUBROUTINE bc_apply1d( jg, jb, jcs, jce, jbc, ngpblks, current_date, values )

    USE mo_run_config_lite,        ONLY: dtime  
    USE mtime_lite,                     ONLY: datetime
    USE mo_exception_lite,         ONLY: finish 
    USE mo_util_extfield_lite,     ONLY: EF_VALUE, EF_FILE
    USE mo_parallel_config_lite,   ONLY: nproma

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: jg, jb, jcs, jce, jbc, ngpblks
    TYPE(datetime), POINTER, INTENT(IN) :: current_date
    REAL(dp), INTENT(INOUT) :: values(:)

    INTEGER :: jl
    REAL(dp) :: factor
    REAL(dp), ALLOCATABLE :: bc_values(:,:) 
    TYPE(boundary_conditions), POINTER :: bc
    TYPE(datetime), POINTER :: mtime_hour

    bc => bc_lists(jg)
    !$ACC DATA PRESENT(values, bc) 

    IF ( .NOT. bc%bc_list(jbc)%bc_ldefined ) &
      CALL finish('bc_apply', TRIM(bc%bc_list(jbc)%bc_name)//' has no values defined yet')

    ALLOCATE(bc_values(1:nproma,1:ngpblks))
    !$ACC ENTER DATA CREATE(bc_values)

    IF (bc%bc_list(jbc)%bc_ef%ef_type == EF_VALUE) THEN
      bc_values = bc%bc_list(jbc)%bc_ef%ef_value
    ELSE
      IF ((bc%bc_list(jbc)%bc_ef%ef_interpolate /= EF_NOINTER) &
          .AND. (bc%bc_list(jbc)%bc_ef%ef_type == EF_FILE)) THEN
        bc_values = (1._dp - bc_timefac(jbc, bc, current_date)) * bc%bc_list(jbc)%bc_values_pointer%p_2d &
                    + bc_timefac(jbc, bc, current_date) * bc%bc_list(jbc)%bc_values_pointer_later%p_2d
      ELSE
        bc_values = bc%bc_list(jbc)%bc_values_pointer%p_2d
      ENDIF
    ENDIF

    SELECT CASE(bc%bc_list(jbc)%bc_mode)
    CASE (BC_REPLACE)
      !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
      !$ACC LOOP GANG VECTOR 
      DO jl = jcs,jce
        values(jl) = bc_values(jl,jb)
      END DO
      !$ACC END PARALLEL LOOP
    CASE (BC_ADD)
      !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
      !$ACC LOOP GANG VECTOR 
      DO jl = jcs,jce
        values(jl) = values(jl) + bc_values(jl,jb)
      END DO
      !$ACC END PARALLEL LOOP
    CASE (BC_RELAX)
      factor = exp(-(dtime/bc%bc_list(jbc)%bc_relaxtime))
      !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
      !$ACC LOOP GANG VECTOR 
      DO jl = jcs,jce
        values(jl) = (1.0_dp - factor) * values(jl) + factor * bc_values(jl,jb)
      END DO
      !$ACC END PARALLEL LOOP
    CASE (BC_SPECIAL) 
      !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
      !$ACC LOOP GANG VECTOR
      DO jl = jcs,jce
        values(jl) = bc_values(jl,jb)
      END DO
      !$ACC END PARALLEL LOOP
    END SELECT

    !$ACC WAIT(1) 
    !$ACC EXIT DATA DELETE(bc_values)  
    DEALLOCATE(bc_values)

    !$ACC END DATA

  END SUBROUTINE bc_apply1d

! !---------------------------------------------------------------------------------------------------

  FUNCTION bc_timefac( jbc, bc, current_date ) RESULT( timefac )
 
    USE mtime_lite,  ONLY: datetime!, getTotalMilliSecondsTimeDelta,      &
                      !timeDelta, newTimedelta, deallocateTimedelta, &
                      !getTimeDeltaFromDateTime
    USE mo_run_config_lite, ONLY: dtime

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: jbc
    TYPE(boundary_conditions), POINTER, INTENT(IN) :: bc
    TYPE(datetime), POINTER, INTENT(IN) :: current_date

    REAL(dp) :: timefac
    REAL(dp) :: delta_t_input,  delta_t_elapsed_since_last_input
    !TYPE(datetime), POINTER   :: ztime1, ztime2
    !TYPE(timeDelta), POINTER  :: delta_mtime_input,  delta_mtime_elapsed_since_last_input

    ! delta_mtime_input => newTimedelta("PT0S")
    ! delta_mtime_elapsed_since_last_input => newTimedelta("PT0S")

    ! ztime1 => bc%bc_list(jbc)%bc_ef%ef_times_prior(bc%bc_list(jbc)%bc_ef%ef_timeindex_prior)%datetime
    ! ztime2 => bc%bc_list(jbc)%bc_ef%ef_times(bc%bc_list(jbc)%bc_ef%ef_timeindex)%datetime

    ! delta_mtime_input = getTimeDeltaFromDateTime(ztime2, ztime1) 
    ! delta_mtime_elapsed_since_last_input=getTimeDeltaFromDateTime(current_date, ztime1) 

    ! delta_t_input = getTotalMilliSecondsTimeDelta(delta_mtime_input, current_date)
    ! delta_t_elapsed_since_last_input = getTotalMilliSecondsTimeDelta(delta_mtime_elapsed_since_last_input, current_date)

    ! CALL deallocateTimedelta(delta_mtime_input)
    ! CALL deallocateTimedelta(delta_mtime_elapsed_since_last_input)

    delta_t_input = dtime !1800._dp
    delta_t_elapsed_since_last_input = dtime !1800._dp

    timefac = delta_t_elapsed_since_last_input / delta_t_input

  END FUNCTION bc_timefac

! !---------------------------------------------------------------------------------------------------
 
  SUBROUTINE bc_apply2d(  jg, jb, jcs, jce, jbc, ngpblks, current_date,   &
                          paphm1, pzf, pzh, values, minlev, maxlev        )
                         
    USE mo_run_config_lite,        ONLY: dtime
    USE mtime_lite,                     ONLY: datetime
    USE mo_exception_lite,         ONLY: finish
    USE mo_run_config_lite,        ONLY: nlev, nlevp1
    USE mo_parallel_config_lite,   ONLY: nproma
    USE mo_util_extfield_lite,     ONLY: EF_VALUE, EF_FILE
  
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: jg, jb, jcs, jce, jbc, ngpblks 
    TYPE(datetime), POINTER, INTENT(IN) :: current_date
    REAL(dp), INTENT(IN) :: pzf(:,:)     ! geometric height above sea level, full level [m] (nproma,nlev)
    REAL(dp), INTENT(IN) :: pzh(:,:)     ! geometric height above sea level, half level [m] (nproma,nlevp1)
    REAL(dp), INTENT(IN) :: paphm1(:,:)  ! air pressure, half level [Pa] (nproma,nlevp1)
    REAL(dp), INTENT(INOUT) :: values(:,:)
    INTEGER, OPTIONAL, INTENT(OUT) :: minlev, maxlev   

    INTEGER :: jl, jk, kmin, kmax
    REAL(dp) :: factor
    REAL(dp), ALLOCATABLE :: bc_values(:,:,:) 
    REAL(dp), ALLOCATABLE :: ztmpvalues(:,:)
    TYPE(boundary_conditions), POINTER :: bc

    bc =>  bc_lists(jg)

    !$ACC DATA PRESENT(pzf, pzh, paphm1, values, bc)

    IF ( .NOT. bc%bc_list(jbc)%bc_ldefined ) &
      CALL finish('bc_apply', TRIM(bc%bc_list(jbc)%bc_name)//' has no values defined yet')

    ALLOCATE(bc_values(1:nproma,bc%bc_list(jbc)%bc_ef%ef_nzval,1:ngpblks))
    
    !$ACC ENTER DATA CREATE(bc_values)

    IF (bc%bc_list(jbc)%bc_ef%ef_type == EF_VALUE) THEN
      bc_values = bc%bc_list(jbc)%bc_ef%ef_value
    ELSE
      IF ((bc%bc_list(jbc)%bc_ef%ef_interpolate /= EF_NOINTER) .AND. (bc%bc_list(jbc)%bc_ef%ef_type == EF_FILE)) THEN 
        stop 2517
      ELSE
        bc_values = bc%bc_list(jbc)%bc_values_pointer%p_3d
      ENDIF
    ENDIF

    SELECT CASE(bc%bc_list(jbc)%bc_domain)
    CASE (BC_EVERYWHERE) 
      kmin = 1
      kmax = nlev
    CASE (BC_BOTTOM)
      kmin = nlev
      kmax = nlev
    CASE (BC_TOP)
      kmin = 1
      kmax = 1
    CASE (BC_LEVEL)
      kmin =bc%bc_list(jbc)%bc_minlev
      kmax =bc%bc_list(jbc)%bc_maxlev
    CASE (BC_ALTITUDE, BC_PRESSURE)
      ALLOCATE(ztmpvalues(SIZE(values,1),SIZE(values,2)))
      !$ACC ENTER DATA CREATE(ztmpvalues)

      SELECT CASE(bc%bc_list(jbc)%bc_vertint)
      CASE (BC_VERTICAL_NONE)
      CASE (BC_VERTICAL_INTERPOLATION)
        !$ACC UPDATE HOST(paphm1, pzf, pzh, bc_values) ASYNC(1)
        !$ACC WAIT(1)    
        CALL bc_vert_interpolation(jg,jb,jcs,jce,jbc,paphm1,pzf,pzh,bc_values,ztmpvalues)
        !$ACC UPDATE DEVICE(ztmpvalues) ASYNC(1)
        !$ACC WAIT(1)
      CASE (BC_VERTICAL_WEIGHTED_INTERPOLATION) 
        CALL bc_vert_weighted_interpolation(jg,jb,jcs,jce,jbc,paphm1,pzf,pzh,bc_values,ztmpvalues)
      CASE (BC_VERTICAL_INTEGRATION) 
        STOP 555 
      END SELECT
      kmin = bc%bc_list(jbc)%bc_minlev
      kmax = bc%bc_list(jbc)%bc_maxlev
    END SELECT
    
    IF (bc%bc_list(jbc)%bc_ndim == 2) THEN 
      SELECT CASE(bc%bc_list(jbc)%bc_mode)
      CASE (BC_REPLACE)
        !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1) 
        !$ACC LOOP SEQ
        DO jk = kmin,kmax
          !$ACC LOOP GANG VECTOR 
          DO jl = jcs,jce
            values(jl,jk) = bc_values(jl,1,jb)
          END DO
          !$ACC END LOOP 
        END DO
        !$ACC END PARALLEL
      CASE (BC_ADD)
        !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
        !$ACC LOOP SEQ
        DO jk = kmin,kmax
          !$ACC LOOP GANG VECTOR 
          DO jl = jcs,jce
            values(jl,jk) = values(jl,jk) + bc_values(jl,1,jb)
          END DO
          !$ACC END LOOP 
        END DO
        !$ACC END PARALLEL
      CASE (BC_RELAX)
        factor = exp(-(dtime/bc%bc_list(jbc)%bc_relaxtime))
        !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
        !$ACC LOOP SEQ
        DO jk = kmin,kmax
          !$ACC LOOP GANG VECTOR 
          DO jl = jcs,jce
            values(jl,jk) = (1.0_dp - factor) * values(jl,jk) + factor * bc_values(jl,1,jb)
          END DO
          !$ACC END LOOP 
        END DO
        !$ACC END PARALLEL
      CASE (BC_SPECIAL) 
        CALL finish('bc_apply', TRIM(bc%bc_list(jbc)%bc_name)//' a 3d field was passed, but bc_list contains a 2d field')
      END SELECT
    ELSE
      SELECT CASE(bc%bc_list(jbc)%bc_mode)
      CASE (BC_REPLACE)
        IF (ALLOCATED(ztmpvalues)) THEN
          !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
          !$ACC LOOP GANG VECTOR COLLAPSE(2)
          DO jk = kmin,kmax
            DO jl = jcs,jce
              values(jl,jk) = ztmpvalues(jl,jk)
            END DO
          END DO
          !$ACC END PARALLEL LOOP
          !$ACC WAIT(1)
          !$ACC EXIT DATA DELETE(ztmpvalues)
          DEALLOCATE(ztmpvalues)
        ELSE
          !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
          !$ACC LOOP GANG VECTOR COLLAPSE(2)
          DO jk = kmin,kmax
            DO jl = jcs,jce
              values(jl,jk) = bc_values(jl,jk,jb)
            END DO
          END DO
          !$ACC END PARALLEL LOOP
        ENDIF
      CASE (BC_ADD)
        IF (ALLOCATED(ztmpvalues)) THEN 
          !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
          !$ACC LOOP GANG VECTOR COLLAPSE(2)
          DO jk = kmin,kmax
            DO jl = jcs,jce
              values(jl,jk) = values(jl,jk) + ztmpvalues(jl,jk)
            END DO
          END DO
          !$ACC END PARALLEL LOOP
          !$ACC WAIT(1)
          !$ACC EXIT DATA DELETE(ztmpvalues)
          DEALLOCATE(ztmpvalues)
        ELSE
          !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
          !$ACC LOOP GANG VECTOR COLLAPSE(2)
          DO jk = kmin,kmax
            DO jl = jcs,jce
              values(jl,jk) = values(jl,jk) + bc_values(jl,jk,jb)
            END DO
          END DO
          !$ACC END PARALLEL LOOP
        ENDIF
      CASE (BC_RELAX)
        factor = exp(-(dtime/bc%bc_list(jbc)%bc_relaxtime))
        IF (ALLOCATED(ztmpvalues)) THEN
          !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
          !$ACC LOOP GANG VECTOR COLLAPSE(2) 
          DO jk = kmin,kmax
            DO jl = jcs,jce
              values(jl,jk) = (1.0_dp - factor) * values(jl,jk) + factor * ztmpvalues(jl,jk)
            END DO
          END DO
          !$ACC END PARALLEL LOOP
          !$ACC WAIT(1)
          !$ACC EXIT DATA DELETE(ztmpvalues)
          DEALLOCATE(ztmpvalues)
        ELSE 
          !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
          !$ACC LOOP GANG VECTOR COLLAPSE(2)
          DO jk = kmin,kmax
            DO jl = jcs,jce
              values(jl,jk) = (1.0_dp - factor) * values(jl,jk) + factor * bc_values(jl,jk,jb)
            END DO
          END DO
          !$ACC END PARALLEL LOOP
        ENDIF  
      CASE (BC_SPECIAL)
        IF (ALLOCATED(ztmpvalues)) THEN
          !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
          !$ACC LOOP GANG VECTOR COLLAPSE(2)
          DO jk = kmin,kmax
            DO jl = jcs,jce
              values(jl,jk) = ztmpvalues(jl,jk)
            END DO
          END DO
          !$ACC END PARALLEL LOOP
          !$ACC WAIT(1)
          !$ACC EXIT DATA DELETE(ztmpvalues)
          DEALLOCATE(ztmpvalues)
        ELSE
          !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
          !$ACC LOOP GANG VECTOR COLLAPSE(2)
          DO jk = kmin,kmax
            DO jl = jcs,jce
              values(jl,jk) = bc_values(jl,jk,jb)
            END DO
          END DO
          !$ACC END PARALLEL LOOP
        ENDIF
      END SELECT
    ENDIF

    IF (PRESENT(minlev)) minlev = kmin
    IF (PRESENT(maxlev)) maxlev = kmax

    !$ACC WAIT(1) 
    !$ACC EXIT DATA DELETE(bc_values) 
    DEALLOCATE(bc_values)
  
    !$ACC END DATA 

  END SUBROUTINE bc_apply2d

! !---------------------------------------------------------------------------------------------------

  SUBROUTINE bc_vert_interpolation ( jg, jb, jcs, jce, jbc, paphm1, pzf, pzh, bc_values, values )

    USE mo_kind_lite,        ONLY: dp
    USE mo_run_config_lite,  ONLY: nlev, nlevp1 

    INTEGER,  INTENT(IN)  :: jg, jb, jcs, jce, jbc
    REAL(dp), INTENT(IN)  :: pzf(:,:)    ! geometric height above sea level, full level [m] (nproma,nlev)
    REAL(dp), INTENT(IN)  :: pzh(:,:)    ! geometric height above sea level, half level [m] (nproma,nlevp1)
    REAL(dp), INTENT(IN)  :: paphm1(:,:) ! air pressure, half level [Pa] (nproma,nlevp1)
    REAL(dp), ALLOCATABLE, INTENT(IN) :: bc_values(:,:,:)
    REAL(dp), ALLOCATABLE, INTENT(INOUT) :: values(:,:)

    INTEGER  :: jk, jl, jz, jincr, jzstart, jzstop, jkstart, jkstop
    REAL(dp) :: zmha(jce,nlevp1)
    REAL(dp) :: denom_diff, nom_diff, vertintfac
    TYPE(boundary_conditions), POINTER  :: bc

    bc=>bc_lists(jg)

    values(:,:) = 0._dp

    IF (bc%bc_list(jbc)%bc_domain == BC_ALTITUDE) THEN

      DO jk = 1,nlevp1
        DO jl = jcs,jce
          zmha(jl,jk) = pzh(jl,jk) 
        END DO 
      END DO
      jzstart = bc%bc_list(jbc)%bc_ef%ef_nzval
      jzstop = 1 
      jkstart = bc%bc_list(jbc)%bc_maxlev 
      jkstop  = bc%bc_list(jbc)%bc_minlev
      IF (jkstop == 1) jkstop = 2
 
      DO jl = jcs,jce

        jz = jzstart
 
        DO jk = jkstart,jkstop,-1
    
          IF (zmha(jl,jk) .ge. bc%bc_list(jbc)%bc_ef%ef_zvalueh(jzstart)) THEN
            values(jl,jk) = bc_values(jl,jzstart,jb)
          ELSE IF (zmha(jl,jk) .le. bc%bc_list(jbc)%bc_ef%ef_zvalueh(jzstop)) THEN
            values(jl,jk) = bc_values(jl,jzstop,jb)
          ELSE
            denom_diff  = bc%bc_list(jbc)%bc_ef%ef_zvalueh(jz) - bc%bc_list(jbc)%bc_ef%ef_zvalueh(jz-1)
            nom_diff    = bc%bc_list(jbc)%bc_ef%ef_zvalueh(jz) - zmha(jl,jk)
            IF (nom_diff .ge. 0.0_dp) THEN
              vertintfac = nom_diff / denom_diff
              values(jl,jk) = bc_values(jl,jz,jb) + vertintfac * &
                              (bc_values(jl,jz-1,jb) - bc_values(jl,jz,jb))
            ELSE
              jz = MAX(jz - 1, jzstop)
            ENDIF
          ENDIF

        ENDDO

      ENDDO

    ELSE

      DO jk = 1,nlevp1
        DO jl = jcs,jce
          zmha(jl,jk) = paphm1(jl,jk)
        END DO 
      END DO
      jzstart = 2
      jzstop  = bc%bc_list(jbc)%bc_ef%ef_nzval + 1
      jkstart = bc%bc_list(jbc)%bc_minlev + 1
      jkstop  = bc%bc_list(jbc)%bc_maxlev
 
      DO jl = jcs,jce
 
        jz = jzstart
 
        DO jk = jkstart,jkstop

          IF (zmha(jl,jk) .le. bc%bc_list(jbc)%bc_ef%ef_zvalueh(jzstart)) THEN
            values(jl,jk) = bc_values(jl,jzstart,jb)
          ELSE IF (zmha(jl,jk) .ge. bc%bc_list(jbc)%bc_ef%ef_zvalueh(jzstop)) THEN
            values(jl,jk) = bc_values(jl,jzstop,jb)
          ELSE
            denom_diff  = bc%bc_list(jbc)%bc_ef%ef_zvalueh(jz) - bc%bc_list(jbc)%bc_ef%ef_zvalueh(jz-1)
            nom_diff    = bc%bc_list(jbc)%bc_ef%ef_zvalueh(jz) - zmha(jl,jk)
            IF (nom_diff .ge. 0.0_dp) THEN
              vertintfac   = nom_diff / denom_diff
              values(jl,jk) = bc_values(jl,jz,jb) + vertintfac * &
                              (bc_values(jl,jz-1,jb) - bc_values(jl,jz,jb))
            ELSE
              jz = MIN(jz + 1, jzstop)
            ENDIF
          ENDIF

        ENDDO
        
      ENDDO

    ENDIF

  END SUBROUTINE bc_vert_interpolation

! !---------------------------------------------------------------------------------------------------
 
  SUBROUTINE bc_vert_weighted_interpolation(  jg, jb, jcs, jce, jbc,              & 
                                              paphm1, pzf, pzh, bc_values, values )

    USE mo_kind_lite,        ONLY: dp
    USE mo_run_config_lite,  ONLY: nlev, nlevp1

    INTEGER,  INTENT(IN)  :: jg, jb, jcs, jce, jbc
    REAL(dp), INTENT(IN)  :: pzf(:,:)    ! geometric height above sea level, full level [m] (nproma,nlev)
    REAL(dp), INTENT(IN)  :: pzh(:,:)    ! geometric height above sea level, half level [m] (nproma,nlevp1)
    REAL(dp), INTENT(IN)  :: paphm1(:,:) ! air pressure, half level [Pa] (nproma,nlevp1)
    REAL(dp), ALLOCATABLE, INTENT(IN) :: bc_values(:,:,:)
    REAL(dp), ALLOCATABLE, INTENT(INOUT) :: values(:,:)

    INTEGER  :: jk, jl, jz, jincr, jzstart, jzstop, jkstart, jkstop
    REAL(dp) :: zmha(jce,nlevp1)
    REAL(dp) :: zfrac, zmdelta, zmdeltai
    TYPE(boundary_conditions), POINTER  :: bc

    bc=>bc_lists(jg)
  
    values(:,:) = 0._dp
    !$ACC ENTER DATA COPYIN(bc%bc_list(jbc)%bc_ef%ef_zvalueh)

    !$ACC DATA PRESENT(pzf, pzh, paphm1, bc_values, values, bc) &
    !$ACC   CREATE(zmha)  
    IF (bc%bc_list(jbc)%bc_domain == BC_ALTITUDE) THEN
      !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
      !$ACC LOOP GANG VECTOR COLLAPSE(2)
      DO jk = 1,nlevp1
        DO jl = jcs,jce
          zmha(jl,jk) = pzh(jl,jk) 
        END DO 
      END DO
      !$ACC END PARALLEL LOOP 

      jincr   = -1
      jzstart = bc%bc_list(jbc)%bc_ef%ef_nzval
      jzstop  = 1
      jkstart = bc%bc_list(jbc)%bc_maxlev 
      jkstop  = bc%bc_list(jbc)%bc_minlev 
      IF (jkstop == 1) jkstop = 2
    ELSE
      !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
      !$ACC LOOP GANG VECTOR COLLAPSE(2)
      DO jk = 1,nlevp1
        DO jl = jcs,jce
          zmha(jl,jk) = paphm1(jl,jk) 
        END DO 
      END DO
      !$ACC END PARALLEL LOOP 

      jincr  = 1
      jzstart = 2
      jzstop  = bc%bc_list(jbc)%bc_ef%ef_nzval + 1
      jkstart = bc%bc_list(jbc)%bc_minlev + 1
      jkstop  = bc%bc_list(jbc)%bc_maxlev
    ENDIF

    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR COLLAPSE(2) PRIVATE(zmdelta, zmdeltai, jz, zfrac)
    DO jk = jkstart,jkstop,jincr
      DO jl = jcs,jce
        
        ! calculate increment of half levels
        zmdelta = zmha(jl,jk) - zmha(jl,jk-jincr)
        zmdeltai = 1._dp / zmdelta

        ! loop over levels of emission file      
        !$ACC LOOP SEQ
        DO jz = jzstart,jzstop,jincr

          ! fraction of file level overlapping with model level 
          zfrac = MAX(0._dp, (MIN(zmha(jl,jk), bc%bc_list(jbc)%bc_ef%ef_zvalueh(jz)) - &
            MAX(zmha(jl,jk-jincr), bc%bc_list(jbc)%bc_ef%ef_zvalueh(jz-jincr))) * zmdeltai)

          ! fraction of file level coinciding with model level
          zfrac = MIN(zfrac, 1._dp)
          values(jl,jk) = values(jl,jk) + zfrac * bc_values(jl,jz,jb)

        ENDDO

      ENDDO
    ENDDO
    !$ACC END PARALLEL LOOP 

    !eehol: add some values
    !$ACC PARALLEL DEFAULT(PRESENT) ASYNC(1)
    !$ACC LOOP GANG VECTOR COLLAPSE(2)
    DO jk = 1,nlev
      DO jl = jcs,jce
        IF (values(jl,jk)<=0._dp) THEN
          values(jl,jk) = bc_values(jl,jk,jb)
        ENDIF
      ENDDO
    ENDDO
    !$ACC END PARALLEL LOOP

    !$ACC WAIT(1)
    !$ACC END DATA

    !$ACC EXIT DATA DELETE(bc%bc_list(jbc)%bc_ef%ef_zvalueh)
  END SUBROUTINE bc_vert_weighted_interpolation

END MODULE mo_subm_bcs_lite
