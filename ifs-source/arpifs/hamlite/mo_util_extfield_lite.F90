! 
! based on mo_external_field_processor.f90  
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

MODULE mo_util_extfield_lite

  USE mo_kind_lite,            ONLY: wp, dp
  USE mtime_lite,              ONLY: datetime
  USE mo_exception_lite,       ONLY: finish, message, message_text
  USE mo_util_timeconv_lite,   ONLY: datetimes  

  IMPLICIT NONE

  INCLUDE  'netcdf.inc' 

  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: ef_get_first_timestep!, ef_get_next_timestep
  PUBLIC :: ef_read_single!, ef_get_levname_and_size, ef_get_full_and_half_level_information 

  PUBLIC :: EF_INACTIVE, EF_VALUE, EF_FILE, EF_MODULE
  PUBLIC :: EF_UNDEFINED, EF_3D, EF_LONLAT, EF_LEV, EF_SINGLE
  PUBLIC :: EF_TIMERESOLVED, EF_IGNOREYEAR, EF_CONSTANT
  PUBLIC :: EF_NOINTER, EF_LINEAR, EF_CUBIC

  PUBLIC :: external_field

  !--------------------
  ! module parameters
  !--------------------

  ! flags of ef_type
  INTEGER, PARAMETER :: EF_INACTIVE = 0
  INTEGER, PARAMETER :: EF_VALUE    = 1
  INTEGER, PARAMETER :: EF_FILE     = 2
  INTEGER, PARAMETER :: EF_MODULE   = 3

  ! flags of ef_geometry
  INTEGER, PARAMETER :: EF_UNDEFINED = 0
  INTEGER, PARAMETER :: EF_3D        = 1
  INTEGER, PARAMETER :: EF_LONLAT    = 2
  INTEGER, PARAMETER :: EF_LEV       = 3
  INTEGER, PARAMETER :: EF_SINGLE    = 4

  ! flags of ef_timedef
  INTEGER, PARAMETER :: EF_TIMERESOLVED = 1
  INTEGER, PARAMETER :: EF_IGNOREYEAR   = 2
  INTEGER, PARAMETER :: EF_CONSTANT     = 3

  ! flags of ef_interpolate
  INTEGER, PARAMETER :: EF_NOINTER = 0
  INTEGER, PARAMETER :: EF_LINEAR  = 1
  INTEGER, PARAMETER :: EF_CUBIC   = 2

  CHARACTER(LEN=*), PARAMETER :: thismodule = "mo_util_extfield"

  !--------------------
  ! module variables
  !--------------------

  TYPE :: external_field    
    INTEGER                      :: ef_type = EF_INACTIVE, &    ! EF_MODULE, EF_FILE, EF_VALUE, EF_INACTIVE
                                                                ! information for type='file'
                                    ef_nzval, &                 ! number of levels in input file (set by user)
                                    ef_geometry=-1, &           ! EF_3D, EF_LONLAT, EF_LEV, EF_SINGLE
                                    ef_timedef=1, &             ! EF_TIMERESOLVED, EF_IGNOREYEAR, EF_CONSTANT
                                    ef_timeindex = 0, &         ! time record in ef file
                                    ef_timeindex_prior = 0, &   ! (for time interpolation:
                                                                ! prior time step might be from a previous file)
                                    ef_interpolate = EF_NOINTER ! no interpolation in time
    LOGICAL                      :: ef_ltpredict=.false.        ! is the next timestep predictable from the actual one?
                                                                ! (important for the last timestep in a file)
    REAL(dp)                     :: ef_timeoffset = 0.0_dp, &   ! offset value for file time
                                                                ! (unit: unit of time record in input file)
                                    ef_timetol = 2.0_dp, &      ! tolerance value for time comparison  
                                    ef_value = 0._dp, &         ! information for type='value'
                                    ef_factor = 1.0_dp          ! scaling factor
    REAL(dp), ALLOCATABLE        :: ef_zvalueh(:)               ! half levels either in height[m]    (descending order)
                                                                !             or     in pressure[Pa] (ascending order)
                                                                ! (not set by user)
    CHARACTER(LEN=512)           :: &
                                    ef_file= '', &              ! filename
                                    ef_template= '', &          ! template for filename
                                    ef_varname= ''              ! variable name (set by user via namelist)
    CHARACTER(LEN=30)            :: ef_actual_unit=''
    TYPE(datetimes), ALLOCATABLE :: ef_times(:)                 ! (not set by user)
    TYPE(datetimes), ALLOCATABLE :: ef_times_prior(:)           ! (for time interpolation:
                                                                ! prior time step might be from a previous file)
  END TYPE external_field
 
  CONTAINS
 
! !---------------------------------------------------------------------------------------------------

  SUBROUTINE ef_check (jg, p_patch, efield, ncid)

    USE mo_model_domain_lite,    ONLY: t_patch 
    
    IMPLICIT NONE

    INTEGER, INTENT(IN)                 :: jg  
    INTEGER, INTENT(IN)                 :: ncid
    TYPE(t_patch), INTENT(IN)           :: p_patch 
    TYPE(external_field), INTENT(INOUT) :: efield

    INTEGER                             :: dimid, dimlen, varid, status
    CHARACTER                           :: cinter

    IF (trim(efield%ef_varname) .EQ. '') THEN
      CALL message('ef_check', 'ef_varname has to be defined!')
    ELSE
      status = nf_inq_varid( ncid, efield%ef_varname, varid )
      IF (status /= nf_noerr) THEN
        CALL message('ef_check', trim(efield%ef_varname)  //  &
          ' does not exist in file ' // TRIM(efield%ef_file) // '!')
      ELSE
        IF (.not. ef_check_var(efield,ncid)) CALL message('ef_check', &
          'wrong dimensionality for ' // TRIM(efield%ef_varname)//' in ' // TRIM(efield%ef_file))
      ENDIF
    ENDIF
    SELECT CASE (efield%ef_geometry)
      CASE (EF_3D, EF_LONLAT, EF_LEV, EF_SINGLE)
        ! IF (.NOT. ef_check_geometry( jg, p_patch, efield,ncid)) CALL message('ef_check', &
        !   '  missing dimension or wrong dimension length found in '//TRIM(efield%ef_file))
      CASE DEFAULT
        CALL message('ef_check', 'not a defined external_field_geometry given!')
    END SELECT
    SELECT CASE (efield%ef_timedef)
      CASE (EF_TIMERESOLVED, EF_IGNOREYEAR, EF_CONSTANT)
        IF (efield%ef_timedef .eq. EF_CONSTANT) THEN

          ! check whether ef_timeindex is set

          IF (efield%ef_timeindex .eq. 0) THEN

            ! better handled in other routines if set!

            efield%ef_timeindex = 1
            CALL nf_check(nf_inq_dimid  (ncid, "time", dimid))
            CALL nf_check(nf_inq_dimlen (ncid, dimid, dimlen))
            IF (dimlen /= 1) CALL message('ef_check', 'no timeindex (of multiple timesteps) ' // &
              'for input file defined but timedef "constant" is given!')
          ENDIF
        ENDIF
      CASE DEFAULT
        CALL message('ef_check', 'not a defined external_field_timedef given!')
    END SELECT
    SELECT CASE (efield%ef_interpolate)
      CASE (0, 1, 2)
      CASE DEFAULT
        write(cinter,'(i1)') efield%ef_interpolate
        CALL message('ef_check', cinter//' is not a defined external_field_interpolation!')
    END SELECT
    IF (.NOT. ef_check_units(efield)) CALL message('ef_check', 'unit mismatch in field definition and file!')

  END SUBROUTINE ef_check

! !---------------------------------------------------------------------------------------------------

  RECURSIVE FUNCTION ef_get_geometry(efield,ncid,jgeom) RESULT (found)
 
!    USE mo_mpi_lite,         ONLY: my_process_is_mpi_workroot, p_pe, p_io, p_bcast, p_comm_work

    IMPLICIT NONE

    TYPE(external_field), INTENT(inout) :: efield
    INTEGER, INTENT(in)                 :: ncid
    INTEGER, POINTER, INTENT(in)        :: jgeom

    INTEGER, PARAMETER                  :: MAXDIM=4
    INTEGER                             :: ndim
    CHARACTER(LEN=20)                   :: dims(MAXDIM)
    INTEGER                             :: varid
    LOGICAL                             :: found
    CHARACTER(LEN=25)                   :: varname
    CHARACTER(len=9), PARAMETER         :: geometry_name(4) = (/ 'EF_3D    ', &
                                                                'EF_LONLAT', &
                                                                'EF_LEV   ', &
                                                                'EF_SINGLE' /)

    found = .FALSE.

    SELECT CASE (efield%ef_geometry)
      CASE (EF_3D) 
          ndim=2
          dims(1) = 'cell'
          dims(2) = 'hlev'
      CASE (EF_LONLAT)
          ndim=1
          dims(1) = 'cell'
      CASE (EF_LEV)
          ndim=1
          dims(1) = 'hlev'
      CASE (EF_SINGLE)
          ndim=0 
      CASE (EF_UNDEFINED)
          !! try out various options by calling ef_get_geometry recursively
          ndim=-1
          efield%ef_geometry = EF_3D
          found = ef_get_geometry(efield,ncid,jgeom)
          IF (.NOT. found) THEN
            efield%ef_geometry = EF_LONLAT
            found = ef_get_geometry(efield,ncid,jgeom)
          END IF
          IF (.NOT. found) THEN
            efield%ef_geometry = EF_LEV
            found = ef_get_geometry(efield,ncid,jgeom)
          END IF
          IF (.NOT. found) THEN
            efield%ef_geometry = EF_SINGLE
            found = ef_get_geometry(efield,ncid,jgeom)
          END IF
          IF (found) THEN
            jgeom = efield%ef_geometry
            CALL message('ef_get_geometry', 'Determined geometry for '//TRIM(efield%ef_varname)// &
                        ' in '//TRIM(efield%ef_file)//' as '//TRIM(geometry_name(efield%ef_geometry)))
          ELSE
            CALL message('ef_get_geometry', 'Could not determine geometry for '//TRIM(efield%ef_varname)//  &
                        ' in '//TRIM(efield%ef_file))
          END IF 
    END SELECT

    IF ((efield%ef_timedef /= EF_CONSTANT) .AND. (ndim /= -1)) THEN
      ndim = ndim + 1
      dims(ndim) = 'time'
    ENDIF
 
    IF (.NOT. found) THEN
      CALL nf_check( nf_inq_varid( ncid, efield%ef_varname, varid ),efield%ef_file)
!      IF ( p_pe == p_io ) THEN 
      found = ef_check_vardims(ncid,varid, ndim, dims)
!      END IF
!      CALL p_bcast(found,     p_io, p_comm_work)
      IF (found) jgeom = efield%ef_geometry
    END IF
 
  END FUNCTION ef_get_geometry

! !---------------------------------------------------------------------------------------------------

  FUNCTION ef_check_var (efield, ncid) RESULT(lfound)
      
    TYPE(external_field), INTENT(inout) :: efield
    INTEGER, INTENT(in)                 :: ncid
    
    INTEGER, TARGET  :: igeom
    INTEGER, POINTER :: pgeom
    LOGICAL :: lfound

    igeom = EF_UNDEFINED
    pgeom => igeom
    lfound = ef_get_geometry(efield,ncid,pgeom)
    IF (lfound) THEN
      efield%ef_geometry = igeom
    ENDIF
    
  END FUNCTION ef_check_var

! !---------------------------------------------------------------------------------------------------

  FUNCTION ef_check_units(efield) RESULT (found)
 
    TYPE(external_field), INTENT(in) :: efield
    LOGICAL                          :: found

    found = .true.
 
  END FUNCTION ef_check_units

! !---------------------------------------------------------------------------------------------------

  ! FUNCTION ef_check_geometry(jg, p_patch, efield, io_file_id) RESULT (lok)
 
  !   USE mo_model_domain,      ONLY: t_patch 
  !   USE mo_grid_config,       ONLY: dynamics_grid_filename
  !   USE mo_parallel_config,   ONLY: nproma, idx_no, blk_no
  
  !   IMPLICIT NONE

  !   INTEGER, INTENT(IN)                 :: jg  
  !   INTEGER, INTENT(IN)                 :: io_file_id
  !   TYPE(t_patch),        INTENT(IN)    :: p_patch  
  !   TYPE(external_field), INTENT(INOUT) :: efield

  !   INTEGER, PARAMETER               :: MAXDIM=4
  !   INTEGER                          :: ndim, i, dimmodell(MAXDIM), io_dimlen, io_var_id, iret, g_var_id 
  !   REAL(dp), ALLOCATABLE            :: zlatvals(:), zlonvals(:), zlatvals_g(:), zlonvals_g(:), dl(:)
  !   REAL(dp)                         :: zlatmax, zlatmin, zlonmax, zlonmin
  !   CHARACTER(LEN=20)                :: dimnames(MAXDIM)
  !   LOGICAL                          :: lok, lcheckgrid
  !   INTEGER                          :: ncells_g, blks, nlev
  !   INTEGER                          :: grid_file_id 

  !   ncells_g = p_patch%n_patch_cells_g ! _g: number of cells on the global patch 
  !   nlev  = p_patch%nlev
  !   blks = p_patch%nblks_c
    
  !   lok = .TRUE.
  !   lcheckgrid = .FALSE.
  !   SELECT CASE (efield%ef_geometry)
  !     CASE (EF_3D)
  !         ndim=2
  !         dimnames(1) = 'hlev'
  !         dimmodell(1) = nlev
  !         dimnames(2) = 'cell'
  !         dimmodell(2) = ncells_g
  !         lcheckgrid = .TRUE.
  !     CASE (EF_LONLAT)
  !         ndim=1
  !         dimnames(1) = 'cell'
  !         dimmodell(1) = ncells_g
  !         lcheckgrid = .TRUE.
  !     CASE (EF_LEV) 
  !         stop 4433
  !         ndim=1
  !         dimnames(1) = 'hlev'
  !         dimmodell(1) = p_patch%nlev
  !     CASE (EF_SINGLE)
  !         ndim=0
  !   END SELECT

  !   DO i = 1, ndim
  !     iret = nf_inq_dimid  (io_file_id, dimnames(i), io_var_id )
  !     IF ( (iret /= 0 ) .AND. ( dimnames(i) .eq. 'hlev' ) ) THEN
  !       CALL nf_check(nf_inq_dimid  (io_file_id, 'lev', io_var_id),"ef_check_geometry_2")
  !     END IF
  !     CALL nf_check(nf_inq_dimlen (io_file_id, io_var_id, io_dimlen))
  !     IF (io_dimlen /= dimmodell(i)) THEN 
  !       IF (dimnames(i) .EQ. 'hlev' ) THEN  
  !       ELSE
  !         WRITE(message_text, '(a,i0,a,i0)') 'ef_check_geometry: dimlen(lev) = ',io_dimlen,', model levels = ',dimmodell(i)
  !         CALL message('ef_check_geometry', message_text)
  !         lok = .FALSE.
  !       END IF
  !     END IF
  !   ENDDO
 
  !   IF (lok .AND. lcheckgrid) THEN

  !     ALLOCATE ( zlatvals( ncells_g ) )
  !     ALLOCATE ( zlonvals( ncells_g ) )
  !     ALLOCATE ( zlatvals_g( ncells_g ) )
  !     ALLOCATE ( zlonvals_g( ncells_g ) )
  !     ALLOCATE ( dl( ncells_g ) )

  !     CALL nf_check (nf_inq_varid(io_file_id, 'clat', io_var_id ),TRIM(efield%ef_file))
  !     CALL nf_check (nf_get_vara_double(io_file_id,io_var_id,(/1/),(/ ncells_g /),zlatvals),TRIM(efield%ef_file) )

  !     CALL nf_check (nf_inq_varid(io_file_id, 'clon', io_var_id ),TRIM(efield%ef_file))
  !     CALL nf_check (nf_get_vara_double(io_file_id,io_var_id,(/1/),(/ ncells_g /),zlonvals),TRIM(efield%ef_file) )

  !     ! check against grid file (don't have cell locations for global patch here)
  !     CALL nf_check( nf_open(dynamics_grid_filename(jg), NF_NOWRITE, grid_file_id ), TRIM(dynamics_grid_filename(jg)) )

  !     CALL nf_check (nf_inq_varid(grid_file_id, 'clat', g_var_id ),TRIM(dynamics_grid_filename(jg)) )
  !     CALL nf_check (nf_get_vara_double(grid_file_id,g_var_id,(/1/),(/ ncells_g /),zlatvals_g),TRIM(dynamics_grid_filename(jg)) )

  !     CALL nf_check (nf_inq_varid(grid_file_id, 'clon', g_var_id ),TRIM(dynamics_grid_filename(jg)) )
  !     CALL nf_check (nf_get_vara_double(grid_file_id,g_var_id,(/1/),(/ ncells_g /),zlonvals_g),TRIM(dynamics_grid_filename(jg)) )

  !     dl=zlatvals-zlatvals_g
  !     IF ( MAXVAL ( dl ) .GT. 1.e-10 ) THEN !unit here is radians
  !       CALL message(thismodule,'File: '//TRIM(efield%ef_file))
  !       CALL finish(thismodule, 'ef_check_geometry: Input latitudes do not match model latitudes!')
  !     END IF  

  !     dl=zlonvals-zlonvals_g
  !     IF ( MAXVAL ( dl ) .GT. 1.e-10 ) THEN !unit here is radians
  !       CALL message(thismodule,'File: '//TRIM(efield%ef_file))
  !       CALL finish(thismodule, 'ef_check_geometry: Input longitudes do not match model longitudes!')
  !     END IF  

  !     CALL nf_check( nf_close( grid_file_id ) )
      
  !     DEALLOCATE ( zlatvals )
  !     DEALLOCATE ( zlonvals )
  !     DEALLOCATE ( zlatvals_g )
  !     DEALLOCATE ( zlonvals_g )
  !     DEALLOCATE ( dl )
  !   END IF

  ! END FUNCTION ef_check_geometry

! !---------------------------------------------------------------------------------------------------

  FUNCTION ef_check_vardims(ncid,varid, ndim, dims) RESULT (found)

    IMPLICIT NONE

    INTEGER, INTENT(IN)                   :: ncid,varid, ndim
    CHARACTER(LEN=*), INTENT(IN)          :: dims(*)

    INTEGER                               :: i, j, vardim
    LOGICAL                               :: found, found2
    INTEGER, DIMENSION(NF_MAX_VAR_DIMS)   :: vardimids
    CHARACTER(LEN=20)                     :: vardimname

    found = .FALSE.
    CALL nf_check(nf_inq_varndims(ncid, varid, vardim))
    IF (vardim .eq. ndim) THEN
      found = .TRUE.
      CALL nf_check(nf_inq_vardimid(ncid, varid, vardimids))
      DO i = 1, ndim
        CALL nf_check(nf_inq_dimname(ncid, vardimids(i), vardimname))
        found2=.FALSE.
        DO j = 1, ndim
          IF (TRIM(dims(j)) == TRIM(vardimname)) found2 = .TRUE.
        ENDDO
        IF ((.NOT. found2) .AND. (vardimname == 'lev')) THEN
          vardimname ='hlev'
          DO j = 1, ndim
            IF (dims(j) == vardimname) found2 = .TRUE.
          ENDDO
        ENDIF
        found = found .AND. found2
      ENDDO
    ENDIF

  END FUNCTION ef_check_vardims

! !---------------------------------------------------------------------------------------------------

!   SUBROUTINE ef_convert_times( current_date, dimlen, intimes, units, timedef, seasonal, &
!                                outtimes, lmonthly, newyear                              )
 
!     USE mtime,                  ONLY: getNoOfDaysInYearDateTime, deallocateDatetime
!     USE mo_util_timeconv,       ONLY: datetimes, TC_set, add_date, TC_split, write_date

!     IMPLICIT NONE

!     TYPE(datetime), INTENT(IN)             :: current_date
!     INTEGER, INTENT(IN)                    :: dimlen
!     REAL(dp), INTENT(IN)                   :: intimes(:)
!     CHARACTER (len = *), INTENT(IN)        :: units
!     INTEGER, INTENT(IN)                    :: timedef
!     LOGICAL, INTENT(IN)                    :: seasonal
!     INTEGER, INTENT(IN)                    :: newyear
!     TYPE(datetimes)                        :: outtimes(:)
!     LOGICAL, INTENT(OUT)                   :: lmonthly

!     INTEGER           :: nstart, i
!     INTEGER           :: year, month, day, hour, minute, second
!     INTEGER           :: nyear,nmonth,nday,nhour,nminute,nsecond
!     INTEGER           :: cyear,cmonth,cday,chour,cminute,csecond
!     INTEGER           :: add_day, add_second
!     CHARACTER(LEN=128):: clunits       ! local copy with minimum length
!     LOGICAL           :: lsince
!     INTEGER           :: yl1, yl2

!     TYPE(datetime), POINTER  :: dummy_date

!     ! 1) --- identify time units 
!     second = 0
!     lsince   = .FALSE.
!     lmonthly = .FALSE.
!     clunits = TRIM(units)//'                             '

!     IF (clunits(1:9) == "day since") THEN
!       lsince = .TRUE.
!       nstart = 10 
!       CALL nf_getreftime(units(nstart:), year, month, day, hour, minute)
!     ELSE IF (clunits(1:10) == "days since") THEN
!       lsince = .TRUE.
!       nstart = 11
!       CALL nf_getreftime(units(nstart:), year, month, day, hour, minute)
!     ELSE IF (clunits(1:11) == "month since") THEN
!       lsince = .TRUE.
!       nstart = 12
!       CALL nf_getreftime(units(nstart:), year, month, day, hour, minute)
!       lmonthly = .TRUE.
!     ELSE IF (clunits(1:12) == "months since") THEN
!       lsince = .TRUE.
!       nstart = 13
!       CALL nf_getreftime(units(nstart:), year, month, day, hour, minute)
!       lmonthly = .TRUE.
!     ELSE IF (clunits(1:5) == "month") THEN
!       lmonthly = .TRUE.
!     ELSE
!       CALL message('ef_convert_times', 'time format not supported!')
!     ENDIF
      
!     ! 2) --- convert date to standard format
!     IF (lsince) THEN
!       DO i = 1, dimlen
 
!         IF (lmonthly) THEN
!           nyear = year + (month+int(intimes(i))) / 12
!           nmonth = mod(month+int(intimes(i)),12)
!           if (nmonth == 0) nmonth = 12
!           CALL TC_set(nyear,nmonth,day,hour,minute,second,outtimes(i)%datetime)
!         ELSE
!           CALL TC_set(year,month,day,hour,minute,second,outtimes(i)%datetime) 
!           add_day=int(intimes(i))
!           add_second=nint((intimes(i)-add_day)*86400.0_dp)
!           CALL add_date(outtimes(i)%datetime, days=add_day, seconds=add_second)  
!         END IF
  
!         IF (timedef == EF_IGNOREYEAR) THEN

!             yl1=getNoOfDaysInYearDateTime(outtimes(i)%datetime)
!             CALL TC_split(outtimes(i)%datetime,cyear,cmonth,cday,chour,cminute,csecond)
!             CALL TC_set( newyear, cmonth, cday, chour, cminute, csecond, dummy_date )
!             yl2=getNoOfDaysInYearDateTime(dummy_date)
!             CALL deallocateDatetime(dummy_date)  

!           IF (yl1 == yl2) THEN
!             cyear = newyear
!             CALL deallocateDatetime(outtimes(i)%datetime)
!             CALL TC_set(cyear,cmonth,cday,chour,cminute,csecond,outtimes(i)%datetime)
!           ELSE
!             cyear = newyear
!             IF ((cmonth == 2) .AND. (cday == 29)) THEN
!               CALL deallocateDatetime(outtimes(i)%datetime)
!               CALL TC_set(cyear,2,28,23,59,59,outtimes(i)%datetime)
!             ELSE
!               CALL deallocateDatetime(outtimes(i)%datetime)
!               CALL TC_set(cyear,cmonth,cday,chour,cminute,csecond,outtimes(i)%datetime)
!             ENDIF
!           ENDIF
!         ENDIF

!       END DO
!     ELSE 
!       DO i = 1, dimlen
!         IF (timedef == EF_IGNOREYEAR) THEN
!             year = newyear
!         ELSE
!             year=INT(current_date%date%year)
!         END IF
!         nmonth=int(intimes(i))
!         CALL TC_set(year,nmonth,1,0,0,0,outtimes(i)%datetime)
!       END DO
!     END IF

!   END SUBROUTINE ef_convert_times

! !---------------------------------------------------------------------------------------------------

  SUBROUTINE nf_getreftime(cdate, year, month, day, hour, minute)

    IMPLICIT NONE

    CHARACTER(len=*), INTENT(IN)   :: cdate
    INTEGER, INTENT(OUT)           :: year, month, day, hour, minute

    INTEGER   :: nlen, nstart, ndx

    nlen = LEN_TRIM(cdate)

    nstart = 1
    ndx = INDEX(cdate(nstart:nlen),'-')
    read(cdate(nstart:nstart+ndx-2),*) year
    nstart = nstart+ndx
    ndx = INDEX(cdate(nstart:nlen),'-') 
    read(cdate(nstart:nstart+ndx-2),*) month
    nstart = nstart+ndx
    ndx = INDEX(cdate(nstart:nlen),' ') 
    IF (ndx > 0) THEN
      read(cdate(nstart:nstart+ndx-2),*) day
      nstart = nstart+ndx
      ndx = INDEX(cdate(nstart:nlen),':') 
      read(cdate(nstart:nstart+ndx-2),*) hour
      nstart = nstart+ndx
      read(cdate(nstart:nlen),'(i2)') minute
    ELSE
      read(cdate(nstart:nlen),*) day
      hour = 0
      minute = 0
    ENDIF

  END SUBROUTINE nf_getreftime
 
!---------------------------------------------------------------------------------------------------

!   SUBROUTINE ef_check_time(efield, time, lmonthly)
 
!     IMPLICIT NONE

!     REAL(dp),             INTENT(IN) :: time(:)
!     LOGICAL,              INTENT(IN) :: lmonthly
!     TYPE(external_field), INTENT(IN) :: efield

!     INTEGER    :: ntime, i
!     REAL(dp)   :: testval
!     LOGICAL    :: lerr

!     ntime = SIZE(time)

!     ! -- case 1: file has only one time step
!     IF (ntime == 1) THEN
!       IF (time(1) < 0._dp .OR. time(1) > 15000._dp) THEN
!         WRITE(message_text, *) 'Suspicious time value (', time(1), &
!                     '). File has only one time value. ',TRIM(efield%ef_file)
!         CALL message('ef_check_time', message_text)
!       END IF
!     END IF

!     ! -- case 2: monthly data with monthly units
!     IF (lmonthly) THEN
!       IF (ntime == 1) THEN
!         WRITE(message_text, *)  'Only one monthly time value in file. ',TRIM(efield%ef_file)
!         CALL message('ef_check_time', message_text)
!       ELSE IF (ntime /= 12) THEN
!         WRITE(message_text, *) ntime, ' time values with monthly time unit in file. ', &
!                               & TRIM(efield%ef_file)
!         CALL message('ef_check_time', message_text)
!       END IF
!       DO i=1,ntime-1
!         testval = time(i+1)-time(i)
!         IF (testval < 0._dp .OR. ABS(testval) /= 1._dp) THEN
!           WRITE(message_text, *) 'Suspicious time values at i=', i, i+1,  &
!                                 ' (', time(i),',', time(i+1), '). ',TRIM(efield%ef_file)
!           CALL message('ef_check_time', message_text)
!         END IF
!       END DO

!     ! -- case 3: presumably monthly data (daily units)
!     ELSE IF (ntime == 12) THEN 
!       DO i=1,ntime-1
!         testval = time(i+1)-time(i)
!         IF (testval < 0._dp .OR. ABS(testval) < 28._dp .OR. ABS(testval) > 31._dp) THEN
!           WRITE(message_text, *) 'Suspicious time interval at i=', i, i+1,  &
!                                 ' (', time(i),',', time(i+1), ').'
!           CALL message('ef_check_time', message_text)
!         END IF
!       END DO
!       IF (time(1) < 0._dp) THEN
!         WRITE(message_text, *) 'Suspicious time value <0.'
!         CALL message('ef_check_time', message_text)
!       END IF
!     END IF
 
!   END SUBROUTINE ef_check_time

! !---------------------------------------------------------------------------------------------------

  SUBROUTINE ef_get_actual_filename (current_date, efield, changed)

    !USE mo_util_timeconv,   ONLY: TC_get, write_date, add_date
    USE mtime_lite,              ONLY: datetime!, newDatetime, deallocateDatetime
    USE mo_run_config_lite,      ONLY: dtime  ! the mtime step
    
    IMPLICIT NONE

    TYPE(datetime), POINTER, INTENT(IN)   :: current_date 
    TYPE(external_field), INTENT(INOUT)   :: efield
    LOGICAL, INTENT(OUT)                  :: changed

    CHARACTER(LEN=512)                  :: efile
    TYPE(datetime), POINTER             :: previous_date
    INTEGER                             :: pyear, cyear, month, day, hour, minute, second

    changed = .FALSE.

    efile = expand_template(current_date, efield%ef_template, efield%ef_varname, .TRUE.)

    IF (efile /= efield%ef_file) THEN 
      efield%ef_file = efile
      changed = .TRUE.
    ENDIF

    IF (efield%ef_timedef == EF_IGNOREYEAR) THEN
      ! now compare year of last step with actual step
      !CALL TC_get(current_date, year=cyear)
      !CALL TC_get(current_date, year=pyear, lprevious=.TRUE.)
      cyear = current_date%date%year
      pyear = current_date%date%year
      ! ef_times have to be recalculated (and adjusted to the current year)!
      IF (cyear /= pyear) changed = .TRUE.
    ENDIF
  
  END SUBROUTINE ef_get_actual_filename

! !---------------------------------------------------------------------------------------------------

  SUBROUTINE ef_get_filename (jg,p_patch,current_date, efield, changed, ioffset, newyear)

    ! ioffset = -1 : get previous filename
    ! ioffset =  0 : get actual   filename
    ! ioffset =  1 : get next     filename

    USE mo_util_filename_lite,      ONLY: str_filter
    USE mtime_lite,                 ONLY: datetime, timedelta!, newDatetime,  deallocateDatetime, &
                                  !&  OPERATOR(+), OPERATOR(-),  OPERATOR(*),                &
                                 ! &  newTimedelta, deallocateTimedelta
    USE mo_model_domain_lite,       ONLY: t_patch
    USE mo_io_units_lite,           ONLY: filename_max
    !USE mo_util_timeconv,          ONLY: TC_get, add_date, TC_split

    IMPLICIT NONE

    INTEGER,                 INTENT(IN)           :: jg  
    TYPE(t_patch),           INTENT(IN)           :: p_patch
    TYPE(datetime), POINTER, INTENT(IN)           :: current_date
    INTEGER,                 INTENT(IN)           :: ioffset
    TYPE(external_field),    INTENT(INOUT)        :: efield
    LOGICAL,                 INTENT(OUT)          :: changed
    INTEGER,                 INTENT(OUT)          :: newyear 

    CHARACTER(LEN=512)                  :: efile
    CHARACTER(LEN=16)                   :: cnlev

    TYPE(datetime), POINTER             :: date
    TYPE(timedelta), POINTER            :: dtime
    CHARACTER(LEN=filename_max)         :: grid_name
    INTEGER                             :: year, month, day, hour, minute, second, &
                                            ndx, idays, nform, nlev
    !CALL TC_get(current_date,year=year)

    !date => newDatetime( current_date )
    year = current_date%date%year

    nlev = p_patch%nlev

    newyear = year
    changed = .FALSE.
    !IF (ioffset == 0) THEN
      CALL ef_get_actual_filename(current_date, efield, changed)
    !ELSE
      !grid_name=get_grid_name(1)  
    !  WRITE(*,*) 'nlev =', nlev
    !  write(cnlev,'(a1,i0)') 'L',nlev
    !  ndx = INDEX(efield%ef_template,'%S')
      ! IF (ndx > 0) THEN
      !   CALL add_date(date, seconds=ioffset)
      ! ELSE
      !   ndx = INDEX(efield%ef_template,'%I')
      !   IF (ndx > 0) THEN
      !     CALL add_date(date, minutes=ioffset)
      !   ELSE
      !     ndx = INDEX(efield%ef_template,'%H')
      !     IF (ndx > 0) THEN
      !       CALL add_date(date, hours=ioffset)
      !     ELSE
      !       ndx = INDEX(efield%ef_template,'%D')
      !       IF (ndx > 0) THEN
      !         CALL add_date(date, days=ioffset)
      !       ELSE
      !         ndx = INDEX(efield%ef_template,'%M')
      !         IF (ndx > 0) THEN
      !           CALL add_date(date, months=ioffset)
      !         ELSE
      !           ndx = INDEX(efield%ef_template,'%Y')
      !           IF (ndx > 0) THEN
      !             CALL add_date(date, years=ioffset) 
      !           ENDIF
      !         ENDIF
      !       ENDIF
      !     ENDIF
      !   ENDIF
      ! ENDIF
      ! CALL TC_split(date,year,month,day,hour,minute,second)
      ! efile = str_filter(efield%ef_template,year, month, day, hour, minute, second,0, &
      !                   & trim(efield%ef_varname),TRIM(grid_name),cnlev)
      ! IF (efile /= efield%ef_file) THEN 
      !   efield%ef_file = efile
      !   changed = .TRUE.
      ! ENDIF
    !END IF

    !CALL deallocateDatetime( date )
 
  END SUBROUTINE ef_get_filename

! !---------------------------------------------------------------------------------------------------

!   SUBROUTINE ef_get_next_timestep(current_date, jg, p_patch, efield)
 
!     USE mo_time_config,       ONLY: end_datetime_string 
!     USE mo_model_domain,      ONLY: t_patch
!     USE mtime,                ONLY: datetime, newDatetime 
!     USE mo_util_timeconv,     ONLY: TC_get, add_date, write_date

!     IMPLICIT NONE

!     TYPE(datetime),  POINTER, INTENT(IN)    :: current_date
!     INTEGER,                  INTENT(IN)    :: jg 
!     TYPE(t_patch),            INTENT(IN)    :: p_patch 
!     TYPE(external_field),     INTENT(INOUT) :: efield             

!     LOGICAL                      :: lchanged, lexist, lmonthly, lastindex
!     INTEGER                      :: newyear
!     INTEGER                      :: year, month, day, hour, minute, second

!     lexist = .TRUE.
!     lastindex = (efield%ef_timeindex == size(efield%ef_times))
!     CALL ef_get_filename (jg,p_patch,current_date, efield, lchanged, 0, newyear)
!     IF (efield%ef_timedef /= EF_CONSTANT) THEN
!       efield%ef_timeindex = 0
!       CALL ef_get_filetimes(jg, p_patch, current_date, efield,.TRUE.,lexist, lmonthly, newyear)

!       efield%ef_timeindex = ef_get_next_timeindex(jg, p_patch, current_date,                                         &
!                                                   efield%ef_timeindex, efield%ef_times, efield%ef_timetol, lmonthly, &
!                                                   efield%ef_varname                                                   )



!       IF (efield%ef_timeindex == 0) THEN
!         ! look for matching date in next file
!         CALL ef_get_filename (jg,p_patch,current_date, efield, lchanged, 1, newyear)
!         IF (lastindex .AND. (.NOT. lchanged) .AND. (efield%ef_timedef == EF_IGNOREYEAR)) THEN
!           CALL TC_get(current_date, year=year)
!           newyear=year+1
!         ENDIF
!         CALL ef_get_filetimes(jg, p_patch, current_date, efield, .FALSE., lexist, lmonthly, newyear)
!         IF (lexist) THEN
!           efield%ef_timeindex = ef_get_next_timeindex(jg, p_patch,         current_date,                 &
!                                                       0,  efield%ef_times, efield%ef_timetol, lmonthly,  &
!                                                       efield%ef_varname   ) 
!         ELSE
!           ! keep data until the end of the model run
!           efield%ef_timeindex = 1
!           ALLOCATE(efield%ef_times(1))
!           NULLIFY(efield%ef_times(1)%datetime)  
!           efield%ef_times(1)%datetime = newDatetime(end_datetime_string) 
!           CALL add_date(efield%ef_times(1)%datetime, days=1) 
!           call write_date(efield%ef_times(1)%datetime,'VVVVVVVVV') 
!           stop 554 
!         ENDIF
!       ENDIF
!     ENDIF

!     IF (efield%ef_timeindex == 0) THEN
!       IF (.NOT. lexist) THEN
!         CALL message('ef_get_next_timestep','Data file '//TRIM(efield%ef_file)//' does not exist!')
!       ELSE
!         CALL message('ef_get_next_timestep','No data found in file '//TRIM(efield%ef_file)//'!')
!       ENDIF
!     ENDIF

!   END SUBROUTINE ef_get_next_timestep

! !---------------------------------------------------------------------------------------------------

  SUBROUTINE ef_get_first_timestep(current_date, jg, p_patch, efield)
  
    !USE mo_master_config,       ONLY: experimentStopDate
    USE mtime_lite,                  ONLY: datetime!, newDatetime, deallocateDatetime
    USE mo_model_domain_lite,        ONLY: t_patch
    USE mo_subm_main_lite,           ONLY: emi_scenario
!    USE mo_mpi_lite,                 ONLY: my_process_is_mpi_workroot, p_pe, p_io, p_bcast, p_comm_work
    !USE mo_util_timeconv,       ONLY: add_date, write_date

    IMPLICIT NONE
  
    TYPE(datetime), POINTER,  INTENT(IN)    :: current_date
    INTEGER,                  INTENT(IN)    :: jg  
    TYPE(t_patch),            INTENT(IN)    :: p_patch 
    TYPE(external_field),     INTENT(INOUT) :: efield

    LOGICAL                   :: lchanged, lexist, lmonthly
    INTEGER                   :: newyear, ncid, varid, ndx_r, str_len
    CHARACTER(len=256)        :: str2

    lexist=.FALSE.

    ! after the next command a possibly existing scenario string (%R) is
    ! permanently replaced in efield%ef_template!
    ndx_r = INDEX(efield%ef_template,'%R')
    IF (ndx_r > 0) THEN
      str_len  = LEN_TRIM(TRIM(efield%ef_template))
      str2 = efield%ef_template(1:ndx_r-1) // TRIM(emi_scenario)// efield%ef_template(ndx_r+3:str_len)
      efield%ef_template = TRIM(str2)
    ENDIF

    IF (efield%ef_timedef == EF_CONSTANT) THEN
      ! check whether data file exists
      CALL ef_get_filename (jg, p_patch, current_date, efield, lchanged, 0, newyear)
      CALL ef_open (jg, p_patch, efield, ncid, .FALSE.)
      IF (ncid <= 0) THEN
        CALL message('ef_get_first_timestep','Data file '//TRIM(efield%ef_file)//' does not exist!')
        efield%ef_timeindex = 0
      ELSE
        ! read unit of field once (lfirst!)
        CALL nf_check(nf_inq_varid( ncid, efield%ef_varname, varid ),efield%ef_file)
        efield%ef_actual_unit=''
        CALL nf_check(nf_get_att_text(ncid, varid, 'units',efield%ef_actual_unit),efield%ef_file)
        ! keep data until the end of the model run 
        CALL nf_check( nf_close( ncid ) )
        ALLOCATE(efield%ef_times(efield%ef_timeindex))
        !efield%ef_times(efield%ef_timeindex)%datetime => newDatetime( experimentStopDate )
        !CALL add_date(efield%ef_times(efield%ef_timeindex)%datetime, days=1)
        lexist=.TRUE.
      ENDIF
    ELSE
      efield%ef_timeindex = 0
      CALL ef_get_filename (jg, p_patch, current_date, efield, lchanged, 0, newyear)
      CALL ef_get_filetimes(jg, p_patch, current_date, efield,.TRUE.,lexist, lmonthly, newyear)
      !IF (lexist) efield%ef_timeindex = 1!ef_get_first_timeindex(current_date, efield%ef_times, efield%ef_timetol, lmonthly, &
      !                                            efield%ef_varname)

      IF (efield%ef_timeindex == 0) THEN
        ! look for matching date in "previous" file
        CALL ef_get_filename (jg, p_patch, current_date, efield, lchanged, -1, newyear)
        IF (.NOT. lchanged) newyear = newyear - 1
        CALL ef_get_filetimes(jg, p_patch, current_date, efield,.TRUE.,lexist, lmonthly, newyear)
        IF (lexist) efield%ef_timeindex = 1!ef_get_first_timeindex(current_date, efield%ef_times, efield%ef_timetol, lmonthly, &
        !                                            efield%ef_varname)
      ENDIF
      ! read unit of field once (lfirst!)
!      IF ( p_pe == p_io ) THEN
        CALL ef_open (jg, p_patch, efield, ncid, .FALSE.)
        CALL nf_check(nf_inq_varid( ncid, efield%ef_varname, varid ),efield%ef_file)
        efield%ef_actual_unit=''
        CALL nf_check(nf_get_att_text(ncid, varid, 'units',efield%ef_actual_unit),efield%ef_file)
        CALL nf_check( nf_close( ncid ) )
!      END IF
      
!      CALL p_bcast(efield%ef_actual_unit,     p_io, p_comm_work)
    ENDIF

    IF (efield%ef_timeindex == 0) THEN
      IF (.NOT. lexist) THEN
        CALL message('ef_get_first_timestep','Data file '//TRIM(efield%ef_file)//' does not exist!')
      ELSE
        CALL message('ef_get_first_timestep','No data found in file '//TRIM(efield%ef_file)//'!')
      ENDIF
    ENDIF

  END SUBROUTINE ef_get_first_timestep

! !---------------------------------------------------------------------------------------------------

  SUBROUTINE ef_get_filetimes (jg, p_patch, current_date, efield, labort, lexist, lmonthly, newyear)
 
    USE mo_model_domain_lite,    ONLY: t_patch

    IMPLICIT NONE
    
    INTEGER,              INTENT(IN)    :: jg  
    INTEGER,              INTENT(IN)    :: newyear
    TYPE(t_patch),        INTENT(IN)    :: p_patch 
    TYPE(datetime),       INTENT(IN)    :: current_date
    LOGICAL,              INTENT(IN)    :: labort
    TYPE(external_field), INTENT(INOUT) :: efield
    LOGICAL,              INTENT(OUT)   :: lexist
    LOGICAL,              INTENT(OUT)   :: lmonthly

    INTEGER               :: ncid, dimlen, dimid, varid
    REAL(dp), ALLOCATABLE :: filetimes(:)
    CHARACTER (len = 80)  :: units

    lexist = .FALSE.
    IF (ALLOCATED(efield%ef_times)) DEALLOCATE(efield%ef_times)
    CALL ef_open (jg, p_patch, efield, ncid, .TRUE.)
    IF (ncid > 0) THEN 
      lexist = .TRUE.
      CALL nf_check(nf_inq_dimid  (ncid, "time", dimid))
      CALL nf_check(nf_inq_dimlen (ncid, dimid, dimlen))
      ALLOCATE(filetimes(dimlen))
      ALLOCATE(efield%ef_times(dimlen))
      CALL nf_check(nf_inq_varid( ncid, "time", varid ),efield%ef_file)
      CALL nf_check (nf_get_var_double(ncid,varid,filetimes),efield%ef_file)
      filetimes = filetimes + efield%ef_timeoffset
      units=''
      CALL nf_check(nf_get_att_text(ncid, varid, 'units', units),efield%ef_file)
      CALL nf_check( nf_close( ncid ) )
    ELSE 
      IF (labort) THEN
        CALL message('ef_get_filetimes', "file "//trim(efield%ef_file)//" does not exist!")
      ELSE
        CALL message('ef_get_filetimes', "file "//trim(efield%ef_file)//" does not exist!")
        CALL message('ef_get_filetimes', "old data will be used until the end of the model run!")
      ENDIF 
      RETURN
    ENDIF
 
    ! CALL ef_convert_times(dimlen,filetimes,trim(units),efield%ef_timedef,.TRUE.,efield%ef_times, lmonthly)
    !CALL ef_convert_times(current_date, dimlen,filetimes,trim(units),efield%ef_timedef,.FALSE.,efield%ef_times, lmonthly,newyear)
    !CALL ef_check_time(efield, filetimes, lmonthly)      
    DEALLOCATE(filetimes)
    
  END SUBROUTINE ef_get_filetimes

! !---------------------------------------------------------------------------------------------------

!   FUNCTION ef_get_next_timeindex(jg, p_patch, current_date, nstart, times, timetol, lmonthly, vname) RESULT (nind)

!     USE mo_model_domain,            ONLY: t_patch
!     USE mtime,                      ONLY: datetime, OPERATOR(<)
!     USE mo_util_timeconv,           ONLY: TC_get, write_date 

!     IMPLICIT NONE

!     INTEGER, INTENT(IN)                   :: jg  
!     INTEGER, INTENT(IN)                   :: nstart
!     REAL(dp), INTENT(IN)                  :: timetol
!     LOGICAL, INTENT(IN)                   :: lmonthly
!     CHARACTER (len = *), INTENT(IN)       :: vname
!     TYPE(datetimes), INTENT(IN)           :: times(:)
!     TYPE(t_patch),            INTENT(IN)  :: p_patch    
!     TYPE(datetime),  POINTER, INTENT(IN)  :: current_date

!     INTEGER             :: i, nind, dimlen
!     INTEGER             :: idayc, idayf, idummy
!     LOGICAL             :: lfound
!     CHARACTER (len = 4) :: cind

!     nind = 0 
!     lfound = .FALSE. 
!     i = nstart + 1
!     dimlen = SIZE(times)
!     DO WHILE ((.NOT. lfound) .AND. (i <= dimlen))
!       IF (current_date < times(i)%datetime ) THEN
!         nind = i
!         lfound = .TRUE.
!         write(cind,'(i4)') nind
!         CALL write_date(times(nind)%datetime,'.........next reading for '//TRIM(vname)//' will be done at index '//&
!                         TRIM(cind)//' and file date: ')
!       ENDIF
!       i = i + 1
!     ENDDO
 
!   END FUNCTION ef_get_next_timeindex

! !---------------------------------------------------------------------------------------------------

  ! FUNCTION ef_get_first_timeindex(current_date, times, timetol, lmonthly, vname) RESULT (nind)

  !   USE mtime, ONLY: OPERATOR(==), OPERATOR(<), OPERATOR(>)
  !   USE mo_util_timeconv, ONLY: TC_get, write_date 

  !   IMPLICIT NONE
    
  !   REAL(dp), INTENT(IN)                :: timetol
  !   LOGICAL, INTENT(IN)                 :: lmonthly
  !   CHARACTER (len = *), INTENT(IN)     :: vname
  !   TYPE(datetimes), INTENT(IN)         :: times(:)
  !   TYPE(datetime), POINTER, INTENT(IN) :: current_date

  !   INTEGER           :: i, nind, dimlen
  !   INTEGER           :: idayc, idayf, imonthc, imonthf
  !   LOGICAL           :: lfound
  !   CHARACTER (len=4) :: cind

  !   nind = 0  
  !   lfound = .FALSE. 
  !   i = 1
  !   dimlen = SIZE(times)

  !   DO WHILE ((.NOT. lfound) .AND. (i <= dimlen))
  !     IF ((current_date < times(i)%datetime) .OR. (current_date == times(i)%datetime)) THEN 
  !       IF (current_date == times(i)%datetime) THEN 
  !         nind = i
  !       ELSE 
  !         nind = i - 1
  !       ENDIF
  !       lfound = .TRUE.
  !     ENDIF 
  !     CALL TC_get(current_date, day=idayc, month=imonthc)
  !     CALL TC_get(times(i)%datetime, day=idayf, month=imonthf)
  !     IF (lmonthly .AND. (nind == 0) .AND. (ABS(idayc-idayf) < timetol) .AND. & 
  !         imonthc == imonthf ) THEN 
  !       nind = i
  !       lfound = .TRUE.
  !     ENDIF 
  !     i= i + 1
  !   ENDDO
  !   IF ((i > dimlen) .AND. (current_date > times(dimlen)%datetime)) nind = dimlen
  !   IF (nind /= 0) THEN
  !     write(cind,'(i4)') nind
  !     CALL write_date(times(nind)%datetime,'.........first reading for '//TRIM(vname)//' is done at index '//&
  !                     TRIM(cind)//' and file date: ')
  !   ENDIF
  
  ! END FUNCTION ef_get_first_timeindex

! !---------------------------------------------------------------------------------------------------

  FUNCTION expand_template (current_date, template,spec,ltime) RESULT (efile)

    !USE mo_util_timeconv,       ONLY: TC_split
    USE mo_run_config_lite,          ONLY: nlev
    USE mo_util_filename_lite,       ONLY: str_filter
    USE mo_io_units_lite,            ONLY: filename_max
    USE mtime_lite,                  ONLY: datetime 

    IMPLICIT NONE

    TYPE(datetime), POINTER, INTENT(IN) :: current_date
    CHARACTER(LEN=512),      INTENT(IN) :: template
    CHARACTER(LEN=512),      INTENT(IN) :: spec
    LOGICAL,                 INTENT(IN) :: ltime

    CHARACTER(LEN=512)             :: efile
    INTEGER                        :: ndx, year, month, day, hour, minute, second, nform
    CHARACTER(LEN=filename_max)    :: grid_name

    CHARACTER(LEN=16)         :: cnlev

    ndx = INDEX(template,'%')

    IF (ndx > 0) THEN

       grid_name=get_grid_name(1)  

       WRITE(cnlev,'(a1,i0)') 'L',nlev

      IF (ltime) THEN
       !CALL TC_split(current_date, year, month, day, hour, minute, second)
       year = current_date%date%year
       month = current_date%date%month
       day = current_date%date%day
       hour = current_date%time%hour
       minute = current_date%time%minute
       second = current_date%time%second
       efile = str_filter(template,year, month, day, hour, minute, second,0,TRIM(spec),TRIM(grid_name),cnlev)
      ELSE
       efile = str_filter(template,0, 0, 0, 0, 0, 0,0,TRIM(spec),TRIM(grid_name),cnlev)
      ENDIF
    ELSE
      efile=template
    ENDIF
    
  END FUNCTION expand_template

! !---------------------------------------------------------------------------------------------------

  FUNCTION get_grid_name ( jg ) RESULT ( gridname )
 
    USE mo_io_units_lite,           ONLY: filename_max
    USE mo_hammoz_lite_config,        ONLY: dynamics_grid_filename

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: jg  

    INTEGER :: len
    CHARACTER(LEN=filename_max) :: filename, gridname

    filename=dynamics_grid_filename(jg)

    len = INDEX(TRIM(filename),'.nc', BACK= .TRUE.)-1
    IF ( len .LT. 0 ) THEN
       CALL finish(thismodule, 'ef_get_grid_name: expected .nc file ')
    END IF

    gridname=filename(1:len)

  END FUNCTION get_grid_name

! !---------------------------------------------------------------------------------------------------

  SUBROUTINE ef_open(jg, p_patch, efield,io_file_id,lwith_check)
  
    USE mo_model_domain_lite,             ONLY: t_patch

    IMPLICIT NONE

    INTEGER,              INTENT(IN)    :: jg  
    TYPE(t_patch),        INTENT(IN)    :: p_patch 
    TYPE(external_field), INTENT(INOUT) :: efield
    INTEGER,              INTENT(OUT)   :: io_file_id
    LOGICAL, INTENT(in)                 :: lwith_check

    LOGICAL                           :: lexist

    IF (trim(efield%ef_template) /= '') THEN
      INQUIRE (file=efield%ef_file, exist=lexist)
      IF (lexist) THEN
        CALL nf_check( nf_open( TRIM(efield%ef_file), NF_NOWRITE, io_file_id ), TRIM(efield%ef_file))
        IF (lwith_check) CALL ef_check( jg, p_patch, efield, io_file_id )
      ELSE
        io_file_id = -1
      ENDIF
    ELSE
      CALL message('ef_open', 'no template name defined!')
    ENDIF

  END SUBROUTINE ef_open

! !---------------------------------------------------------------------------------------------------

!   SUBROUTINE ef_get_full_and_half_level_information (jg, p_patch, efield, nzval, clevname, lreverse, lispressure, llevel)
 
!     USE mo_model_domain,  ONLY: t_patch
!     USE mo_mpi,           ONLY: my_process_is_mpi_workroot, p_pe, p_io, p_bcast, p_comm_work 

!     IMPLICIT NONE

!     INTEGER,              INTENT(IN)    :: jg   
!     TYPE(t_patch),        INTENT(IN)    :: p_patch 
!     INTEGER, INTENT(IN)                 :: nzval
!     CHARACTER(len=5),INTENT(IN)         :: clevname
!     LOGICAL, INTENT(OUT)                :: lreverse, lispressure, llevel
!     TYPE(external_field), INTENT(INOUT) :: efield

!     INTEGER               :: nlev, iret, io_file_id, io_var_id, ilev
!     REAL(dp), ALLOCATABLE :: zzvalues(:), zzvaltemp(:)
!     CHARACTER(len=80)     :: units
!     CHARACTER(LEN=128)    :: clunits ! local copy with minimum length

!     ! get values of levels (altitude or pressure values)
!     nlev=p_patch%nlev

!     lreverse = .FALSE.
!     llevel = .FALSE.
!     ALLOCATE(zzvalues(nzval))
!     IF ( p_pe == p_io ) THEN
!       CALL ef_open (jg, p_patch, efield, io_file_id, .FALSE.)
!       iret = nf_inq_varid( io_file_id, TRIM(clevname), io_var_id )
!       IF (iret /= 0 ) CALL nf_check( nf_inq_varid( io_file_id, "lev", io_var_id ),efield%ef_file)
!       CALL nf_check (nf_get_var_double(io_file_id,io_var_id,zzvalues),efield%ef_file)
!       units=' '
!       CALL nf_check(nf_get_att_text(io_file_id, io_var_id, 'units', units),efield%ef_file)
!       CALL nf_check(nf_close(io_file_id))
!     END IF
!     CALL p_bcast(zzvalues,  p_io, p_comm_work)
!     CALL p_bcast(units,  p_io, p_comm_work)
   
!     clunits = TRIM(units)//'                             '
!     IF (clunits(1:2) == "km") THEN
!       ! convert from km to m
!       zzvalues = zzvalues * 1000._dp
!     ELSE
!       IF (clunits(1:3) == "hPa") THEN
!         ! convert from hPa to Pa 
!         zzvalues = zzvalues * 100._dp
!       ENDIF
!     ENDIF
  
!     IF ((clunits(1:2) == "km") .or. (clunits(1:2) == "m ")) THEN
!       lispressure = .FALSE.
!       IF (zzvalues(1) < zzvalues(2)) THEN 
!         lreverse = .TRUE.
!         ALLOCATE(zzvaltemp(nzval))
!         zzvaltemp = zzvalues
!         DO ilev = 1, nzval
!           zzvalues(ilev) = zzvaltemp(nzval - ilev + 1) 
!         ENDDO
!         DEALLOCATE (zzvaltemp)
!       ENDIF
          
!       ! calculate half levels (in the arithmetic middle of the full levels
!       IF (ALLOCATED(efield%ef_zvalueh)) DEALLOCATE(efield%ef_zvalueh)
!       ALLOCATE(efield%ef_zvalueh(nzval+1))
!       efield%ef_zvalueh(nzval+1) = 0._dp
!       DO ilev=nzval,2,-1
!         efield%ef_zvalueh(ilev) = (zzvalues(ilev-1) + zzvalues(ilev))/2._dp
!       ENDDO
!       efield%ef_zvalueh(1) = efield%ef_zvalueh(2) + (zzvalues(1) - efield%ef_zvalueh(2))*2._dp
!       DEALLOCATE(zzvalues)
!     ELSE
!       IF ((clunits(1:3) == "hPa") .or. (clunits(1:2) == "Pa")) THEN
!         lispressure = .TRUE.
!         IF (zzvalues(2) < zzvalues(1)) THEN  
!           lreverse = .TRUE.
!           ALLOCATE(zzvaltemp(nzval))
!           zzvaltemp = zzvalues
!           DO ilev = 1, nzval
!             zzvalues(ilev) = zzvaltemp(nzval - ilev + 1) 
!           ENDDO
!           DEALLOCATE (zzvaltemp)
!         ENDIF
              
!         ! calculate half levels (in the arithmetic middle of the full levels
!         IF (ALLOCATED(efield%ef_zvalueh)) DEALLOCATE(efield%ef_zvalueh)
!         ALLOCATE(efield%ef_zvalueh(nzval+1))
!         efield%ef_zvalueh(1) = 0._dp
!         DO ilev=2,nzval
!           efield%ef_zvalueh(ilev) = (zzvalues(ilev-1) + zzvalues(ilev))/2._dp
!         ENDDO
!         efield%ef_zvalueh(nzval+1) = efield%ef_zvalueh(nzval) + (zzvalues(nzval) - efield%ef_zvalueh(nzval))*2._dp
!         DEALLOCATE(zzvalues)
!       ELSE
!         IF ((clunits(1:5) == "level") .or. (clunits(1:1) == "1")) THEN
!           CALL message('ef_get_full_and_half_level_information', &
!                       'Vertical coordinate system given in model levels. This is still deprecated!')
!           IF (nzval /= nlev) THEN
!             CALL message('ef_get_full_and_half_level_information',  &
!               'number of model levels of input file do not fit!')
!           ELSE
!             llevel = .TRUE.
!           ENDIF
!         ELSE
!           CALL message('ef_get_full_and_half_level_information', 'vertical coordinate system unknown!')
!         ENDIF
!       ENDIF
!     ENDIF

!   END SUBROUTINE ef_get_full_and_half_level_information

! !---------------------------------------------------------------------------------------------------

!   SUBROUTINE ef_get_levname_and_size(filename, clevname,nzval)
    
!     USE mo_util_netcdf77, ONLY: read_diml_nf77
!     USE mo_mpi,           ONLY: my_process_is_mpi_workroot, p_pe, p_io, p_bcast, p_comm_work

!     IMPLICIT NONE

!     CHARACTER(LEN=*), INTENT(IN)  :: filename
!     CHARACTER(LEN=5), INTENT(OUT) :: clevname
!     INTEGER, INTENT(OUT)          :: nzval

!     clevname = "lev"
!     IF ( p_pe == p_io ) THEN 
!       nzval=read_diml_nf77(filename,TRIM(clevname))
!     END IF
!     CALL p_bcast(nzval,     p_io, p_comm_work)
!     IF (nzval < 0) THEN
!       clevname = "hlev"
!       nzval=read_diml_nf77(filename,TRIM(clevname))
!     ENDIF

!   END SUBROUTINE ef_get_levname_and_size

! !---------------------------------------------------------------------------------------------------

  SUBROUTINE ef_read_single(efield, value)

    USE mo_util_netcdf77_lite, ONLY: read_var_hs_nf77_0d  

    IMPLICIT NONE

    TYPE(external_field), INTENT(inout) :: efield
    REAL(dp),             INTENT(out)   :: value
    INTEGER                             :: ierr

    call read_var_hs_nf77_0d(efield%ef_file, "time", efield%ef_timeindex, trim(efield%ef_varname), value, ierr)
    value=value*efield%ef_factor
    CALL message('ef_read_single', 'new single value read for variable '//TRIM(efield%ef_varname) &
                //' from file '//TRIM(efield%ef_file))

  END SUBROUTINE ef_read_single

! !---------------------------------------------------------------------------------------------------

  SUBROUTINE nf_check(status, fname) 

    USE mo_exception_lite, ONLY: finish

    IMPLICIT NONE
    
    INTEGER :: status
    CHARACTER(len=*), OPTIONAL :: fname

    IF (status /= nf_noerr) THEN
      IF (PRESENT(fname)) THEN
        CALL finish('netcdf error in '//TRIM(fname),nf_strerror(status))
      ELSE
        CALL finish('netcdf error',nf_strerror(status))
      END IF
    ENDIF

  END SUBROUTINE nf_check

END MODULE mo_util_extfield_lite
