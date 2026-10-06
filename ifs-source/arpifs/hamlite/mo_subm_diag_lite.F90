! 
! based on mo_submodel_diag.f90 
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

MODULE mo_subm_diag_lite

  USE mo_kind_lite,                  ONLY: dp
  USE mo_exception_lite,             ONLY: finish, message, message_text
  !USE mo_var_list,              ONLY: t_var_list => t_var_list_ptr
  USE mo_subm_trac_para_lite,        ONLY: ln 

#ifndef _OPENACC
#define __acc_attach(ptr)
#endif

  IMPLICIT NONE
  PRIVATE
 
  !--------------------
  ! module interface
  !--------------------

  ! PUBLIC  :: t_diag, t_diag_list    ! diagnostic structures
  ! PUBLIC  :: new_diag_list          ! subroutine for creating a new diagnostic list
  ! PUBLIC  :: new_diag               ! subroutine for defining diagnostic entries
  ! PUBLIC  :: new_diag_element       ! subroutine for defining a single diagnostic entry
  ! PUBLIC  :: get_diag_pointer       ! subroutine to retrieve diagnostics
  ! PUBLIC  :: get_diag_index         ! return index of a specific diag in diag_list 

  PUBLIC  :: OFF, ON, BYTRACER, BYSPECIES, BYMODE, BYNUMMODE, BYUSER  ! key types      

  ! INTERFACE get_diag_pointer
  !   MODULE PROCEDURE get_diag_pointer2d
  !   MODULE PROCEDURE get_diag_pointer3d
  ! END INTERFACE 
  
  !--------------------
  ! module parameters
  !--------------------

  INTEGER, PARAMETER    :: OFF       = 0,  &
                           ON        = 1,  &
                           BYTRACER  = 1,  &
                           BYSPECIES = 2,  &
                           BYMODE    = 3,  &
                           BYNUMMODE = 4,  &
                           BYUSER    = 5

  !--------------------
  ! module variables
  !--------------------
 
  TYPE :: t_diag                         ! 2-D or 3-D output field plus meta data
     INTEGER            :: key_type      ! Output type: 
                                         !   1 = by tracer
                                         !   2 = by species
                                         !   3 = by aerosol mode or bin (mass)
                                         !   4 = by aerosol mode or bin (number)
                                         !   5 = by anything else
     INTEGER            :: key           ! index to tracer, species, mode etc.
     INTEGER            :: ndims         ! number of dimensions for this diag
     CHARACTER (LEN=ln) :: name          ! name of the diagnostic field
     REAL(dp), POINTER  :: fld2d(:,:)
     REAL(dp), POINTER  :: fld3d(:,:,:)
  END TYPE t_diag

  TYPE t_diag_list                       ! list of t_diag elements
     INTEGER            :: ndims         ! number of dimensions fo rthe entire list (2 or 3)
     INTEGER            :: ndiag         ! number of diagnostic entries
     INTEGER            :: nmaxdiag      ! max. number of allowed diag entries (automatic!)
     INTEGER            :: nmaxkey(5)    ! maximum allowed value for each key_type 
     CHARACTER (len=ln) :: diag_list_name
     CHARACTER (len=64) :: longname      ! default variable longname template
     CHARACTER (len=64) :: units         ! default variable units
     LOGICAL            :: lpost         ! include in output
     LOGICAL            :: lrestart      ! include in restart file
     !TYPE(t_var_list), POINTER    :: tstream       ! stream reference
     TYPE(t_diag), ALLOCATABLE    :: diag(:)
     INTEGER           :: table          ! grib table
  END TYPE t_diag_list    

  CONTAINS

! !---------------------------------------------------------------------------------------------------

!   SUBROUTINE new_diag_list( diag_list, tstream, diagname,                           &
!                             longname, units, ndims, nmaxkey, lpost, lrestart, table )

!     USE mo_subm_spec,             ONLY: nspec       ! for default list size
!     USE mo_subm_trac,             ONLY: ntrac       ! for default list size

!     ! -- parameters
!     TYPE(t_diag_list),           INTENT(inout)   :: diag_list
!     TYPE(t_var_list),  POINTER,  INTENT(inout)   :: tstream    ! pointer reference to existing stream
!     CHARACTER(len=*),  OPTIONAL, INTENT(in)      :: diagname   ! name of the diagnostic type 
!     CHARACTER(len=*),  OPTIONAL, INTENT(in)      :: longname   ! descriptive name of diagnostics: "of XY" wil be added
!     CHARACTER(len=*),  OPTIONAL, INTENT(in)      :: units      ! physical unit for output
!     INTEGER,           OPTIONAL, INTENT(in)      :: ndims      ! dimensionality of diagnostics (2 or 3-D)
!     INTEGER,           OPTIONAL, INTENT(in)      :: nmaxkey(5) ! maximum number of key values for each of
!                                                               ! the five key types
!     LOGICAL,           OPTIONAL, INTENT(in)      :: lpost      ! include in output (default=true)
!     LOGICAL,           OPTIONAL, INTENT(in)      :: lrestart   ! include in restart (default=true)
!     INTEGER,           OPTIONAL, INTENT(in)      :: table      ! grib table
  
!     ! -- local variables
!     CHARACTER(len=ln)                :: cdiagname, csubmname
!     CHARACTER(len=64)                :: clongname, cunits
!     INTEGER                          :: idims, imaxkey(5)
!     INTEGER                          :: imaxdiag
!     INTEGER                          :: ctable

!     ! -- executable code

!     ! 1) -- set defaults
!     cdiagname   = 'unknown'
!     csubmname   = 'unknown'
!     clongname   = ''
!     cunits      = ''
!     idims       = 2          ! set 2-D as default
!     imaxkey     = (/ ntrac, nspec, 0, 0, 0 /)
!     ctable=255
  
!     ! 1) -- check input arguments and set local variables
    
!     IF (PRESENT(diagname))   cdiagname   = diagname 
!     IF (PRESENT(longname))   clongname   = longname
!     IF (PRESENT(units))      cunits      = units
!     IF (PRESENT(ndims))      idims       = ndims
!     IF (PRESENT(nmaxkey))    imaxkey     = nmaxkey
!     IF (PRESENT(table))      ctable      = table

!     ! -- error checks
!     IF (idims /= 2 .AND. idims /= 3) THEN
!       CALL message('new_diag_list', TRIM(cdiagname)//': Invalid value of ndims. Only 2 or 3 allowed!')
!       idims = 2      ! this should get the code through the initialisation after which it stops
!     END IF

!     imaxdiag = MAXVAL(imaxkey)  ! maximum allowed number of diagnostics 
!     IF (imaxdiag > ntrac) THEN
!       write(message_text,*) 'cdiagname=',TRIM(cdiagname),                    &
!                             ': Invalid value in nmaxval (=',imaxdiag,')! ',  &
!                             'Only up to ntrac (=',ntrac,') allowed!'
!       CALL message('new_diag_list', message_text)
!     END IF

!     ! 2) -- associate diagnostics with existing stream or create new stream
!     IF (ASSOCIATED(tstream)) THEN  
!       write(message_text,*) 'Defining diagnostics type ', TRIM(cdiagname), &
!           &                 ' using stream ', TRIM(tstream%p%vlname)
!       CALL message('new_diag_list', message_text)
!     ELSE
!       CALL finish('new_diag_list', 'Routine requires a valid stream reference!')
!     END IF

!     ! 3) -- set t_diag_list structure fields
!     diag_list%ndims          = idims
!     diag_list%ndiag          = 0
!     diag_list%nmaxdiag       = imaxdiag
!     diag_list%nmaxkey        = imaxkey 
!     diag_list%longname       = clongname
!     diag_list%units          = cunits
!     diag_list%diag_list_name = cdiagname
!     diag_list%tstream        => tstream
!     diag_list%lpost          = .true.
!     diag_list%lrestart       = .true.
!     IF (PRESENT(lpost)) THEN
!       diag_list%lpost        = lpost
!     END IF
!     IF (PRESENT(lrestart)) THEN
!       diag_list%lrestart     = lrestart
!     END IF
!     diag_list%table          = ctable

!     ! 4) -- allocate diag field

!     IF (ALLOCATED(diag_list%diag)) CALL finish('new_diag_list', 'diag field already allocated!')
!     ALLOCATE (diag_list%diag(imaxdiag)) 

!   END SUBROUTINE new_diag_list    

! !---------------------------------------------------------------------------------------------------

!   SUBROUTINE new_diag_element(diag_list, diagname, ikey_type, ikey,     &
!                               longname, units, lpost, lrestart, shape2D, shape3D)

!     USE mo_cf_convention,       ONLY: t_cf_var
!     USE mo_cdi_constants,       ONLY: GRID_UNSTRUCTURED_CELL, GRID_CELL
!     USE mo_zaxis_type,          ONLY: ZA_HYBRID, ZA_HYBRID_HALF,         &
!       &                               ZA_SURFACE
!     USE mo_grib2,               ONLY: t_grib2_var, grib2_var
!     USE mo_io_config,           ONLY: lnetcdf_flt64_output
!     USE mo_cdi,                 ONLY: DATATYPE_PACK16, DATATYPE_PACK24,  &
!                                       DATATYPE_FLT32,  DATATYPE_FLT64,   &
!       &                               GRID_UNSTRUCTURED 
!     USE mo_var_metadata,        ONLY: create_vert_interp_metadata, vintp_types   
!     USE mo_impl_constants,      ONLY: VINTP_METHOD_LIN
!     USE mo_var_list,            ONLY: add_var
!     USE mo_cdi,                 ONLY: TSTEP_INSTANT,TSTEP_ACCUM
!     USE mo_util_string,         ONLY: tolower

!     ! -- parameters
!     TYPE(t_diag_list),          INTENT(inout)   :: diag_list
!     CHARACTER(len=*),           INTENT(in)      :: diagname   ! name of the diagnostic variable
!     INTEGER,                    INTENT(in)      :: ikey_type  ! key type (1 to 4)
!     INTEGER,                    INTENT(in)      :: ikey       ! tracer-, species- or mode-number
!     CHARACTER(len=*), OPTIONAL, INTENT(in)      :: longname   ! descriptive name of diagnostics
!     CHARACTER(len=*), OPTIONAL, INTENT(in)      :: units      ! physical unit for output
!     LOGICAL,          OPTIONAL, INTENT(in)      :: lpost      ! add diagnostics to file output
!     LOGICAL,          OPTIONAL, INTENT(in)      :: lrestart   ! add diagnostics to restart file
!     INTEGER,          OPTIONAL, INTENT(in)      :: shape2d(2) ! shape of 2-D output array (kproma, kblks)
!     INTEGER,          OPTIONAL, INTENT(in)      :: shape3d(3) ! shape of 3-D output array (/kproma, klev, kblks/)

!     ! -- local variables
!     INTEGER               :: i, jt
!     LOGICAL               :: lok, lopost, lorestart
!     CHARACTER(len=64)     :: clongname, cunits
!     CHARACTER(len=32)     :: cdebug, cdebug2
    
!     TYPE(t_cf_var)    :: cf_desc
!     TYPE(t_grib2_var) :: grib2_desc

!     INTEGER :: ibits, iextbits
!     INTEGER :: datatype_flt

!     INTEGER :: cshape2d(2), cshape3d(3)
    
!     ibits = DATATYPE_PACK16
!     iextbits = DATATYPE_PACK24

!     IF ( lnetcdf_flt64_output ) THEN
!       datatype_flt = DATATYPE_FLT64
!     ELSE
!       datatype_flt = DATATYPE_FLT32
!     END IF

!     ! 1) -- error checks
!     lok = .true. 
!     IF (ikey_type < 1 .OR. ikey_type > 5) THEN
!       CALL message('new_diag_element', diag_list%diag_list_name//': Invalid value of key_type (1..5)!')
!       lok = .false.
!     ELSE IF (ikey < 1 .OR. ikey > diag_list%nmaxkey(ikey_type)) THEN
!       write (message_text, *) diag_list%diag_list_name//': Invalid key value ',ikey,  &
!                               '. For key_type ',ikey_type, &
!                               ' the maximum allowed value is ',diag_list%nmaxkey(ikey_type)
!       CALL message('new_diag_element', message_text)
!       lok = .false.
!     END IF
!     IF (diag_list%ndiag == diag_list%nmaxdiag) THEN
!       write (message_text, *) 'Diagnostics list '//diag_list%diag_list_name        &
!                               //' full. Maximum allowed number of diag entries is ',diag_list%nmaxdiag
!       CALL message('new_diag_element', message_text)
!       CALL message('', 'Check the nmaxkey values in the call to new_diag_element_list.')
!       lok = .false.
!     END IF
 
!     DO jt=1,diag_list%ndiag
!       IF (TRIM(diag_list%diag(jt)%name) == TRIM(diagname)) THEN
!           lok = .FALSE.
!           EXIT
!       ENDIF
!     ENDDO 

!     IF (.NOT. lok) RETURN

!     !-- optional arguments
!     IF (PRESENT(longname)) THEN
!       clongname = TRIM(longname)
!     ELSE
!       clongname = TRIM(diag_list%longname)//' of '//TRIM(diagname)
!     END IF
!     IF (PRESENT(units)) THEN
!       cunits = units
!     ELSE
!       cunits = diag_list%units
!     END IF
!     cshape2D(:)=-999
!     cshape3D(:)=-999
!     IF ( PRESENT(shape2D) ) THEN 
!       cshape2D=shape2D
!     END IF
!     IF ( PRESENT(shape3D) ) THEN 
!       cshape3D=shape3D
!     END IF
!     IF (diag_list%ndims == 2 .AND. cshape2D(1) .EQ. -999 ) THEN
!       CALL finish('new_diag_element', 'need 2D array shape as argument') 
!     END IF
!     IF (diag_list%ndims == 3 .AND. cshape3D(1) .EQ. -999 ) THEN
!       CALL finish('new_diag_element', 'need 3D array shape as argument') 
!     END IF

!     ! 2) -- increase number of diagnostics and populate diag element
!     diag_list%ndiag = diag_list%ndiag + 1
!     i = diag_list%ndiag
!     diag_list%diag(i)%name     = diagname
!     diag_list%diag(i)%key_type = ikey_type
!     diag_list%diag(i)%key      = ikey

!     ! 3) -- add stream element and associate pointer
!     lopost = diag_list%lpost              ! set default
!     IF (PRESENT(lpost))  lopost = lpost   ! overwrite if given explicitly
!     lorestart = diag_list%lrestart              ! set default
!     IF (PRESENT(lrestart))  lorestart = lrestart   ! overwrite if given explicitly

!     IF (ikey_type == BYTRACER)  cdebug = 'BYTRACER'
!     IF (ikey_type == BYSPECIES) cdebug = 'BYSPECIES'
!     IF (ikey_type == BYMODE)    cdebug = 'BYMODE'
!     IF (ikey_type == BYNUMMODE) cdebug = 'BYNUMMODE'
!     IF (ikey_type == BYUSER)    cdebug = 'BYUSER'
!     cdebug2 = ', lpost=.true. (default)'
!     IF (PRESENT(lpost)) THEN
!       IF (lpost) THEN
!         WRITE(cdebug2,*) ', lpost=.true.'
!       ELSE
!         WRITE(cdebug2,*) ', lpost=.false.'
!       END IF
!     END IF

!     IF (diag_list%ndims == 2) THEN

!       WRITE(message_text, '(A,A24,1x,4(A))') 'defining new 2D diagnostics ', TRIM(diagname), &
!                           TRIM(cdebug), ': units=', TRIM(cunits), TRIM(cdebug2)
!       CALL message('', message_text)

!       cf_desc    = t_cf_var( diagname, cunits, clongname, datatype_flt)
!       grib2_desc = grib2_var(diag_list%table, 255, 255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!       CALL add_var( diag_list%tstream, tolower(diagname), diag_list%diag(i)%fld2d,            &
!                   & GRID_UNSTRUCTURED_CELL, ZA_SURFACE, cf_desc, grib2_desc, ldims=cshape2d, lopenacc=.TRUE.)
!       __acc_attach(diag_list%diag(i)%fld2d)

!       diag_list%diag(i)%ndims = 2
!     ELSE
!       WRITE(message_text, '(A,A24,1x,4(A))') 'defining new 3D diagnostics ', TRIM(diagname), &
!                           TRIM(cdebug), ': units=', TRIM(cunits), TRIM(cdebug2)
!       CALL message('', message_text)
!       cf_desc    = t_cf_var( diagname, cunits, clongname, datatype_flt)
!       grib2_desc = grib2_var(diag_list%table, 255, 255, ibits, GRID_UNSTRUCTURED, GRID_CELL)
!       CALL add_var( diag_list%tstream, tolower(diagname), diag_list%diag(i)%fld3d,                        &
!                                 & GRID_UNSTRUCTURED_CELL, ZA_HYBRID, cf_desc, grib2_desc, ldims=cshape3d, &
!                                 & vert_interp=create_vert_interp_metadata(                                &
!                                 &             vert_intp_type=vintp_types("P","Z","I"),                    &
!                                 &             vert_intp_method=VINTP_METHOD_LIN,                          &
!                                 &             l_loglin=.FALSE.,                                           &
!                                 &             l_extrapol=.FALSE.), lopenacc=.TRUE.                        )
!       __acc_attach(diag_list%diag(i)%fld3d)

!       diag_list%diag(i)%ndims = 3
!     END IF

!   END SUBROUTINE new_diag_element

! !---------------------------------------------------------------------------------------------------

!   SUBROUTINE new_diag(diag_list, nval, valflag, valname, ikey_type,     &
!                       longname, units, lpost, lrestart, shape2d, shape3d)

!     TYPE(t_diag_list),          INTENT(inout)   :: diag_list
!     INTEGER,                    INTENT(in)      :: nval          ! number of diag elements to be created
!     LOGICAL,                    INTENT(in)      :: valflag(nval) ! decides whether to create diag element
!     CHARACTER(len=*),           INTENT(in)      :: valname(nval) ! name of the diagnostic type
!     INTEGER,                    INTENT(in)      :: ikey_type     ! key type (1 to 4)
!     CHARACTER(len=*), OPTIONAL, INTENT(in)      :: longname      ! descriptive name of diagnostics
!     CHARACTER(len=*), OPTIONAL, INTENT(in)      :: units         ! physical unit for output
!     LOGICAL,          OPTIONAL, INTENT(in)      :: lpost         ! add diagnostics to file output
!     LOGICAL,          OPTIONAL, INTENT(in)      :: lrestart      ! add diagnostics to restart file
!     INTEGER,          OPTIONAL, INTENT(in)      :: shape2d(2)    ! shape of 2-D output array (kproma, kblks)
!     INTEGER,          OPTIONAL, INTENT(in)      :: shape3d(3)    ! shape of 3-D output array (/kproma, klev, kblks/)
  
!     ! -- local variables
!     INTEGER               :: jt
!     LOGICAL               :: lok, lopost, lorestart
!     CHARACTER(len=64)     :: clongname, cunits
!     INTEGER               :: cshape2d(2), cshape3d(3)

!     ! 1) -- error checks
!     lok = .true. 
!     IF (nval <= 0) THEN
!       CALL message('new_diag', diag_list%diag_list_name//': No flag and name values provided!')
!       lok = .false.
!     END IF
!     IF (ikey_type < 1 .OR. ikey_type > 5) THEN
!       CALL message('new_diag', diag_list%diag_list_name//': Invalid value of key_type (1..5)!')
!       lok = .false.
!     END IF
!     !! Abort processing in case of errors
!     IF (.NOT. lok) RETURN

!     ! 2) -- handle optional arguments
!     IF (PRESENT(longname)) THEN
!       clongname = longname
!     ELSE
!       clongname = diag_list%longname
!     END IF
!     IF (PRESENT(units)) THEN
!       cunits = units
!     ELSE
!       cunits = diag_list%units
!     END IF
!     IF (PRESENT(lpost)) THEN
!       lopost = lpost
!     ELSE
!       lopost = diag_list%lpost
!     END IF
!     IF (PRESENT(lrestart)) THEN
!       lorestart = lrestart
!     ELSE
!       lorestart = diag_list%lrestart
!     END IF
!     cshape2D(:)=-999
!     cshape3D(:)=-999
!     IF ( PRESENT(shape2D) ) THEN 
!       cshape2D=shape2D
!     END IF
!     IF ( PRESENT(shape3D) ) THEN 
!       cshape3D=shape3D
!     END IF
!     ! 3) -- add diag elements for each valname that has valflag=.true.
!     DO jt = 1,nval
!       IF (valflag(jt)) CALL new_diag_element(diag_list,                                              &
!                                             TRIM(diag_list%diag_list_name)//'_'//TRIM(valname(jt)), &
!                                             ikey_type, jt,                                          &
!                                             longname=TRIM(clongname)//' of '//TRIM(valname(jt)),    & 
!                                             units=TRIM(cunits), lpost=lopost, lrestart=lorestart,   &
!                                             shape2D=cshape2D, shape3D=cshape3D )
!     END DO

!   END SUBROUTINE new_diag

! !---------------------------------------------------------------------------------------------------

!   FUNCTION get_diag_index(diag_list, ldebug, itrac, ivalue) RESULT (index)

!     USE mo_subm_trac,         ONLY: trlist
!     USE mo_subm_trac_para,    ONLY: AEROSOLMASS, AEROSOLNUMBER

!     INTEGER                         :: index
!     TYPE(t_diag_list), INTENT(in)   :: diag_list
!     LOGICAL,           INTENT(in)   :: ldebug       ! print debugging info
!     INTEGER,           INTENT(in)   :: itrac        ! tracer index in trlist
!     INTEGER, OPTIONAL, INTENT(in)   :: ivalue       ! key value to look for if key_type==BYUSER

!     ! -- local variables
!     INTEGER            :: i, iphase, ispec, iclass
!     LOGICAL            :: lfound

!     ! 1) -- collect information
!     index  = -1
!     lfound = .false.
!     iphase = trlist%ti(itrac)%nphase
!     ispec  = trlist%ti(itrac)%spid
!     iclass  = trlist%ti(itrac)%mode

!     IF (ldebug) THEN
!       WRITE(message_text,*) 'itrac, iphase, ispec, iclass = ',itrac, iphase, ispec, iclass
!       CALL message('get_diag_index', message_text)
!     END IF

!     ! 2) -- find matching diag element
!     DO i = 1, diag_list%ndiag
!       SELECT CASE(diag_list%diag(i)%key_type) 
!       CASE(1)       ! BYTRACER : index = tracer number
!         IF (diag_list%diag(i)%key == itrac) THEN
!           lfound = .true.
!           index = i
!           IF (ldebug) THEN
!             WRITE(message_text,*) 'itrac: lfound=.true. for diag = ',i,'. diag_list%diag(i)%key_type = ', &
!                                   diag_list%diag(i)%key_type
!             CALL message('', message_text)
!           END IF
!         END IF
!       CASE(2)       ! BYSPECIES : index = species number
!   !### ispec == 0 shouldn't occur...
!         IF (ispec > 0 .AND. diag_list%diag(i)%key == ispec) THEN
!           lfound = .true.
!           IF (ldebug) THEN
!             WRITE(message_text,*) 'ispec: lfound=.true. for diag = ',i,'. diag_list%diag(i)%key_type = ', &
!                                   diag_list%diag(i)%key_type
!             CALL message('', message_text)
!           END IF
!           index = i
!         END IF
!       CASE(3)       ! BYMODE : index = mode
!         IF (iclass > 0 .AND. diag_list%diag(i)%key == iclass .AND. iphase == AEROSOLMASS) THEN
!           lfound = .true.
!           index = i
!           IF (ldebug) THEN
!             WRITE(message_text,*) 'iclass: lfound=.true. for diag = ',i,'. diag_list%diag(i)%key_type = ', &
!                                   diag_list%diag(i)%key_type
!             CALL message('', message_text)
!           END IF
!         END IF
!       CASE(4)       ! BYNUMMODE : index = mode
!         IF (iclass > 0 .AND. diag_list%diag(i)%key == iclass .AND. iphase == AEROSOLNUMBER) THEN
!           lfound = .true.
!           index = i
!           IF (ldebug) THEN
!             WRITE(message_text,*) 'iclass (NUM): lfound=.true. for diag = ',i,'. diag_list%diag(i)%key_type = ', &
!                                   diag_list%diag(i)%key_type
!             CALL message('', message_text)
!           END IF
!         END IF
!       CASE(5)       ! BYUSER : index = something else, user-defined
!         IF (.NOT. PRESENT(ivalue)) CALL finish('get_diag_index', 'ivalue required but not given!')
!         IF (diag_list%diag(i)%key == ivalue) THEN
!           lfound = .true.
!           index = i
!         END IF
!       END SELECT
!       IF (lfound) EXIT          ! first occurence of diag takes precedence! (usually BYTRACER)
!     END DO

!   END FUNCTION get_diag_index

! !---------------------------------------------------------------------------------------------------

!   SUBROUTINE get_diag_pointer2d(diag_list, ptr2d, itrac, ivalue, ierr, ldebug)

!     TYPE(t_diag_list), INTENT(in)   :: diag_list
!     INTEGER,           INTENT(in)   :: itrac       ! tracer index in trlist
!     INTEGER, OPTIONAL, INTENT(in)   :: ivalue      ! key value to look for if key_type==BYUSER
!     INTEGER, OPTIONAL, INTENT(out)  :: ierr        ! error value (code will abort in case of error if not present)
!     LOGICAL, OPTIONAL, INTENT(in)   :: ldebug
!     REAL(dp), POINTER, INTENT(out)  :: ptr2d(:,:)

!     INTEGER, PARAMETER :: OK=0, LIST_EMPTY=1, NOT_FOUND=2
!     INTEGER            :: ind, myierr
!     LOGICAL            :: lodebug


!     NULLIFY (ptr2d)
!     myierr = OK

!     lodebug = .FALSE.
!     IF (PRESENT(ldebug)) THEN
!       lodebug = ldebug
!     END IF

!     ! 1) -- check if diag_list is empty
!     IF (diag_list%ndiag == 0) THEN
!       myierr = LIST_EMPTY
!     ELSE

!       ! 2) -- find diag index
!       IF (PRESENT(ivalue)) THEN
!         ind = get_diag_index(diag_list, lodebug, itrac, ivalue) 
!       ELSE
!         ind = get_diag_index(diag_list, lodebug, itrac) 
!       END IF

!       IF (ind <= 0) THEN
!         IF (PRESENT(ivalue)) THEN
!           WRITE (message_text, *) 'Cannot find index for diag '//TRIM(diag_list%diag_list_name), &
!                                   ': itrac, ivalue = ', itrac, ivalue
!         ELSE 
!           WRITE (message_text, *) 'Cannot find index for diag '//TRIM(diag_list%diag_list_name), &
!                                   ': itrac = ', itrac
!         END IF
!         myierr = NOT_FOUND
!         IF (.NOT. PRESENT(ierr)) CALL finish('get_diag_pointer', message_text)
!       END IF
!     END IF

!     IF (myierr == OK) ptr2d => diag_list%diag(ind)%fld2d
!     IF (PRESENT(ierr)) ierr = myierr

!   END SUBROUTINE get_diag_pointer2d   

! !---------------------------------------------------------------------------------------------------

!   SUBROUTINE get_diag_pointer3d(diag_list, ptr3d, itrac, ivalue, ierr, ldebug)
 
!     TYPE(t_diag_list), INTENT(in)   :: diag_list
!     INTEGER,           INTENT(in)   :: itrac       ! tracer index in trlist
!     INTEGER, OPTIONAL, INTENT(in)   :: ivalue      ! key value to look for if key_type==4
!     INTEGER, OPTIONAL, INTENT(out)  :: ierr        ! error value (code will abort in case of error if not present)
!     LOGICAL, OPTIONAL, INTENT(in)   :: ldebug
!     REAL(dp), POINTER, INTENT(out)  :: ptr3d(:,:,:)

!     INTEGER, PARAMETER :: OK=0, LIST_EMPTY=1, NOT_FOUND=2
!     INTEGER            :: ind, myierr
!     LOGICAL            :: lodebug


!     NULLIFY (ptr3d)
!     myierr = OK

!     lodebug = .FALSE.
!     IF (PRESENT(ldebug)) THEN
!       lodebug = ldebug
!     END IF

!     ! 1) -- check if diag_list is empty
!     IF (diag_list%ndiag == 0) THEN
!       myierr = LIST_EMPTY
!     ELSE

!       ! 2) -- find diag index
!       IF (PRESENT(ivalue)) THEN
!         ind = get_diag_index(diag_list, lodebug, itrac, ivalue)
!       ELSE
!         ind = get_diag_index(diag_list, lodebug, itrac)
!       END IF

!       IF (ind <= 0) THEN
!         IF (PRESENT(ivalue)) THEN
!           WRITE (message_text, *) 'Cannot find index for diag '//diag_list%diag_list_name, &
!                                   ': itrac, ivalue = ', itrac, ivalue
!         ELSE 
!           WRITE (message_text, *) 'Cannot find index for diag '//diag_list%diag_list_name, &
!                                   ': itrac = ', itrac
!         END IF
!         myierr = NOT_FOUND
!         IF (.NOT. PRESENT(ierr)) CALL finish('get_diag_pointer', message_text)
!       END IF
!     END IF

!     IF (myierr == OK) ptr3d => diag_list%diag(ind)%fld3d
!     IF (PRESENT(ierr)) ierr = myierr

!   END SUBROUTINE get_diag_pointer3d   

END MODULE mo_subm_diag_lite
