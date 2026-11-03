! 
! based on mo_submodel_tracer.f90
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

MODULE mo_subm_trac_lite

  USE mo_kind_lite,              ONLY: dp 
  USE mo_util_string_lite,       ONLY: separator      
  USE mo_exception_lite,         ONLY: finish, message, message_text 
  USE mo_subm_spec_lite,         ONLY: t_species
  USE mo_subm_trac_para_lite,    ONLY: nmaxtrac, ln, ll, lf,                   &  
                                  OK, NAME_USED, TABLE_FULL, OFF, ON,     &         
                                  GAS, AEROSOL, GAS_OR_AEROSOL,           &
                                  AEROSOLMASS, AEROSOLNUMBER, UNDEFINED

  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------
   
  PUBLIC :: ntrac         ! number of tracers (equal to trlist% ntrac)  
  PUBLIC :: trlist        ! tracer info list variable   
  
  PUBLIC :: nburden,      &
            diag_burden,  &
            d_burden          ! burden diagnostics 
                                             
  PUBLIC :: new_tracer,         & ! request tracer          
            validate_traclist,  & ! evaluate a list of tracer names for certain properties 
            init_tracer,        & 
            complete_tracer,    &
            print_tracer,       &
            new_diag_burden       ! get_diag_burden

  !--------------------
  ! module variables
  !--------------------
 
  INTEGER, SAVE :: ntrac = 0    ! number of tracers actually defined 
  INTEGER, SAVE :: nburden = 0  ! number of burden diagnostics currently defined

  !
  ! Individual settings for each tracer
  ! Default values are marked with * *
  ! 
  
  TYPE, EXTENDS(t_species) :: t_trinfo
 
    ! identification of transported quantity
    !
    CHARACTER(len=ln) :: basename   ! name (instead of xt..)
    CHARACTER(len=ln) :: subname    ! optional for 'colored' tracer
    CHARACTER(len=ln) :: fullname   ! name_subname 
    INTEGER           :: spid       ! species id (index in speclist) where physical/chemical 
                                    ! properties are defined  
    INTEGER           :: mode       ! aerosol mode or bin number  (default 0)  

    ! Requested resources ...
    ! 
    INTEGER           :: burdenid   ! index in burden diagnostics  
    INTEGER           :: ndrydep    ! dry deposition flag 
    INTEGER           :: nwetdep    ! wet deposition flag      
    INTEGER           :: nsedi      ! sedimentation flag       
    INTEGER           :: nemis      ! surface emission flag     
                                    ! emission flag:  *0*=no emissions, 
				                            !                  1=surface flux cond.,
                                    !                  2=tendency (2D emis.) [additive] 
    
    ! Flags used for postprocessing
    ! 
    INTEGER           :: code       ! tracer code,          (default 235...)
    INTEGER           :: table      ! tracer code table     (default 0) 
  END TYPE t_trinfo
   
  ! tracer list 
  TYPE t_trlist  
    INTEGER         :: ntrac          ! number of tracers specified 
    INTEGER         :: anywetdep      ! wet deposition requested for any tracer
    INTEGER         :: anydrydep      ! wet deposition requested for any tracer
    INTEGER         :: anysedi        ! sedimentation  requested for any tracer
    INTEGER         :: anysemis       ! surface emission flag for any tracer   
    TYPE (t_trinfo) :: ti  (nmaxtrac) ! Individual settings for each tracer
  END TYPE t_trlist
 
  TYPE(t_trlist), SAVE, TARGET :: trlist         

  ! type declaration
  TYPE diag_burden
    CHARACTER(len=ln)   :: name
    INTEGER             :: itype      ! type of burden calculation (see below)
                                      ! additive binary values 
    REAL(dp), POINTER   :: ptr1(:,:)  ! pointer for type= 1 : total column mass
    REAL(dp), POINTER   :: ptr2(:,:)  ! pointer for type= 2 : tropospheric column mass
    REAL(dp), POINTER   :: ptr4(:,:)  ! pointer for type= 4 : stratospheric column mass
    REAL(dp), POINTER   :: ptr8(:,:)  ! pointer for type= 8 : column density
    REAL(dp), POINTER   :: ptr16(:,:) ! pointer for type=16 : tropospheric column density
    REAL(dp), POINTER   :: ptr32(:,:) ! pointer for type=32 : stratospheric column density
  END TYPE diag_burden           
     
  TYPE (diag_burden) :: d_burden(nmaxtrac)
 
  CONTAINS
 
!---------------------------------------------------------------------------------------------------

  SUBROUTINE init_tracer
 
    INTEGER :: i
    
    trlist% ti(:)% code       = 0
    trlist% ti(:)% table      = 131 ! MPI-Meteorology, Hamburg 
    trlist% ti(:)% basename   = ''
    trlist% ti(:)% subname    = ''
    trlist% ti(:)% fullname   = '' 
    trlist% ti(:)% units      = ''
    trlist% ti(:)% longname   = ''
    trlist% ti(:)% moleweight = 0._dp ! molecular mass (copied from species upon initialisation)
       
    trlist% ti(:)% ndrydep    = 0
    trlist% ti(:)% nwetdep    = 0
    trlist% ti(:)% nsedi      = 0
    trlist% ti(:)% nemis      = 0 
     
    trlist% ti(:)% nphase     = 0    ! phase undefined
    trlist% ti(:)% mode       = 0    ! aerosol mode or bin (0 is ok for gas-phase tracers) 
     
    trlist% ti(:)% spid       = 0    ! no species index attached
    trlist% ti(:)% burdenid   = -1

  END SUBROUTINE init_tracer
 
!---------------------------------------------------------------------------------------------------
  
  SUBROUTINE new_tracer ( name, spid, subname, longname,      & ! names...
                          units, nphase, mode,                & 
                          code, table,                        & ! output
                          burdenid,                           & ! diagnostics 
                          ndrydep, nwetdep, nsedi, nemis,     & ! processes
                          moleweight,                         & ! species properties 
                          idx, ierr)                            ! return info

    CHARACTER(len=*) ,INTENT(in)            :: name      ! name of tracer 
    INTEGER          ,INTENT(in)  ,OPTIONAL :: spid      ! species index
    CHARACTER(len=*) ,INTENT(in)  ,OPTIONAL :: subname   ! optional for 'colored'
    CHARACTER(len=*) ,INTENT(in)  ,OPTIONAL :: longname  ! long name
    CHARACTER(len=*) ,INTENT(in)  ,OPTIONAL :: units     ! units 
    INTEGER          ,INTENT(in)  ,OPTIONAL :: nphase    ! phase indicator
    INTEGER          ,INTENT(in)  ,OPTIONAL :: mode      ! mode indicator or bin number 
    INTEGER          ,INTENT(in)  ,OPTIONAL :: code      ! grib code
    INTEGER          ,INTENT(in)  ,OPTIONAL :: table     ! grib table 
    INTEGER          ,INTENT(in)  ,OPTIONAL :: burdenid  ! burden diagnostics number 
    INTEGER          ,INTENT(in)  ,OPTIONAL :: ndrydep   ! dry deposition flag
    INTEGER          ,INTENT(in)  ,OPTIONAL :: nwetdep   ! wet deposition flag
    INTEGER          ,INTENT(in)  ,OPTIONAL :: nsedi     ! sedimentation flag
    INTEGER          ,INTENT(in)  ,OPTIONAL :: nemis     ! surface emission flag 
    
    !! species properties (to be removed in future versions)
    REAL(dp)         ,INTENT(in)  ,OPTIONAL :: moleweight! molecular weight  
    
    !! return values
    INTEGER          ,INTENT(out) ,OPTIONAL :: idx       ! position in tracerinfo
    INTEGER          ,INTENT(out) ,OPTIONAL :: ierr      ! error return value

    INTEGER                 :: i
    CHARACTER(len=ln)       :: fullname
    TYPE(t_trinfo) ,POINTER :: ti
    LOGICAL                 :: lspec     ! species id provided as input?  
    CHARACTER(len=ln)       :: cname     ! local copy
    INTEGER                 :: ispid     ! local copy of species index 

    !
    ! set default values for optional INTENT(out) parameters
    !
    IF (PRESENT(ierr))  ierr = UNDEFINED    ! fail safe
    IF (PRESENT(idx))   idx = 0

    ! test for presence of valid spid
    lspec = PRESENT(spid)
    ispid = 0
    IF (lspec) ispid = spid     

    ! make sure that nphase is defined if spid is given
    IF (lspec) THEN
      IF (.NOT. PRESENT(nphase))  &
        CALL message('new_tracer', 'Undefined phase for tracer ' // TRIM(name))
    END IF
    
    !
    ! derive full name
    !
    ! error if name is empty
    cname = name
    IF (LEN_TRIM(name) == 0) THEN
      WRITE(message_text,*) 'Empty species name, ntrac=', ntrac
      CALL message('new_tracer', message_text)
    END IF

    IF(PRESENT(subname)) THEN
      IF(TRIM(subname) /= '') THEN
        fullname = TRIM(cname)//'_'//subname
      ELSE
        fullname = cname
      END IF
    ELSE
      fullname = cname
    END IF
    !
    ! check for table full and name used errors
    !
    IF (ntrac >= nmaxtrac) THEN
      WRITE (message_text, *) 'Tracer table full. nmaxtrac =', nmaxtrac
      CALL finish('new_tracer', message_text)
      IF (PRESENT(ierr))  ierr = TABLE_FULL
      RETURN
    END IF
    IF (ANY(trlist% ti(1:ntrac)% fullname==fullname)) THEN
      CALL message('new_tracer', 'Tracer name already used: ' // TRIM(fullname))
      IF (PRESENT(ierr))  ierr = NAME_USED
      RETURN
    ENDIF
      
    !
    ! set tracer info for the new tracer
    !
    ntrac = ntrac + 1
    trlist% ntrac = ntrac
    ti => trlist% ti (ntrac)
    IF (PRESENT(idx))   idx  = ntrac
    IF (PRESENT(ierr))  ierr = OK     ! now we have a valid new entry even if it may contain rubbish
    !
    ! special handling for colored tracers:
    !   take over properties of previous tracer 
    !   use same attributes
    !
    IF (PRESENT(subname) .AND. ntrac > 1) THEN
      DO i=1,ntrac-1
        IF ( trlist% ti (i)% basename == name .AND.trlist% ti (i)% subname  == '' ) THEN
          ti = trlist% ti (i)
          IF (trlist% ti (i)% code > 0) ti% code = 0
        ENDIF
      END DO
    ENDIF
    !
    ! define tracer properties
    !
    ti% basename   = name
    ti% fullname   = fullname 
    ti% longname   = fullname
    ti% spid       = ispid
    
    IF (PRESENT(subname))    ti% subname    = subname
    IF (PRESENT(longname))   ti% longname   = longname
    IF (PRESENT(units))      ti% units      = units 
    IF (PRESENT(nphase))     ti% nphase     = nphase
    IF (PRESENT(mode))       ti% mode       = mode 
    IF (PRESENT(code))       ti% code       = code
    IF (PRESENT(table))      ti% table      = table 
    IF (PRESENT(burdenid))   ti% burdenid   = burdenid  
    IF (PRESENT(ndrydep))    ti% ndrydep    = ndrydep
    IF (PRESENT(nwetdep))    ti% nwetdep    = nwetdep
    IF (PRESENT(nsedi))      ti% nsedi      = nsedi
    IF (PRESENT(nemis))      ti% nemis      = nemis 
    IF (PRESENT(moleweight)) ti% moleweight = moleweight  

  END SUBROUTINE new_tracer

!---------------------------------------------------------------------------------------------------

  SUBROUTINE validate_traclist(tracnam, defaultnam, nphase, ldrydep, lwetdep, lsedi, lemis)
    
    USE mo_util_str12_lite,          ONLY: st1_in_st2_idx
    USE mo_util_string_lite,         ONLY: tolower
!    USE mo_mpi_lite,                 ONLY: my_process_is_mpi_workroot

    CHARACTER(len=*),           INTENT(inout) :: tracnam(:)       ! tracer names to be tested
    CHARACTER(len=*), OPTIONAL, INTENT(in)    :: defaultnam(:)    ! default names in list
                                                                  ! expanded when first entry is 'default'
    INTEGER,          OPTIONAL, INTENT(in)    :: nphase           ! phase value to be tested 
    LOGICAL,          OPTIONAL, INTENT(in)    :: ldrydep          ! test for ndrydep>0
    LOGICAL,          OPTIONAL, INTENT(in)    :: lwetdep          ! test for nwetdep>0
    LOGICAL,          OPTIONAL, INTENT(in)    :: lsedi            ! test for nsedi>0
    LOGICAL,          OPTIONAL, INTENT(in)    :: lemis            ! test for nemis>0

    CHARACTER(len=ln)      :: allnam(nmaxtrac)    ! complete array of tracer names defined
    INTEGER                :: tridx(nmaxtrac)     ! map tracnam to trlist indices
    INTEGER                :: listsize, ndef, jt, ind
    LOGICAL                :: ltest(nmaxtrac)     ! result of tracer property test
    LOGICAL                :: lspecial            ! default or all option was used

    CHARACTER(LEN=20)       :: suff

    !-- initialize
    listsize = SIZE(tracnam)
!    IF (my_process_is_mpi_workroot()) write(0,*) 'tracnam list size = ',listsize, ' string length = ',LEN(tracnam(1))
    ltest(:) = .true.             ! default: accept all tracers as valid
    lspecial = .false.
    ndef = 0                      ! number of tracers copied from default list
    DO jt = 1,ntrac 
      allnam(jt) = trlist%ti(jt)%fullname
    END DO
    tridx(:) = 0
    suff=''

    !-- establish list of valid tracer names
    !-- handle special keyword "all"
    IF (tolower(tracnam(1)) == 'all') THEN
      DO jt = 1,ntrac
        tracnam(jt) = TRIM(allnam(jt))
      END DO
      lspecial = .true.
    END IF

    !-- handle special keyword "default"
    IF (tolower(tracnam(1)) == 'default') THEN
      IF (PRESENT(defaultnam)) THEN
        ndef = SIZE(defaultnam)
        !-- copy extra entries after "default" to make room for default tracers
        ! (in extreme cases some tracers could get lost, but this would mean a bizarre tracnam list)
        DO jt = listsize-ndef,2,-1
          tracnam(jt+ndef) = tracnam(jt)
        END DO
        !-- fill in the default tracer names
        DO jt=1,ndef
          tracnam(jt) = TRIM(defaultnam(jt))
        END DO
        lspecial = .true.
      ELSE
        CALL message('validate_traclist', 'Found string "default" in tracnam, '//    &
                     'but there is no defaultnam argument!')
        tracnam(1) = ''
      END IF
    END IF

    !-- validity check: make sure that all tracers in tracnam are valid tracer names
    DO jt = 1, listsize 
      ind = st1_in_st2_idx(tracnam(jt), allnam)
      IF (ind <= 0) THEN 
        tracnam(jt) = ''
      ELSE
        tridx(jt) = ind
      END IF
    END DO

    !-- tracer property tests
    IF (PRESENT(nphase)) THEN
      DO jt = 1,listsize
        IF (tridx(jt) > 0) THEN
          ! test if tracer nphase is not equal to argument. If so, set ltest(jt) to false
          IF (trlist%ti(tridx(jt))%nphase /= nphase) ltest(jt) = .false. 
        END IF
      END DO
    END IF
    IF (PRESENT(ldrydep)) THEN
    suff='drydep'  
      DO jt = 1,listsize
        IF (tridx(jt) > 0) THEN
          ! test if tracer is attached to dry deposition scheme
          IF (trlist%ti(tridx(jt))%ndrydep == 0) ltest(jt) = .false. 
        END IF
      END DO
    END IF
    IF (PRESENT(lwetdep)) THEN
      suff='wetdep'  
      DO jt = 1,listsize
        IF (tridx(jt) > 0) THEN
          ! test if tracer is attached to wet deposition scheme
          IF (trlist%ti(tridx(jt))%nwetdep == 0) ltest(jt) = .false. 
        END IF
      END DO
    END IF
    IF (PRESENT(lsedi)) THEN
      suff='sedi'  
      DO jt = 1,listsize
        IF (tridx(jt) > 0) THEN
          ! test if tracer is attached to sedimentation scheme
          IF (trlist%ti(tridx(jt))%nsedi == 0) ltest(jt) = .false. 
        END IF
      END DO
    END IF
    IF (PRESENT(lemis)) THEN
      suff='emis'  
      DO jt = 1,listsize
        IF (tridx(jt) > 0) THEN
          ! test if tracer is attached to emissions scheme
          IF (trlist%ti(tridx(jt))%nemis == 0) ltest(jt) = .false. 
        END IF
      END DO
    END IF
      
    !-- remove "false" tracers from tracnam list
    DO jt = 1,listsize 
      IF (.NOT. ltest(jt)) tracnam(jt) = ''
    END DO
  
!    IF ( my_process_is_mpi_workroot()  ) THEN
    write(0,*) '---------------------------------------------------------'
    write(0,*) 'validate_traclist: final tracnam list ', TRIM(suff)
    DO jt=1,listsize
    IF (TRIM(tracnam(jt)) /= '') write(0,*) jt,': ',tracnam(jt)
    END DO
    write(0,*) '---------------------------------------------------------'
!    END IF

  END SUBROUTINE validate_traclist

!---------------------------------------------------------------------------------------------------

  SUBROUTINE complete_tracer

    INTEGER :: jt
  
    trlist% anydrydep    = 0
    trlist% anywetdep    = 0
    trlist% anysedi      = 0
    trlist% anysemis     = 0   

    DO jt = 1,ntrac 
      trlist% anydrydep = IOR (trlist% anydrydep, trlist% ti(jt)% ndrydep)
      trlist% anywetdep = IOR (trlist% anywetdep, trlist% ti(jt)% nwetdep)
      trlist% anysedi   = IOR (trlist% anysedi,   trlist% ti(jt)% nsedi)
      trlist% anysemis  = IOR (trlist% anysemis,  trlist% ti(jt)% nemis)  
    END DO
 
  END SUBROUTINE complete_tracer 

!---------------------------------------------------------------------------------------------------

  SUBROUTINE print_tracer 
 
    INTEGER :: jt

    CALL message('','')
    CALL message('',separator)
 
    CALL message('', 'Tracer information')
    CALL message('', '')
    WRITE(message_text,'(a)') 'index, fullname' 
    CALL message('', message_text)

    DO jt = 1,ntrac

      WRITE(message_text, '(i0,5x,a20)') jt, TRIM(trlist%ti(jt)%fullname) 
      CALL message('', message_text)

    END DO
 
  END SUBROUTINE print_tracer 

!---------------------------------------------------------------------------------------------------

  INTEGER FUNCTION new_diag_burden (name, itype, lclobber)
    
    CHARACTER(len=*), INTENT(in)        :: name
    INTEGER, INTENT(in), OPTIONAL       :: itype
    LOGICAL, INTENT(in), OPTIONAL       :: lclobber   ! return existing index if true, error if false

    INTEGER         :: jtype, jn
    LOGICAL         :: loclob

    ! Set defaults and check arguments
    new_diag_burden = -1
    jtype = 1       ! total column mass
    IF (PRESENT(itype)) jtype = itype
    IF (jtype > 7) CALL message('new_diag_burden', 'ITYPE > 7 presently not implemented!')
    loclob = .false.    ! return error if burden name already defined
    IF (PRESENT(lclobber)) loclob = lclobber
    
    ! look for existing burden diag of the same name
    DO jn = 1,nburden
      IF (TRIM(d_burden(jn)%name) == TRIM(name)) THEN
        IF (loclob) THEN
          new_diag_burden = jn
        ELSE
          CALL message('new_diag_burden', 'Burden diagnostics with name '//name//' already defined!')
        END IF
        RETURN     ! nothing else to be done
      END IF
    END DO

    ! Add new entry to the list
    nburden = nburden + 1
    IF (nburden > nmaxtrac) THEN
      CALL message('new_diag_burden', 'Maximum number of burden diagnostics exceeded!')
      CALL message('new_diag_burden', 'Error occured while trying to add burden diag for '//name)
      RETURN
    END IF
    d_burden(nburden)%name = TRIM(name)
    d_burden(nburden)%itype = jtype
    new_diag_burden = nburden
    CALL message('new_diag_burden', 'Defined burden diag for '//name)

  END FUNCTION new_diag_burden
  
END MODULE mo_subm_trac_lite
