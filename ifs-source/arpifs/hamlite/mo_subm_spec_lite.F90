! 
! based on mo_species.f90
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

MODULE mo_subm_spec_lite
 
  USE mo_kind_lite,              ONLY: dp
  USE mo_subm_trac_para_lite,    ONLY: ln, ll, ls, &
                                  GAS, AEROSOL, GAS_OR_AEROSOL, nmaxtrac
  USE mo_exception_lite,         ONLY: message, message_text, finish
  
  IMPLICIT NONE
  PRIVATE
 
  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: init_splist
  PUBLIC :: new_species
  PUBLIC :: query_species 
  PUBLIC :: print_species 
 
  PUBLIC :: nmaxspec 
  PUBLIC :: nmaxtrspec 
        
  PUBLIC :: nspec
  PUBLIC :: t_species
  PUBLIC :: speclist 

  !--------------------
  ! module parameters
  !--------------------
   
  INTEGER, PARAMETER :: nmaxspec   = nmaxtrac ! maximum number of species in the model
  INTEGER, PARAMETER :: nmaxtrspec = 10       ! maximum number of species per tracer
 
  !--------------------
  ! module variables
  !--------------------

  ! number of species 
  INTEGER :: nspec             

  ! derived variables 
  !   basic 'species' type for gas and aerosol 
  TYPE :: t_species
   
     ! --- basic information ---  
     CHARACTER(LEN=ll) :: longname            ! Name of the species
     CHARACTER(LEN=ls) :: shortname           ! Shorthand name   
     CHARACTER(LEN=ln) :: units               ! Units for 'outside world', usually kg kg-1 
     REAL(dp)          :: moleweight          ! Molecular weight [g mol-1]  
     ! --- physical and chemical parameters ---  
     INTEGER           :: nphase              ! phase of species   
     REAL(dp)          :: density             ! Density of solid or liquid phase [g cm-3]    
     REAL(dp)          :: kappa               ! Kappa-Koehler coefficient for the species.  
                                              ! See: Petters and Kreidenweis
                                              ! ACP 7, 1961-1971, 2007  
     ! --- process instructions ---  
     LOGICAL           :: lburden             ! Calculate column burden for this species     
     LOGICAL           :: lrad                ! Corresponding tracer should interact with radiation
     LOGICAL           :: lemis               ! Corresponding tracer should be emitted
     LOGICAL           :: ldrydep             ! Corresponding tracer should be dry deposited
     LOGICAL           :: lwetdep             ! Corresponding tracer should be scavenged   
     INTEGER           :: iaerorad            ! Index to aerosol radiation table
     ! --- accounting ---   
     INTEGER           :: idt                 ! Tracer index for gas-phase tracer   
  END TYPE t_species 
  
  TYPE(t_species), TARGET :: speclist(nmaxspec) 

  CONTAINS
   
!---------------------------------------------------------------------------------------------------

  SUBROUTINE init_splist 

    USE mo_kind_lite,                  ONLY: dp
    USE mo_physical_constants_lite,    ONLY: amd   ! dry air mass

    nspec     = 0 
    
    speclist(:)% longname     = ''
    speclist(:)% shortname    = ''
    speclist(:)% units        = ''
    speclist(:)% moleweight   = amd
    speclist(:)% idt          = -1  
    speclist(:)% lburden      = .TRUE. 
    speclist(:)% density      = 0._dp    
    speclist(:)% kappa        = 0._dp 
    speclist(:)% lrad         = .FALSE.
    speclist(:)% lemis        = .FALSE.
    speclist(:)% ldrydep      = .FALSE.
    speclist(:)% lwetdep      = .FALSE. 
    speclist(:)% iaerorad     = 0   
 
  END SUBROUTINE init_splist 

!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE new_species(nphase,                                  &
                         longname,                                &
                         shortname,                               &
                         units,                                   &
                         mw,                                      &  
                         density,                                 &!aerosol     
                         kappa,                                   & 
                         lemis,                                   &
                         ldrydep,                                 &
                         lwetdep,                                 &   
                         lrad,                                    &
                         iaerorad,                                & 
                         lburden,                                 &   
                         idx)
 
    INTEGER,          INTENT(IN)   :: nphase               ! GAS or AEROSOL
    CHARACTER(LEN=*), INTENT(IN)   :: longname             ! full name (e.g. for diagnostics)
    CHARACTER(LEN=*), INTENT(IN)   :: shortname            ! short name (e.g. variable name in output files) 
    CHARACTER(LEN=*), INTENT(IN)   :: units                ! concentration units, eg 'kg kg-1', 'VMR'
    REAL(dp),         INTENT(IN)   :: mw                   ! Molecular weight (required for conversions) 
    REAL(dp), INTENT(IN), OPTIONAL :: density              ! (aerosol) density    
    REAL(dp), INTENT(IN), OPTIONAL :: kappa                ! Kappa-Koehler coefficient 
    LOGICAL,  INTENT(IN), OPTIONAL :: lemis                ! flag for emission
    LOGICAL,  INTENT(IN), OPTIONAL :: ldrydep              ! flag for dry deposition
    LOGICAL,  INTENT(IN), OPTIONAL :: lwetdep              ! flag for wet deposition
    LOGICAL,  INTENT(IN), OPTIONAL :: lrad                 ! flag for radiation interaction
    INTEGER,  INTENT(IN), OPTIONAL :: iaerorad             ! index to aerosol radiation table
    LOGICAL,  INTENT(IN), OPTIONAL :: lburden              ! Calculate column burden 
    INTEGER,  INTENT(OUT),OPTIONAL :: idx                  ! index to species list 

    !--- Local data
    INTEGER              :: i
    CHARACTER(len=16)    :: cphasenam

    !--- executable procedure
    ! Default return value
    IF (PRESENT(idx))   idx = 0

    ! Check validity of nphase
    SELECT CASE(nphase)
    CASE(GAS)
      cphasenam = 'Gas-phase'
    CASE(AEROSOL)
      cphasenam = 'Aerosol'
    CASE(GAS_OR_AEROSOL)
      cphasenam = 'Gas or Aerosol'
    CASE DEFAULT
      CALL message('new_species', 'Invalid value for nphase. Only GAS or AEROSOL allowed.')
      RETURN
    END SELECT
    
    ! Check if a species of this name has already been defined
    CALL query_species(longname=longname, nphase=nphase, index=i) 
    
    IF (i > 0) THEN
      CALL message('new_species', TRIM(cphasenam)//' species '//TRIM(longname)//' already defined.')
      CALL message('', 'Will use existing properties.')
      IF (PRESENT(idx)) idx = i    ! return index
      RETURN
    END IF
   
    CALL query_species(shortname=shortname, nphase=nphase, index=i) 
    
    IF (i > 0) THEN
      CALL message('new_species', TRIM(cphasenam)//' species '//TRIM(shortname)//' already defined.')
      CALL message('', 'Will use existing properties.')
      IF (PRESENT(idx)) idx = i    ! return index
      RETURN
    END IF
  
    ! Check if table is full
    IF (nspec >= nmaxspec) THEN
      WRITE (message_text,*) 'Species list full. nmaxspec = ',nmaxspec
      CALL finish('new_species', message_text)
    END IF
      
    ! Increment number of species instances and store data
    nspec    = nspec + 1
    i        = nspec
      
    ! set species properties 
    speclist(i)% nphase       = nphase
    speclist(i)% longname     = longname
    speclist(i)% shortname    = shortname
    speclist(i)% units        = units
    speclist(i)% moleweight   = mw 
    IF (PRESENT(lburden))   speclist(i)% lburden      = lburden
    IF (PRESENT(lrad))      speclist(i)% lrad         = lrad
    IF (PRESENT(lemis))     speclist(i)% lemis        = lemis
    IF (PRESENT(ldrydep))   speclist(i)% ldrydep      = ldrydep
    IF (PRESENT(lwetdep))   speclist(i)% lwetdep      = lwetdep 
    
    ! for aerosol only 
    IF (PRESENT(density))      speclist(i)% density      = density    
    IF (PRESENT(kappa))        speclist(i)% kappa        = kappa 
    IF (PRESENT(iaerorad))     speclist(i)% iaerorad     = iaerorad
      
    ! capture species definition errors
    IF (IAND(nphase, AEROSOL) == 0) THEN

      ! capture errors 
      IF (PRESENT(density))      CALL message('new_species',  &
                                    'Property "density" undefined for gas-phase species '//TRIM(shortname))    
      IF (PRESENT(kappa))        CALL message('new_species',  &
                                    'Property "kappa" undefined for gas-phase species '//TRIM(shortname)) 
      IF (PRESENT(iaerorad))     CALL message('new_species',  &
                                    'Property "iaerorad" undefined for gas-phase species '//TRIM(shortname))

    END IF
  
    IF (PRESENT(idx)) idx = i ! return index

  END SUBROUTINE new_species 
  
!---------------------------------------------------------------------------------------------------

  SUBROUTINE query_species (longname, shortname,            &      ! search criteria
                           index,                           &      ! query results ...
                           nphase, units,                   &
                           mw,                              &  
                           density,                         & 
                           kappa,                           &
                           lrad,                            &
                           lemis, ldrydep, lwetdep,         &
                           iaerorad,                        & 
                           idt,                             &
                           ierr)

  
  CHARACTER(len=*) ,INTENT(in)  ,OPTIONAL :: longname     ! name of species
  CHARACTER(len=*) ,INTENT(in)  ,OPTIONAL :: shortname    ! subname of species
  INTEGER          ,INTENT(in)  ,OPTIONAL :: nphase       ! GAS or AEROSOL

  INTEGER          ,INTENT(out) ,OPTIONAL :: index        ! index of species as query result
  CHARACTER(len=*) ,INTENT(out) ,OPTIONAL :: units        ! physical units of tracer
  REAL(dp)         ,INTENT(out) ,OPTIONAL :: mw           ! molecular weight 
  REAL(dp)         ,INTENT(out) ,OPTIONAL :: density      ! (aerosol) density    
  REAL(dp)         ,INTENT(out) ,OPTIONAL :: kappa        ! Kappa-Koehler coefficient 
  LOGICAL          ,INTENT(out) ,OPTIONAL :: lrad         ! flag for radiation interaction
  LOGICAL          ,INTENT(out) ,OPTIONAL :: lemis        ! flag for emissions
  LOGICAL          ,INTENT(out) ,OPTIONAL :: ldrydep      ! flag for dry deposition
  LOGICAL          ,INTENT(out) ,OPTIONAL :: lwetdep      ! flag for wet deposition
  INTEGER          ,INTENT(out) ,OPTIONAL :: iaerorad     ! index to aerosol radiation table 
  INTEGER          ,INTENT(out) ,OPTIONAL :: idt          ! associated tracer index
  INTEGER          ,INTENT(out) ,OPTIONAL :: ierr         ! error return value

  ! --- local variables
  INTEGER             :: i
  INTEGER             :: ispec   ! local species id 
  INTEGER             :: iphase
  INTEGER, PARAMETER  :: OK = 0
  INTEGER, PARAMETER  :: UNDEF = 1

  ! -- set defaults
  ispec = 0
  IF (PRESENT(ierr))         ierr         = UNDEF       ! default error: species not found
  IF (PRESENT(index))        index        = 0
  IF (PRESENT(units))        units        = ''
  IF (PRESENT(mw))           mw           = 0._dp 
  IF (PRESENT(density))      density      = 0._dp    
  IF (PRESENT(kappa))        kappa        = 0._dp 
  IF (PRESENT(lrad))         lrad         = .FALSE.
  IF (PRESENT(lemis))        lemis        = .FALSE.
  IF (PRESENT(ldrydep))      ldrydep      = .FALSE.
  IF (PRESENT(lwetdep))      lwetdep      = .FALSE. 
  IF (PRESENT(idt))          idt          = 0
  IF (PRESENT(iaerorad))     iaerorad     = 0

  ! -- test if phase if given
  iphase = -1    ! undefined
  IF (PRESENT(nphase)) THEN
    iphase = nphase
    IF (IAND(iphase, GAS+AEROSOL) == 0) CALL message('query_species', 'Invalid value for nphase!')
  END IF

  ! -- search for species: always search for longname first
  IF (PRESENT(longname)) THEN
      DO i=1, nspec
        IF (TRIM(speclist(i)%longname) == TRIM(longname)) THEN 
          ispec = i
          EXIT
        END IF
      END DO 
  END IF

  ! -- if no species found or longname not given look for shortname
  IF (ispec == 0 .AND. PRESENT(shortname)) THEN
      DO i=1, nspec
        IF (TRIM(speclist(i)%shortname) == TRIM(shortname)) THEN 
          ispec = i
          EXIT 
        END IF
      END DO 
  END IF

  ! --- Collect species properties
  IF (ispec > 0) THEN
    IF (PRESENT(ierr))         ierr         = OK  ! error status: OK
    IF (PRESENT(index))        index        = ispec
    IF (PRESENT(units))        units        = speclist(ispec)% units
    IF (PRESENT(mw))           mw           = speclist(ispec)% moleweight    
    IF (PRESENT(density))      density      = speclist(ispec)% density   
    IF (PRESENT(kappa))        kappa        = speclist(ispec)% kappa 
    IF (PRESENT(lrad))         lrad         = speclist(ispec)% lrad
    IF (PRESENT(lemis))        lemis        = speclist(ispec)% lemis   
    IF (PRESENT(ldrydep))      ldrydep      = speclist(ispec)% ldrydep
    IF (PRESENT(lwetdep))      lwetdep      = speclist(ispec)% lwetdep 
    IF (PRESENT(idt))          idt          = speclist(ispec)% idt
    IF (PRESENT(iaerorad))     iaerorad     = speclist(ispec)% iaerorad
  END IF

  END SUBROUTINE query_species

!---------------------------------------------------------------------------------------------------

  SUBROUTINE print_species
  
    USE mo_util_string_lite,   ONLY: separator

    IMPLICIT NONE

    INTEGER           :: js
    CHARACTER(len=11) :: cstring

    CALL message('','')
    CALL message('',separator)

    CALL message('', 'Species information') 
    CALL message('', '')
    WRITE(message_text,'(a)') 'index, shortname, longname, phase, molecular weight [g/mol]'
    CALL message('', message_text)
    
    DO js = 1,nspec

      SELECT CASE(speclist(js)%nphase)
      CASE (GAS)
        cstring = 'gas        '
      CASE (AEROSOL)
        cstring = 'aerosol    '
      CASE (GAS_OR_AEROSOL)
        cstring = 'gas|aerosol'
      END SELECT
      WRITE(message_text, '(i0,5x,a10,5x,a20,5x,a10,5x,e10.3)') js,  &
        TRIM(speclist(js)%shortname), TRIM(speclist(js)%longname), cstring, speclist(js)%moleweight     
      CALL message('', message_text)

    END DO
 
  END SUBROUTINE print_species

END MODULE mo_subm_spec_lite

