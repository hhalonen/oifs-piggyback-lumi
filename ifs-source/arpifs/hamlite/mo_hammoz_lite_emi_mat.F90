! 
! based on mo_emi_matrix.f90
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

MODULE mo_hammoz_lite_emi_mat

  USE mo_kind_lite,              ONLY: dp
  USE mo_util_extfield_lite,     ONLY: EF_INACTIVE, EF_VALUE, EF_FILE, EF_MODULE,    &
                                  EF_3D, EF_LONLAT, EF_LEV, EF_SINGLE,          &
                                  EF_TIMERESOLVED, EF_IGNOREYEAR, EF_CONSTANT,  & 
                                  EF_NOINTER, EF_LINEAR, EF_CUBIC
  USE mo_subm_bcs_lite,          ONLY: BC_EVERYWHERE, BC_BOTTOM, BC_LEVEL

  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: em_read
  PUBLIC :: em_get_sector_info
  PUBLIC :: em_get_bc_from_matrix 
  PUBLIC :: maxsectors
  PUBLIC :: maxvars 

  PUBLIC :: emi_var
  PUBLIC :: emi_matrix
  PUBLIC :: emi_sector

  PUBLIC :: EM_NONE, EM_SURFACE, EM_VOLUME, EM_LEVEL50M, EM_FIRE

  !--------------------
  ! module parameters
  !--------------------

  INTEGER, PARAMETER :: maxsectors   = 30    ! max number of sectors
  INTEGER, PARAMETER :: maxvars      = 50    ! max number of variables in one sector

  INTEGER, PARAMETER :: EM_NONE     = 0
  INTEGER, PARAMETER :: EM_SURFACE  = 1
  INTEGER, PARAMETER :: EM_VOLUME   = 2
  INTEGER, PARAMETER :: EM_LEVEL50M = 3
  INTEGER, PARAMETER :: EM_FIRE     = 4

  !--------------------
  ! module variables
  !--------------------

  TYPE :: emi_var 
    CHARACTER(LEN=512) :: ev_varname
    CHARACTER(LEN=512) :: ev_translation
    REAL(KIND=dp)      :: ev_factor
  END TYPE emi_var

  TYPE :: emi_sector 
    CHARACTER(LEN=64)  :: es_sectorname
    CHARACTER(LEN=512) :: es_file
    CHARACTER(LEN=512) :: es_varname
    INTEGER            :: es_nvars
    TYPE(emi_var)      :: es_variables(maxvars)
    INTEGER            :: es_eftype         ! external field type
    INTEGER            :: es_efgeometry     ! external field geometry
    INTEGER            :: es_eftimedef      ! external field time axis
    INTEGER            :: es_efinterpolate  ! external field time interpolation
    REAL(dp)           :: es_eftimeoffset   ! external field time offset
    REAL(dp)           :: es_efvalue  
    INTEGER            :: es_emtype         ! emission (application) type
    INTEGER            :: es_nlev           ! number of vertical levels in input file
  END TYPE emi_sector

  TYPE :: emi_matrix 
    INTEGER            :: em_nsectors
    TYPE(emi_sector)   :: em_sectors(maxsectors)
  END TYPE emi_matrix

  TYPE(emi_matrix), SAVE :: ematrix

  CONTAINS

!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE get_next_dataline (iunit, inline, eof)

    IMPLICIT NONE

    INTEGER, INTENT(IN)             :: iunit
    CHARACTER(LEN=512), INTENT(OUT) :: inline
    LOGICAL, INTENT(OUT)            :: eof

    INTEGER                         :: status
  
    status = 0
    inline ='#'

    DO WHILE ((inline(1:1) == '#') .AND. (status .GE. 0))
      read(iunit,'(a)',iostat=status) inline
      inline = ADJUSTL(inline)
      IF (LEN_TRIM(inline) == 0) inline(1:1) = '#' ! skip empty lines
    ENDDO

    eof = (status < 0)

  END SUBROUTINE get_next_dataline

! !---------------------------------------------------------------------------------------------------

  SUBROUTINE em_read (nsectors)

    USE mo_io_units_lite,      ONLY: find_next_free_unit
    USE mo_util_string_lite,   ONLY: toupper
    USE mo_exception_lite,     ONLY: finish, message, message_text
    !USE mo_mpi_lite,           ONLY: p_pe, p_io, p_bcast,  p_comm_work
    USE mo_subm_main_lite,     ONLY: emi_basepath
    USE mo_util_string_lite,   ONLY: separator

    IMPLICIT NONE

    INTEGER, INTENT(OUT)  :: nsectors

    INTEGER               :: iunit, start_ndx, tokenlen, stop_ndx, i, j, ierr
    INTEGER               :: eftype, efgeometry, eftimedef, efnlev
    INTEGER               :: emtype, nvars, matvec(maxsectors), nheadersec, efinterpolate
    REAL(KIND=dp)         :: factor, efvalue, eftimeoffset
    CHARACTER(LEN=512)    :: inline, pathname, filename, cfile, ctoken
    CHARACTER(LEN=256)    :: eftypestring, domainstring, errmsg
    CHARACTER(LEN=64)     :: secname, varname, help(maxsectors+1), translation
    LOGICAL               :: eof, lfound, llast
    LOGICAL               :: lcorrect  ! try to avoid calling finish only for p_parallel_io
                                       ! (because of bunch of error messages from all other processors when being killed)

    lcorrect = .TRUE.
    CALL message('', separator)
    CALL message('', '')

    IF (LEN(TRIM(emi_basepath)) /= 0 ) THEN
      CALL message('em_read', 'Basepath for emission data files: '//TRIM(emi_basepath))
    ELSE
      WRITE(message_text,'(a)') &
            'Basepath for emission data files is empty! Please set it in your subm_main_list namelist!'
      CALL message('em_read', message_text)
      lcorrect = .FALSE.
    ENDIF

    CALL message('', '')

    ! Initialisation
    nsectors = 0
    ematrix%em_nsectors = 0
    DO i=1, maxsectors
      ematrix%em_sectors(i)%es_nvars  = 0
      ematrix%em_sectors(i)%es_emtype = -1
      ematrix%em_sectors(i)%es_eftype = -1
      ematrix%em_sectors(i)%es_efgeometry = -1
      ematrix%em_sectors(i)%es_eftimedef = -1
      ematrix%em_sectors(i)%es_eftimeoffset = 0.0_dp
      ematrix%em_sectors(i)%es_efinterpolate= -1
      ematrix%em_sectors(i)%es_efvalue = 0._dp
      ematrix%em_sectors(i)%es_nlev = -1
    END DO
    matvec(:) = -1
    eftimeoffset = 0.0_dp
    efinterpolate= EF_NOINTER

    ! read the emissions matrix on the I/O processor
    !IF ( p_pe .eq. p_io  ) THEN
      iunit = find_next_free_unit (30, 100)
      OPEN(iunit,file="emi_spec.txt",status='OLD',iostat=ierr)
      IF (ierr /= 0) THEN
        CALL message('em_read', 'emi_spec not found!')
        lcorrect = .FALSE.
      ELSE
      ! first section: SECTORS
        CALL get_next_dataline (iunit, inline, eof)

        IF (eof) THEN
          CALL message('em_read', 'no information found in emi_spec!')
          lcorrect = .FALSE.
        ELSE
          DO WHILE ((toupper(inline(1:6)) /= 'MATRIX') .AND. (.NOT. eof))
            nsectors = nsectors + 1

            ! read emission sector (short) name
            start_ndx = 1
            CALL extract_token(inline, '=', 64, start_ndx, tokenlen, ctoken)
            secname = ctoken(1:tokenlen)

            ! read boundary condition type
            CALL extract_token(inline, ',', 64, start_ndx, tokenlen, ctoken)
            eftypestring = toupper(ctoken(1:tokenlen))

            !! test validity of eftypestring
            eftype = -1
            IF (TRIM(eftypestring) == 'EF_INACTIVE') THEN
              eftype = EF_INACTIVE
              CALL message('em_read', 'Detected inactive boundary condition for sector '  &
                          //TRIM(secname)//' in emi_spec.txt!')
            END IF
            IF (TRIM(eftypestring) == 'EF_VALUE' ) eftype = EF_VALUE
            IF (TRIM(eftypestring) == 'EF_MODULE') eftype = EF_MODULE
            IF (TRIM(eftypestring) == 'EF_FILE'  ) eftype = EF_FILE
            IF (TRIM(eftypestring) == '') THEN
              CALL message('em_read', 'No processing information for sector '  &
                                  //TRIM(secname)//' in emi_spec.txt! Will use EF_INACTIVE.')
              eftype = EF_INACTIVE
            END IF
            IF (eftype < 0) THEN
              CALL message ('em_read', 'Invalid EF type for sector '//TRIM(secname)   &
                            //' in emi_spec.txt: "'//TRIM(eftypestring)//'"!')
              lcorrect = .FALSE.
              CALL get_next_dataline (iunit, inline,eof)
              CYCLE     ! ignore rest of input. Program will abort anyhow
            END IF

            !! for eftype == EF_FILE must read filename, varname and domainstring
            !! all others except EF_INACTIVE need only domainstring 
            !! The domainstring contains information related to the definition of a 
            !! boundary condition plus a keyword to identify the way how emissions
            !! from this sector shall be applied (surface, volume, level50m, etc.)
            filename=''
            varname=''
            efgeometry = -1
            eftimedef = -1
            efvalue = 0._dp
            emtype = -1
            efnlev=-1

            IF (eftype == EF_FILE) THEN
              CALL extract_token(inline, ',', 512, start_ndx, tokenlen, ctoken)
              filename = ctoken(1:tokenlen)
              IF (TRIM(filename) == '') THEN
                CALL message('em_read', 'Empty filename for sector '//TRIM(secname)  &
                            //' in spite of EF_FILE!')
                lcorrect = .FALSE.
              END IF
              CALL extract_token(inline, ',', 64, start_ndx, tokenlen, ctoken)
              varname = ctoken(1:tokenlen)
              IF (TRIM(varname) == '') THEN
                CALL message('em_read', 'Empty varname for sector '//TRIM(secname)  &
                            //' in spite of EF_FILE!')
                lcorrect = .FALSE.
              END IF
            END IF

            CALL extract_token(inline, '#', 256, start_ndx, tokenlen, ctoken)
            domainstring = toupper(ctoken(1:tokenlen))
            ierr = 0
            SELECT CASE (eftype)
            CASE (EF_INACTIVE)     ! nothing to be done
            CASE (EF_FILE)
              CALL parse_ds(domainstring, ierr, errmsg, emtype, &
                            efgeometry=efgeometry, eftimedef=eftimedef, eftimeoffset=eftimeoffset, &
                            efinterpolate=efinterpolate, efnlev=efnlev)
            CASE (EF_VALUE)
              CALL parse_ds(domainstring, ierr, errmsg, emtype, efvalue=efvalue)
            CASE (EF_MODULE)
              CALL parse_ds(domainstring, ierr, errmsg, emtype)
            CASE DEFAULT                ! error branch: message already printed above
            END SELECT

            IF (ierr /= 0) THEN
              CALL message('em_read', errmsg)
              CALL message('', 'sector = '//TRIM(secname)//', domainstring = '//TRIM(domainstring))
              lcorrect = .FALSE.
            END IF

            IF (eftype >= 0) THEN
              IF (nsectors <= maxsectors) THEN
                ematrix%em_sectors(nsectors)%es_sectorname   = secname
                ematrix%em_sectors(nsectors)%es_file         = filename
                ematrix%em_sectors(nsectors)%es_varname      = varname
                ematrix%em_sectors(nsectors)%es_eftype       = eftype   
                ematrix%em_sectors(nsectors)%es_efgeometry   = efgeometry
                ematrix%em_sectors(nsectors)%es_eftimedef    = eftimedef
                ematrix%em_sectors(nsectors)%es_eftimeoffset = eftimeoffset
                ematrix%em_sectors(nsectors)%es_efinterpolate= efinterpolate
                ematrix%em_sectors(nsectors)%es_efvalue      = efvalue  
                ematrix%em_sectors(nsectors)%es_emtype       = emtype   
                ematrix%em_sectors(nsectors)%es_nlev         = efnlev
              ELSE
                CALL message('em_read', 'Too many sectors defined in emi_spec.txt!')
                lcorrect = .FALSE.
                ! nevertheless overread all following sector definitions to check the rest of emi_spec
                ! ==> don't leave loop but read until "MATRIX" or eof is read (don't end loop if (nsectors < maxsectors))
                !     (this might lead to more than one message of the above type)
              END IF
            END IF

            CALL get_next_dataline (iunit, inline,eof)
          ENDDO
          nsectors = MIN(nsectors,maxsectors)
          ematrix%em_nsectors = nsectors
          IF (ematrix%em_nsectors == 0) THEN
            CALL message('em_read', 'No sectors defined in emi_spec.txt!')
            lcorrect = .FALSE.
          END IF

  ! second section: Species-sector-matrix

          IF (eof) THEN
            CALL message('em_read', 'MATRIX keyword not found in emi_spec!')
            lcorrect = .FALSE.
          END IF

        ! first get the matrix header line

          CALL get_next_dataline (iunit, inline, eof)
          IF (.NOT. eof) THEN
            start_ndx = 1
            CALL condense_string(inline, tokenlen)
            i = 1
            ! get dummy string (e.g. 'SPEC' heading)
            CALL extract_token(inline, ' ', 32, start_ndx, tokenlen, ctoken, llast=llast)
            ! extract sector names and associate them with sector definition above
            DO WHILE (.NOT. llast)
              CALL extract_token(inline, ' ', 32, start_ndx, tokenlen, ctoken, llast=llast)
              DO j=1, ematrix%em_nsectors
                IF (ctoken(1:tokenlen) == TRIM(ematrix%em_sectors(j)%es_sectorname)) matvec(i)=j
              END DO
              IF (matvec(i) == -1 .AND. .NOT. llast) THEN
                CALL message('em_read', 'No sector definition for sector '//TRIM(ctoken)//'!')
              END IF
              i = i + 1
            END DO
            ! nheadersec = number of sectors defined in matrix header line
            ! i-2: last token with tokenlen=0 is read and additionally i is incremented
            nheadersec = i - 2
          ELSE
            CALL message('em_read', 'matrix header line (and matrix) missing in emi_spec!')
            lcorrect = .FALSE.
            nheadersec = 0
          END IF

          IF(ANY(matvec(1:nheadersec)==-1)) THEN
            IF (nheadersec > 0) CALL message('em_read', 'undefined sector(s) found in matrix header line')
            lcorrect = .FALSE.
          END IF
    
        ! now read matrix
          DO WHILE ((toupper(inline(1:5)) /= 'ALIAS') .AND. (.NOT. eof))
            CALL get_next_dataline (iunit, inline,eof)
            IF (.NOT. eof) THEN
              start_ndx = 1
              CALL condense_string(inline, tokenlen)
              i = 1
              ! get variable (i.e. species) name
              CALL extract_token(inline, ' ', 32, start_ndx, tokenlen, ctoken, llast=llast)
              varname = toupper(ctoken(1:tokenlen))
              DO WHILE (.NOT. llast)
                CALL extract_token(inline, ' ', 32, start_ndx, tokenlen, ctoken, llast=llast)
                IF (matvec(i) > 0 .AND. .NOT. llast) THEN
                  IF (ematrix%em_sectors(matvec(i))%es_eftype /= EF_INACTIVE) THEN
                  ! check for "not available" character(s)
                    IF (ctoken(1:1) /= '-' .AND. ctoken(1:1) /= 'X' .AND. ctoken(1:1) /= 'x') THEN
                      factor = -1.0_dp
                      READ(ctoken, *,iostat=ierr) factor
                      IF (ierr /= 0) THEN
                        CALL message('em_read', 'factor for sector '//TRIM(ematrix%em_sectors(matvec(i))%es_sectorname)//&
                                                ' of species '//TRIM(varname)//' is not a number!')
                        lcorrect = .FALSE.
                      END IF
                      nvars = ematrix%em_sectors(matvec(i))%es_nvars + 1
                      ematrix%em_sectors(matvec(i))%es_nvars = nvars
                      ematrix%em_sectors(matvec(i))%es_variables(nvars)%ev_varname = varname
                      ematrix%em_sectors(matvec(i))%es_variables(nvars)%ev_factor = factor
                      ematrix%em_sectors(matvec(i))%es_variables(nvars)%ev_translation = "none"
                    END IF
                  END IF
                END IF
                i = i + 1
              END DO
              IF (((i-2) .lt. nheadersec) .AND. (TRIM(varname) /= 'ALIAS')) THEN
                CALL message('em_read', 'incomplete matrix line for species '//TRIM(varname))
                lcorrect = .FALSE.
              ENDIF
            END IF
          END DO

  ! third section translation table (optional!)

          DO WHILE (.NOT. eof)
            CALL get_next_dataline (iunit, inline,eof)
            IF (.NOT. eof) THEN
              start_ndx = 1
              CALL condense_string(inline, tokenlen)
              CALL extract_token(inline, ' ', 32, start_ndx, tokenlen, ctoken)
              varname = toupper(ctoken(1:tokenlen))
              CALL extract_token(inline, ' ', 32, start_ndx, tokenlen, ctoken)
              translation = ctoken(1:tokenlen)
              ! read aliassectors
              CALL extract_token(inline, '#', 256, start_ndx, tokenlen, ctoken)
              IF (tokenlen==0) THEN
                ! apply aliasname to ALL sectors of that variable
                DO i=1,nheadersec
                  IF (matvec(i) > 0) THEN
                    DO j= 1,ematrix%em_sectors(matvec(i))%es_nvars
                      IF (TRIM(ematrix%em_sectors(matvec(i))%es_variables(j)%ev_varname) == TRIM(varname)) &
                        ematrix%em_sectors(matvec(i))%es_variables(j)%ev_translation=translation
                    END DO
                  END IF
                END DO
              ELSE
                ! apply aliasname of that variable only to given sectors
                llast = .false.
                inline=ctoken
                start_ndx=1
                CALL condense_string(inline, tokenlen)
                DO WHILE (.NOT. llast)
                  CALL extract_token(inline, ' ', 32, start_ndx, tokenlen, ctoken, llast=llast)
                  DO i=1,nheadersec
                    IF (matvec(i) > 0) THEN
                      IF (TRIM(ematrix%em_sectors(matvec(i))%es_sectorname) == ctoken(1:tokenlen)) THEN
                        DO j= 1,ematrix%em_sectors(matvec(i))%es_nvars
                          IF (TRIM(ematrix%em_sectors(matvec(i))%es_variables(j)%ev_varname) == TRIM(varname)) &
                            ematrix%em_sectors(matvec(i))%es_variables(j)%ev_translation=translation
                        END DO
                      END IF
                    END IF
                  END DO
                END DO
              END IF
            END IF
          END DO
        END IF  ! eof (no information)
      END IF  ! iostat (emi_spec) /= 0
    !END IF  ! p_parallel_io

    !CALL p_bcast(lcorrect, p_io, p_comm_work)
    IF (.NOT. lcorrect) CALL finish('em_read','program stopped due to errors!')
    
    ! distribute across all processors 
    !CALL p_bcast_em(ematrix, p_io, comm=p_comm_work)
    !CALL p_bcast(nsectors,p_io, p_comm_work)
 
  END SUBROUTINE em_read

! !---------------------------------------------------------------------------------------------------

  SUBROUTINE extract_token(cstring, cdelim, maxlen, start_ndx, tokenlen, ctoken, lfound, llast)
  
    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(IN)        :: cstring
    CHARACTER(LEN=1), INTENT(IN)        :: cdelim
    INTEGER, INTENT(IN)                 :: maxlen
    INTEGER, INTENT(INOUT)              :: start_ndx
    INTEGER, INTENT(OUT)                :: tokenlen
    CHARACTER(LEN=maxlen), INTENT(OUT)  :: ctoken
    LOGICAL, OPTIONAL, INTENT(OUT)      :: lfound, llast

    CHARACTER(LEN=512)         :: ctmp
    INTEGER                    :: istop
    LOGICAL                    :: lofound, lolast

    lofound = .FALSE.
    lolast = .FALSE.
    IF (start_ndx <= 0) start_ndx = 1     ! make sure it works
    ! check for end of input string
    IF (start_ndx > LEN_TRIM(cstring)) THEN
      start_ndx = LEN_TRIM(cstring)+1
      lolast = .TRUE.
      ctoken = ''
    ELSE
      ! check for delimiter
      istop = INDEX(cstring(start_ndx:), cdelim)
      IF (istop == 0) THEN        ! delimiter not found - assume here comes the last token
        lolast = .TRUE.
        ctmp = TRIM(ADJUSTL(cstring(start_ndx:)))   
        start_ndx = LEN_TRIM(cstring)+1
      ELSE                        ! delimiter found
        lofound = .TRUE. 
        ctmp = TRIM(ADJUSTL(cstring(start_ndx:start_ndx+istop-2)))
        start_ndx = start_ndx + istop  ! points to one character behind delimiter
      END IF
      ! return token
      IF (LEN_TRIM(ctmp) > maxlen) THEN
        ctoken = ctmp(1:maxlen)
      ELSE
        ctoken = TRIM(ctmp)
      END IF
    END IF
    tokenlen = LEN_TRIM(ctoken)
    IF (PRESENT(lfound)) THEN
      lfound = lofound
    END IF
    IF (PRESENT(llast)) THEN
      llast = lolast
    END IF

  END SUBROUTINE extract_token
   
! !---------------------------------------------------------------------------------------------------

  SUBROUTINE condense_string(cstring, len)
  
    IMPLICIT NONE
    
    CHARACTER(LEN=*), INTENT(INOUT)  :: cstring
    INTEGER, INTENT(OUT)             :: len

    INTEGER                    :: i, j
    LOGICAL                    :: lblank

    lblank = .FALSE.
    len = LEN_TRIM(cstring)
    IF (len == 0) RETURN

    j = 0
    ! ignore leading blanks
    i = 1
    DO WHILE (cstring(i:i) == ' ')
      i = i+1
    END DO
    DO i=1,len
      IF (lblank) THEN                 ! a blank has been found at the last step
        IF (cstring(i:i) /= ' ') THEN  ! ignore another blank
          lblank = .FALSE.
          j = j+1
          cstring(j:j) = cstring(i:i)
        END IF
      ELSE                             ! last character was not a blank -> copy
        j = j+1
        cstring(j:j) = cstring(i:i)
        IF (cstring(i:i) == ' ') lblank = .TRUE.
      END IF
    END DO
    cstring(j+1:) = ' '

    len = j

  END SUBROUTINE condense_string
  
! !---------------------------------------------------------------------------------------------------

  SUBROUTINE parse_ds(cstring, ierr, errmsg, emtype, efgeometry, eftimedef, eftimeoffset, efinterpolate, efvalue, efnlev)

    IMPLICIT NONE

    CHARACTER(LEN=*),   INTENT(IN)  :: cstring
    INTEGER,            INTENT(OUT) :: ierr
    CHARACTER(LEN=256), INTENT(OUT) :: errmsg
    INTEGER,            INTENT(OUT) :: emtype
    INTEGER, OPTIONAL,  INTENT(OUT) :: efgeometry, eftimedef, efinterpolate, efnlev
    REAL(dp), OPTIONAL, INTENT(OUT) :: efvalue, eftimeoffset

    INTEGER           :: start_ndx, hstart_ndx, tokenlen, htokenlen, i
    LOGICAL           :: llast, lfound, lnum
    CHARACTER(LEN=32) :: ctoken, chtoken

    !-- set default values
    ierr = 0
    errmsg = ''
    emtype     = EM_SURFACE
    IF (PRESENT(efgeometry)) THEN
      efgeometry = EF_LONLAT    ! this is typical for emission files
    END IF
    IF (PRESENT(eftimedef)) THEN
      eftimedef  = EF_TIMERESOLVED
    END IF
    IF (PRESENT(eftimeoffset)) THEN
      eftimeoffset  = 0._dp
    END IF
    IF (PRESENT(efvalue)) THEN
      efvalue = 0._dp
    END IF
    IF (PRESENT(efinterpolate)) THEN
      efinterpolate = EF_NOINTER
    END IF

    !-- split domainstring at commas and analyze each substring
    ! note: at least one sub string must be given
    IF (LEN_TRIM(cstring) == 0) THEN
      ierr = 1
      errmsg = 'Empty attribute string! You must at least provide a value for '  &
              //'the application mode (surface, volume, etc.)'
      RETURN
    END IF

    start_ndx = 1    ! absolute position in string
    llast = .FALSE.
    DO WHILE(.NOT. llast)
      CALL extract_token(cstring, ',', 32, start_ndx, tokenlen, ctoken, llast=llast)
      lfound = .FALSE.
      ! look for known tokens (token is uppercase because domainstring is uppercase)
      IF (PRESENT(efgeometry)) THEN
        SELECT CASE (TRIM(ctoken))
        CASE ('EF_LONLAT')
          efgeometry = EF_LONLAT
          lfound = .TRUE.
        CASE ('EF_3D')
          efgeometry = EF_3D
          lfound = .TRUE.
        CASE ('EF_LEV')
          efgeometry = EF_LEV
          lfound = .TRUE.
        CASE ('EF_SINGLE')
          efgeometry = EF_SINGLE
          lfound = .TRUE.
        CASE DEFAULT       !! nothing, but valid
        END SELECT
      END IF
      IF (PRESENT(eftimedef)) THEN
        SELECT CASE (TRIM(ctoken))
        CASE ('EF_TIMERESOLVED')
          eftimedef = EF_TIMERESOLVED
          lfound = .TRUE.
        CASE ('EF_IGNOREYEAR')
          eftimedef = EF_IGNOREYEAR
          lfound = .TRUE.
        CASE ('EF_CONSTANT')
          eftimedef = EF_CONSTANT
          lfound = .TRUE.
        CASE DEFAULT
        END SELECT
      END IF
 
      IF (.NOT. lfound) THEN
        SELECT CASE (TRIM(ctoken))
        CASE ('SURFACE')
          emtype = EM_SURFACE
          lfound = .TRUE.
        CASE ('VOLUME')
          emtype = EM_VOLUME
          lfound = .TRUE.
        CASE ('LEVEL50M')
          emtype = EM_LEVEL50M
          lfound = .TRUE.
        CASE ('FIRE')
          emtype = EM_FIRE
          lfound = .TRUE.
        CASE DEFAULT
        END SELECT
      END IF
      IF (PRESENT(eftimeoffset)) THEN
        hstart_ndx = 1
        CALL extract_token(ctoken, '=', 32, hstart_ndx, htokenlen, chtoken)
        IF (TRIM(chtoken) == 'EF_TIMEOFFSET') THEN
          CALL extract_token(ctoken, '=', 32, hstart_ndx, htokenlen,chtoken)
          lnum = testnum(chtoken)
          IF (lnum) THEN
            READ(chtoken, *) eftimeoffset
            lfound = .TRUE.
          ELSE
            ierr = 2
            errmsg = 'Invalid floating point number! '//TRIM(chtoken)
          END IF
        END IF
      END IF
      IF (PRESENT(efinterpolate)) THEN
        hstart_ndx = 1
        CALL extract_token(ctoken, '=', 32, hstart_ndx, htokenlen, chtoken)
        IF (TRIM(chtoken) == 'EF_INTERPOLATE') THEN
          CALL extract_token(ctoken, '=', 32, hstart_ndx, htokenlen,chtoken)
          SELECT CASE (TRIM(chtoken))
          CASE ('EF_NOINTER')
            lfound = .TRUE.
          CASE ('EF_LINEAR')
            efinterpolate = EF_LINEAR
            lfound = .TRUE.
          CASE ('EF_CUBIC')
            ! still to be done ==> for the moment set to EF_LINEAR
            efinterpolate = EF_LINEAR
            lfound = .TRUE.
          CASE DEFAULT
          END SELECT
        END IF
      END IF
      IF (PRESENT(efnlev)) THEN
        hstart_ndx = 1
        CALL extract_token(ctoken, '=', 32, hstart_ndx, htokenlen, chtoken)
        IF (TRIM(chtoken) == 'NLEV') THEN
          CALL extract_token(ctoken, '=', 32, hstart_ndx, htokenlen,chtoken)
          lnum = testint(chtoken)
          IF (lnum) THEN
            READ(chtoken, *) efnlev
            lfound = .TRUE.
          ELSE
            ierr = 2
            errmsg = 'Invalid integer for nlev! '//TRIM(chtoken)
          END IF
        END IF
      END IF


      IF (PRESENT(efvalue) .AND. .NOT. lfound) THEN
        ! test if token is numerical value
        lnum = testnum(ctoken)
        IF (lnum) THEN
          READ(ctoken, *) efvalue
          lfound = .TRUE.
        ELSE
          ierr = 2
          errmsg = 'Invalid floating point number! '//TRIM(ctoken)
        END IF
      END IF
      ! if nothing has been found until now, we have an invalid token string
      IF (.NOT. lfound) THEN
        ierr = 3
        errmsg = 'Undefined token in domainstring: '//TRIM(ctoken)
        RETURN
      END IF
    END DO

    ! consistency checks
    IF (PRESENT(efgeometry)) THEN
      IF (emtype == EM_SURFACE .AND. efgeometry == EF_3D) THEN
        ierr = 4
        errmsg = 'Inconsistent setting of EM_TYPE (surface) and EF_GEOMETRY (3D)!'
      END IF
    END IF
    IF (PRESENT(efnlev)) THEN
      IF (emtype == EM_SURFACE .AND. efnlev /= -1) THEN
        ierr = 5
        errmsg = 'Number of levels (nlev) specified for EM_TYPE (surface)'
      END IF
    END IF

  END SUBROUTINE parse_ds

! !---------------------------------------------------------------------------------------------------

  FUNCTION testnum(string) RESULT(lnum)

    IMPLICIT NONE

    CHARACTER(len=*), INTENT(IN)    :: string

    INTEGER                         :: i, j, jmax
    LOGICAL                         :: lnum, lsign
    CHARACTER(len=16)               :: ctest

    lnum = .TRUE.
    ctest = '0123456789+-.'     ! must not contain exponential sign in position 1
    j = -1
    jmax = 3
    DO i=1,LEN_TRIM(string)
      lsign = (string(1:1) == '+' .OR. string(1:1) == '-')
      IF (INDEX(TRIM(ctest), string(i:i)) == 0) lnum = .FALSE.
      IF (i == 1) ctest = '0123456789.eE'      ! sign not allowed again until after 'e', 'E'
      IF (string(i:i) == '.') THEN
        ctest = '0123456789eE'    ! only one decimal point allowed
        IF (i == 1 .AND. LEN_TRIM(string) == 1) lnum = .FALSE.   ! avoid dot-only
        IF (lsign .AND. i == 2 .AND. LEN_TRIM(string) == 2) lnum = .FALSE.
      END IF
      IF (string(i:i) == 'e' .OR. string(i:i) == 'E') THEN
        ctest = '0123456789+-'
        j = 0
      END IF
      IF (j == 1 .AND. (string(i:i) == '+' .OR. string(i:i) == '-')) jmax = 4
      IF (j > 0) ctest = '0123456789'
      IF (j >= 0) j = j+1              ! count digits of exponent
      IF (j > jmax) lnum = .FALSE.        ! overflow
    END DO

  END FUNCTION testnum

! !---------------------------------------------------------------------------------------------------

  FUNCTION testint(string) RESULT(lnum)

    IMPLICIT NONE

    CHARACTER(len=*), INTENT(in)    :: string
    LOGICAL                         :: lnum
    CHARACTER(len=10)               :: ctest
    INTEGER                         :: i, j, jmax

    lnum = .TRUE.
    ctest = '0123456789' ! must not contain exponential sign in position 1
    j = -1
    jmax = 3
    DO i=1,LEN_TRIM(string)
      IF (INDEX(TRIM(ctest), string(i:i)) == 0) lnum = .FALSE.
    END DO

  END FUNCTION testint

! !---------------------------------------------------------------------------------------------------

  !SUBROUTINE p_bcast_em (em_struc, p_source, comm)
  !
  !  USE mo_mpi_lite, ONLY: p_bcast, p_all_comm => process_mpi_all_comm 
  !
  !  IMPLICIT NONE
  !
  !  INTEGER,           INTENT(IN)    :: p_source
  !  INTEGER, OPTIONAL, INTENT(IN)    :: comm
  !  TYPE(emi_matrix),  INTENT(INOUT) :: em_struc
  !
  !  INTEGER :: p_comm, jsec, jvar
  !
  !  IF (PRESENT(comm)) THEN
  !    p_comm = comm
  !  ELSE
  !    p_comm = p_all_comm
  !  ENDIF
  !
  !  CALL p_bcast(em_struc%em_nsectors,    p_source, p_comm)
  !  DO jsec=1, em_struc%em_nsectors
  !    CALL p_bcast(em_struc%em_sectors(jsec)%es_sectorname,   p_source, p_comm)
  !    CALL p_bcast(em_struc%em_sectors(jsec)%es_file,         p_source, p_comm)
  !    CALL p_bcast(em_struc%em_sectors(jsec)%es_varname,      p_source, p_comm)
  !    CALL p_bcast(em_struc%em_sectors(jsec)%es_nvars,        p_source, p_comm)
  !    CALL p_bcast(em_struc%em_sectors(jsec)%es_eftype,       p_source, p_comm)
  !    CALL p_bcast(em_struc%em_sectors(jsec)%es_efgeometry,   p_source, p_comm)
  !    CALL p_bcast(em_struc%em_sectors(jsec)%es_eftimedef,    p_source, p_comm)
  !    CALL p_bcast(em_struc%em_sectors(jsec)%es_eftimeoffset, p_source, p_comm)
  !    CALL p_bcast(em_struc%em_sectors(jsec)%es_efinterpolate,p_source, p_comm)
  !    CALL p_bcast(em_struc%em_sectors(jsec)%es_efvalue,      p_source, p_comm)
  !    CALL p_bcast(em_struc%em_sectors(jsec)%es_emtype,       p_source, p_comm)
  !    CALL p_bcast(em_struc%em_sectors(jsec)%es_nlev,         p_source, p_comm)
  !    DO jvar=1, em_struc%em_sectors(jsec)%es_nvars
  !      CALL p_bcast(em_struc%em_sectors(jsec)%es_variables(jvar)%ev_varname,     p_source, p_comm)
  !      CALL p_bcast(em_struc%em_sectors(jsec)%es_variables(jvar)%ev_translation, p_source, p_comm)
  !      CALL p_bcast(em_struc%em_sectors(jsec)%es_variables(jvar)%ev_factor,      p_source, p_comm)
  !    END DO
  !  END DO
  !
  !END SUBROUTINE p_bcast_em

! !---------------------------------------------------------------------------------------------------

  SUBROUTINE em_get_sector_info(i, sectorname, nvars)

    USE mo_exception_lite,   ONLY: message, message_text

    IMPLICIT NONE

    INTEGER, INTENT(IN)             :: i
    INTEGER, INTENT(OUT)            :: nvars
    CHARACTER(LEN=64), INTENT(OUT)  :: sectorname

    IF (i > ematrix%em_nsectors) THEN
      WRITE(message_text,'(2(a,i0),a)') 'Sector index',i,' larger than',ematrix%em_nsectors,'! Should not happen!!'
      CALL message('em_get_sector_info', message_text)
    END IF

    sectorname = ematrix%em_sectors(i)%es_sectorname
    nvars      = ematrix%em_sectors(i)%es_nvars

  END SUBROUTINE

! !---------------------------------------------------------------------------------------------------

  SUBROUTINE em_get_bc_from_matrix( i, j, sectorname, bc_struc, emtype, emfactor, varname)

    USE mo_run_config_lite,          ONLY: nlev
    USE mo_exception_lite,           ONLY: message
    USE mo_util_filename_lite,       ONLY: str_filter
    USE mo_util_extfield_lite,       ONLY: EF_NOINTER
    USE mo_subm_main_lite,           ONLY: emi_basepath
    USE mo_subm_bcs_lite,            ONLY: bc_nml, BC_EVERYWHERE, BC_BOTTOM, BC_LEVEL, BC_REPLACE

    IMPLICIT NONE

    INTEGER,          INTENT(IN)   :: i, j
    CHARACTER(LEN=*), INTENT(IN)   :: sectorname
    TYPE(bc_nml),     INTENT(OUT)  :: bc_struc
    INTEGER,          INTENT(OUT)  :: emtype
    REAL(dp),         INTENT(OUT)  :: emfactor  ! emission scaling factor
    CHARACTER(LEN=64), INTENT(OUT) :: varname   ! actual name of a species

    INTEGER            :: ndx, ndx_c, start_ndx, stop_ndx
    LOGICAL            :: found
    CHARACTER(LEN=512) :: cfile, help

    !-- set default values
    emfactor = 1._dp
    bc_struc%ef_interpolate = EF_NOINTER      !! add to parse_ds later (?)
    bc_struc%ef_actual_unit= ''               !! add to parse_ds later (?)
    bc_struc%bc_mode = BC_REPLACE             !! needed for extended emi diagnostics

    !-- fill easy stuff
    emtype = ematrix%em_sectors(i)%es_emtype
    varname = ematrix%em_sectors(i)%es_variables(j)%ev_varname
    bc_struc%ef_type       = ematrix%em_sectors(i)%es_eftype
    bc_struc%ef_geometry   = ematrix%em_sectors(i)%es_efgeometry
    bc_struc%ef_timedef    = ematrix%em_sectors(i)%es_eftimedef
    bc_struc%ef_timeoffset = ematrix%em_sectors(i)%es_eftimeoffset
    bc_struc%ef_interpolate= ematrix%em_sectors(i)%es_efinterpolate
    bc_struc%ef_value      = ematrix%em_sectors(i)%es_efvalue
    bc_struc%ef_nlev       = ematrix%em_sectors(i)%es_nlev

    !-- construct correct file name template
    IF (bc_struc%ef_type == EF_FILE) THEN
      IF (LEN_TRIM(ematrix%em_sectors(i)%es_file) == 0) THEN
        CALL message('em_get_bc_from_matrix', 'Empty file name for EF_FILE in ' &
                    //TRIM(ematrix%em_sectors(i)%es_sectorname))
      END IF
      ndx_c = INDEX(ematrix%em_sectors(i)%es_file,'%C')
      IF (ndx_c > 0) THEN
        help = ematrix%em_sectors(i)%es_file
        help(ndx_c:ndx_c)='$'
        ndx=INDEX(help,'%')
        DO WHILE (ndx > 0)
          help(ndx:ndx)='$'
          ndx=INDEX(help,'%')
        ENDDO
        help(ndx_c:ndx_c)='%'
        IF (TRIM(ematrix%em_sectors(i)%es_variables(j)%ev_translation) /= "none") THEN
          cfile = str_filter(help,0, 0, 0, 0, 0, 0,0,TRIM(ematrix%em_sectors(i)%es_variables(j)%ev_translation),"T00","L00")
        ELSE
          cfile = str_filter(help,0, 0, 0, 0, 0, 0,0,TRIM(ematrix%em_sectors(i)%es_variables(j)%ev_varname),"T00","L00")
        ENDIF
        ndx=INDEX(cfile,'$')
        DO WHILE (ndx > 0)
          cfile(ndx:ndx)='%'
          ndx=INDEX(cfile,'$')
        ENDDO

        IF (cfile(1:1) == '/') THEN
          bc_struc%ef_template = TRIM(cfile)
        ELSE
          bc_struc%ef_template = TRIM(emi_basepath)//'/'//TRIM(cfile)
        ENDIF
      ELSE
        IF (ematrix%em_sectors(i)%es_file(1:1) == '/') THEN
          bc_struc%ef_template = TRIM(ematrix%em_sectors(i)%es_file)
        ELSE
          bc_struc%ef_template = TRIM(emi_basepath)//'/'//TRIM(ematrix%em_sectors(i)%es_file)
        ENDIF
      ENDIF
    ENDIF

    !-- construct bc_domain value
    ! note: except for "volume" emissions (i.e. aircraft, volcanoes) all boundary conditions
    ! are defined as BC_BOTTOM, i.e. as 2D fields. Vertical distribution takes place where
    ! necessary in emi_interface in mo_emission_interface.
    SELECT CASE (ematrix%em_sectors(i)%es_emtype)
    CASE (EM_SURFACE)
      bc_struc%bc_domain = BC_BOTTOM
    CASE (EM_VOLUME)
      bc_struc%bc_domain = BC_EVERYWHERE
      !! WARNING: this might be wrong (e.g. aircraft)
      !! ==> if read from file: BC_ALTITUDE || BC_PRESSURE is set accordingly after reading
    CASE (EM_LEVEL50M)
      bc_struc%bc_domain = BC_BOTTOM
  !   bc_struc%bc_domain = BC_LEVEL
  !   bc_struc%bc_minlev = nlev-1
  !   bc_struc%bc_maxlev = nlev-1
    CASE (EM_FIRE) 
      IF (bc_struc%ef_geometry == EF_3D) THEN
          bc_struc%bc_domain = BC_EVERYWHERE
      ELSE
          bc_struc%bc_domain = BC_BOTTOM
      END IF
    END SELECT

    !--- replace %C0 in varname field by species name 
    IF (index(ematrix%em_sectors(i)%es_varname,'%') > 0) THEN
      IF (trim(ematrix%em_sectors(i)%es_variables(j)%ev_translation) == "none") THEN
        bc_struc%ef_varname = trim(ematrix%em_sectors(i)%es_variables(j)%ev_varname)
      ELSE
        bc_struc%ef_varname = trim(ematrix%em_sectors(i)%es_variables(j)%ev_translation)
      ENDIF
    ELSE
      bc_struc%ef_varname = trim(ematrix%em_sectors(i)%es_varname)
    ENDIF
    emfactor = ematrix%em_sectors(i)%es_variables(j)%ev_factor

  END SUBROUTINE em_get_bc_from_matrix

! !---------------------------------------------------------------------------------------------------

!   SUBROUTINE em_add_spec_to_sector(i, factor, varname)

!     IMPLICIT NONE

!     INTEGER,           INTENT(IN) :: i
!     REAL(dp),          INTENT(IN) :: factor  ! emission scaling factor
!     CHARACTER(LEN=64), INTENT(IN) :: varname   ! actually name of a species

!     INTEGER :: nvars

!     nvars = ematrix%em_sectors(i)%es_nvars + 1
!     ematrix%em_sectors(i)%es_nvars  = nvars

!     ematrix%em_sectors(i)%es_variables(nvars)%ev_varname = varname
!     ematrix%em_sectors(i)%es_variables(nvars)%ev_factor = factor

!   END SUBROUTINE em_add_spec_to_sector

! !---------------------------------------------------------------------------------------------------

!   FUNCTION em_get_SectorIndex (name) RESULT(index)

!     USE mo_exception,   ONLY: message

!     IMPLICIT NONE

!     CHARACTER(LEN=*), INTENT(IN) :: name

!     INTEGER :: index, i
!     LOGICAL :: found 
 
!     i = 0
!     found = .false.
!     DO WHILE ((.NOT. found) .AND. (i <= ematrix%em_nsectors))
!       i = i + 1
!       IF (ematrix%em_sectors(i)%es_sectorname == name) found = .true.
!     ENDDO
!     IF (i .gt. ematrix%em_nsectors) CALL message('em_get_bc_from_matrix', 'no sector '//trim(name)//' defined!')
!     index = i

!   END FUNCTION em_get_SectorIndex

END MODULE mo_hammoz_lite_emi_mat
