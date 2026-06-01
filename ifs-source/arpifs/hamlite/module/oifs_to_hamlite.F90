MODULE OIFS_TO_HAMLITE
!-----------------------------------------------------------------------
! Module for mapping indices between OpenIFS GFL tracers and HAMlite
! internal tracers for the simplified 4-mode HAMlite aerosol scheme.
!
! HAMlite uses 4 native groups: DU(dust), SS(sea-salt), CA(carbon), SU(sulfate)
! Each group has a number tracer (DU_N, SS_N, CA_N, SU_N)
! and a mass tracer (DU_M, SS_M, CA_M, SU_M).
!
! Native tracer conversion: 31-MAR-2026
!-----------------------------------------------------------------------
USE PARKIND1, ONLY: JPIM, JPRB

IMPLICIT NONE

PRIVATE
PUBLIC :: ind_oifs_hamlite_type, ind_oifs_hamlite, init_ind_oifs_hamlite
PUBLIC :: NHLMODES, NHLMASS, NHLGAS, NHLCLOUD
PUBLIC :: NHLCLASS, CLASS_NAMES, MASS_NAMES

! Number of HAMlite groups (DU, SS, CA, SU)
INTEGER(KIND=JPIM), PARAMETER :: NHLMODES = 4

! Number of native mass tracers (1:1 with modes)
INTEGER(KIND=JPIM), PARAMETER :: NHLMASS = 4

! Number of gas-phase tracers (DMS, SO2)
INTEGER(KIND=JPIM), PARAMETER :: NHLGAS = 2

! Number of cloud tracers (CDNC, ICNC)
INTEGER(KIND=JPIM), PARAMETER :: NHLCLOUD = 2

! Number of number-class tracers
INTEGER(KIND=JPIM), PARAMETER :: NHLCLASS = 4

! Names of the 4 native number-class tracers in OIFS YAERO
CHARACTER(LEN=8), PARAMETER :: CLASS_NAMES(NHLCLASS) = (/ &
   & 'DU_N    ', &  ! dust number
   & 'SS_N    ', &  ! sea-salt number
   & 'CA_N    ', &  ! carbonaceous number
   & 'SU_N    '  &  ! sulfate number
   & /)

! Names of the 4 native mass tracers in OIFS YAERO
CHARACTER(LEN=8), PARAMETER :: MASS_NAMES(NHLMASS) = (/ &
   & 'DU_M    ', &  ! dust mass
   & 'SS_M    ', &  ! sea-salt mass
   & 'CA_M    ', &  ! carbonaceous mass
   & 'SU_M    '  &  ! sulfate mass
   & /)

! Index mapping type
TYPE ind_oifs_hamlite_type
   ! Indices into KAERO (OIFS aerosol tracer positions) for each HAMlite variable
   INTEGER(KIND=JPIM) :: ind_class_OIFS(NHLCLASS)       ! 4 number classes
   INTEGER(KIND=JPIM) :: ind_mass_OIFS(NHLMASS)         ! 4 mass tracers
   INTEGER(KIND=JPIM) :: ind_gas_OIFS(NHLGAS)           ! 2 gas tracers (DMS, SO2)
   INTEGER(KIND=JPIM) :: ind_cloud_OIFS(NHLCLOUD)       ! 2 cloud tracers (CDNC, ICNC)
END TYPE ind_oifs_hamlite_type

TYPE(ind_oifs_hamlite_type), SAVE :: ind_oifs_hamlite

CONTAINS

SUBROUTINE init_ind_oifs_hamlite()
   ! Initialize all index arrays to zero (unmapped)
   ind_oifs_hamlite%ind_class_OIFS(:) = 0
   ind_oifs_hamlite%ind_mass_OIFS(:)  = 0
   ind_oifs_hamlite%ind_gas_OIFS(:)   = 0
   ind_oifs_hamlite%ind_cloud_OIFS(:) = 0
END SUBROUTINE init_ind_oifs_hamlite

END MODULE OIFS_TO_HAMLITE
