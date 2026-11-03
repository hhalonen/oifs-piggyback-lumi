SUBROUTINE HAMLITE_INIT(YGFL, YRRIP, YDGEOMETRY, CHEM_SCHEME)
!-----------------------------------------------------------------------
! Initialize HAMlite aerosol scheme within OpenIFS.
! Called from CNT4 when AERO_SCHEME="hamlite".
!
! Steps:
!   1. Create a dummy t_patch from OIFS geometry
!   2. Initialize HAMlite internal data structures
!   3. Populate index mapping between OIFS GFL tracers and HAMlite
!
! Native tracer conversion: 31-MAR-2026
!-----------------------------------------------------------------------
USE PARKIND1,        ONLY: JPIM, JPRB
USE YOMHOOK,         ONLY: LHOOK, DR_HOOK, JPHOOK
USE YOM_YGFL,        ONLY: TYPE_GFLD
USE YOMRIP,          ONLY: TRIP
USE YOMLUN,          ONLY: NULOUT
USE GEOMETRY_MOD,    ONLY: GEOMETRY

! HAMlite config default initializers
USE mo_lite_main_list_config,       ONLY: init_defaults_lite_main_list
USE mo_lite_dust_list_config,       ONLY: init_defaults_lite_dust_list
USE mo_subm_main_list_config_lite,  ONLY: init_defaults_subm_main_list
USE mo_subm_diag_list_config_lite,  ONLY: init_defaults_subm_diag_list
USE mo_subm_interface_lite,         ONLY: init_hamlite_submodel

! Bridge OpenIFS MPI (FIAT/MPL) to HAMlite MPI (mo_mpi_lite)
USE MPL_DATA_MODULE, ONLY: MPL_COMM, MPL_RANK, MPL_NUMPROC
USE mo_mpi_lite, ONLY: p_comm_work, p_pe, p_pe_work, p_io, p_n_work, &
   & process_mpi_all_comm, process_mpi_all_size, my_process_mpi_all_id

! HAMlite core modules
USE mo_ham_lite_init,              ONLY: ham_lite_start, ham_lite_tracer, ham_lite_init
USE mo_model_domain_lite,          ONLY: t_patch
USE mtime_lite,                    ONLY: t_datetime => datetime
USE mo_kind_lite,                  ONLY: wp
USE mo_ham_lite_misc,              ONLY: pm6rp,prhop,prelhum
USE mo_parallel_config_lite,       ONLY: set_nproma
USE mo_ham_lite_main,              ONLY: nclass
USE mo_aes_convect_tables_lite,    ONLY: init_convect_tables
USE mo_ham_lite_seasalt,           ONLY: start_emi_seasalt

! OIFS to HAMlite index mapping
USE OIFS_TO_HAMLITE,  ONLY: ind_oifs_hamlite, init_ind_oifs_hamlite, &
     & NHLCLASS, NHLMASS, NHLGAS, NHLCLOUD, CLASS_NAMES, MASS_NAMES

IMPLICIT NONE

TYPE(TYPE_GFLD),     INTENT(IN) :: YGFL
TYPE(TRIP),          INTENT(IN) :: YRRIP
TYPE(GEOMETRY),      INTENT(IN) :: YDGEOMETRY
CHARACTER(LEN=20),   INTENT(IN) :: CHEM_SCHEME

!<--eehol: Type variables for allocating patch and initializations
TYPE(t_datetime), TARGET :: some_datetime
TYPE(t_datetime), POINTER :: this_datetime
!-->eehol

INTEGER(KIND=JPIM) :: JM, JA, JC
CHARACTER(LEN=16)  :: ZNAME
REAL(KIND=JPHOOK)  :: ZHOOK_HANDLE
TYPE(t_patch)      :: ZDUMMY_PATCH(1)

#include "abor1.intfb.h"

IF (LHOOK) CALL DR_HOOK('HAMLITE_INIT',0,ZHOOK_HANDLE)

WRITE(NULOUT,'(A)') '=================================================='
WRITE(NULOUT,'(A)') 'HAMLITE_INIT: Initializing HAMlite (native tracers)'
WRITE(NULOUT,'(A)') '=================================================='

! getting the correct time step:
!CALL init_mo_time_control(YRRIP)

! --- 1. Build dummy t_patch from OIFS geometry ---
ZDUMMY_PATCH(1)%nblks_c = YDGEOMETRY%YRDIM%NGPBLKS
ZDUMMY_PATCH(1)%nlev    = YDGEOMETRY%YRDIMV%NFLEVG
ZDUMMY_PATCH(1)%nlevp1  = YDGEOMETRY%YRDIMV%NFLEVG + 1
ZDUMMY_PATCH(1)%id      = 1

WRITE(NULOUT,'(A,I6,A,I4)') '  OIFS geometry: NGPBLKS=', &
  YDGEOMETRY%YRDIM%NGPBLKS, ' NFLEVG=', YDGEOMETRY%YRDIMV%NFLEVG

! --- 2. Initialize HAMlite internals ---

! --- 2a. Bridge OpenIFS MPI environment to HAMlite ---
p_comm_work          = MPL_COMM
process_mpi_all_comm = MPL_COMM
process_mpi_all_size = MPL_NUMPROC
my_process_mpi_all_id = MPL_RANK
p_pe                 = MPL_RANK
p_pe_work            = MPL_RANK
p_io                 = 0
p_n_work             = MPL_NUMPROC
WRITE(NULOUT,'(A,I6,A,I8)') '  HAMlite MPI: rank=', MPL_RANK, ' comm=', MPL_COMM

!eehol: set dummy datetime
this_datetime => some_datetime
!Date: 2010-01-01 00:00:00
this_datetime%date%year = 2010
this_datetime%date%month = 1
this_datetime%date%day = 1
this_datetime%time%hour = 0
this_datetime%time%minute = 0
this_datetime%time%second = 0
this_datetime%time%ms = 0

CALL set_nproma(YDGEOMETRY%YRDIM%NGPBLKS)

CALL init_defaults_subm_main_list()
CALL init_defaults_lite_main_list()
CALL init_defaults_lite_dust_list()
CALL init_defaults_subm_diag_list()
CALL init_hamlite_submodel(ZDUMMY_PATCH,this_datetime) !eehol: init hamlite submodel

CALL init_convect_tables

WRITE(NULOUT,'(A)') '  HAMlite core initialized'

! --- 3. Initialize index mapping ---
CALL init_ind_oifs_hamlite()

! --- 4. Match number class tracers (DU_N, SS_N, CA_N, SU_N) ---
DO JM = 1, NCLASS
   DO JA = 1, YGFL%NAERO
      ZNAME = YGFL%YAERO(JA)%CNAME
      IF (TRIM(CLASS_NAMES(JM)) == TRIM(ZNAME)) THEN
         ind_oifs_hamlite%ind_class_OIFS(JM) = JA
         WRITE(NULOUT,'(A,I3,A,A,A,I3)') '  number(',JM,')=', TRIM(ZNAME), &
           ' -> YAERO idx=', JA
         EXIT
      END IF
   END DO
END DO

! --- 5. Match mass tracers (DU_M, SS_M, CA_M, SU_M) ---
DO JM = 1, NHLMASS
   DO JA = 1, YGFL%NAERO
      ZNAME = YGFL%YAERO(JA)%CNAME
      IF (TRIM(MASS_NAMES(JM)) == TRIM(ZNAME)) THEN
         ind_oifs_hamlite%ind_mass_OIFS(JM) = JA
         WRITE(NULOUT,'(A,I3,A,A,A,I3)') '  mass(',JM,')=', TRIM(ZNAME), &
           ' -> YAERO idx=', JA
         EXIT
      END IF
   END DO
END DO

! --- 6. Match cloud tracers (CDNC, ICNC) ---
DO JA = 1, YGFL%NAERO
   ZNAME = YGFL%YAERO(JA)%CNAME
   IF (TRIM(ZNAME) == 'CDNC') THEN
      ind_oifs_hamlite%ind_cloud_OIFS(1) = JA
      WRITE(NULOUT,'(A,A,A,I3)') '  cloud(1)=', TRIM(ZNAME), ' -> YAERO idx=', JA
   ELSE IF (TRIM(ZNAME) == 'ICNC') THEN
      ind_oifs_hamlite%ind_cloud_OIFS(2) = JA
      WRITE(NULOUT,'(A,A,A,I3)') '  cloud(2)=', TRIM(ZNAME), ' -> YAERO idx=', JA
   END IF
END DO

! --- 7. Match gas-phase tracers from YCHEM (DMS, SO2) ---
DO JC = 1, YGFL%NCHEM
   ZNAME = YGFL%YCHEM(JC)%CNAME
   IF (TRIM(ZNAME) == 'DMS') THEN
      ind_oifs_hamlite%ind_gas_OIFS(1) = JC
      WRITE(NULOUT,'(A,A,A,I3)') '  gas(1)=', TRIM(ZNAME), ' -> YCHEM idx=', JC
   ELSE IF (TRIM(ZNAME) == 'SO2') THEN
      ind_oifs_hamlite%ind_gas_OIFS(2) = JC
      WRITE(NULOUT,'(A,A,A,I3)') '  gas(2)=', TRIM(ZNAME), ' -> YCHEM idx=', JC
   END IF
END DO

call start_emi_seasalt()

! --- 8. Summary ---
WRITE(NULOUT,'(A)') '=================================================='
WRITE(NULOUT,'(A,I3,A,I3)') '  Number classes matched: ', &
  COUNT(ind_oifs_hamlite%ind_class_OIFS > 0), ' / ', NCLASS
WRITE(NULOUT,'(A,I3,A,I3)') '  Mass tracers matched:   ', &
  COUNT(ind_oifs_hamlite%ind_mass_OIFS > 0), ' / ', NHLMASS
WRITE(NULOUT,'(A,I3,A,I3)') '  Cloud tracers matched:  ', &
  COUNT(ind_oifs_hamlite%ind_cloud_OIFS > 0), ' / ', NHLCLOUD
WRITE(NULOUT,'(A)') 'HAMLITE_INIT: Initialization complete'
WRITE(NULOUT,'(A)') '=================================================='

IF (LHOOK) CALL DR_HOOK('HAMLITE_INIT',1,ZHOOK_HANDLE)

END SUBROUTINE HAMLITE_INIT
