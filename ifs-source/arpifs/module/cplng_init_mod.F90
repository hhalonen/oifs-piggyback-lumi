MODULE CPLNG_INIT_MOD

   IMPLICIT NONE

   PRIVATE

   PUBLIC CPLNG_INIT

CONTAINS

   SUBROUTINE CPLNG_INIT(LDACTIVE)

      USE PARKIND1,   ONLY: JPIM
      USE MPL_MODULE, ONLY: LMPLUSERCOMM, MPLUSERCOMM
#ifdef WITH_OASIS
      USE MOD_OASIS
#endif
      USE CPLNG_LOG_MOD
      USE CPLNG_DATA_MOD

      ! Arguments
      LOGICAL, INTENT(IN) :: LDACTIVE

      CHARACTER(LEN=6),PARAMETER :: CLMODEL_NAME = "ATMIFS"

      INTEGER(KIND=JPIM) :: INCOMPID
      INTEGER(KIND=JPIM) :: IERROR
      CHARACTER(LEN=3)   :: CLERRSTR

      ! -------------------------------------------------------------------------
      ! * (1) Early return if not active
      ! -------------------------------------------------------------------------
      IF (.NOT.LDACTIVE) THEN
         CALL CPLNG_INFO('CPLNG is switched off! (CPLNG_ACTIVE==false)')
         CALL CPLNG_INFO('Further calls to CPLNG routines will create warnings.')
         RETURN
      ENDIF

      ! -------------------------------------------------------------------------
      ! * (2) Initialise OASIS coupling
      ! -------------------------------------------------------------------------
#ifdef WITH_OASIS
      CALL OASIS_INIT_COMP(INCOMPID,CLMODEL_NAME,IERROR)
      IF (IERROR/=OASIS_OK) THEN
         WRITE (CLERRSTR,'(I3)') IERROR
         CALL ABOR1("CPLNG_INIT: Error in OASIS_INIT_COMM: "//CLERRSTR)
      ENDIF

      ! Get internal communicator from OASIS
      CALL OASIS_GET_LOCALCOMM(MPLUSERCOMM,IERROR)
      IF (IERROR/=OASIS_OK) THEN
         WRITE (CLERRSTR,'(I3)') IERROR
         CALL ABOR1("CPLNG_INIT: Error in OASIS_GET_LOCALCOMM: "//CLERRSTR)
      ENDIF

      ! -------------------------------------------------------------------------
      ! * (3) Let IFS know we've set an internal communicator
      ! -------------------------------------------------------------------------
      LMPLUSERCOMM = .TRUE.

#endif

   END SUBROUTINE CPLNG_INIT

END MODULE CPLNG_INIT_MOD
