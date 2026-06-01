!OPTIONS NODOUBLE
      SUBROUTINE PACK21(KWRDS,PIN,PPACKED,KDUM1,KDUM2)

!**** *PACK21  *  - Pack data by a factor 2 to 1

!     Purpose.
!     --------
!           Pack data by a factor 2 to 1 in order to save memory.
!           Simple version to be used on Fujitsu VPP.

!**   Interface.
!     ----------
!        *CALL* *PACK21(...)*

!        Explicit arguments : KWRDS   - number of words to unpack (copy)
!        -------------------- PIN     - array containing unpacked data
!                             PPACKED - buffer containing (packed) data
!                             KDUM1   - not used
!                             KDUM2   - not used

!        Implicit arguments : None
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------
!        None

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!        Lars Isaksen *ECMWF*

!     Modifications.
!     --------------
!        Original : 98-01-01
!     ------------------------------------------------------------------

      USE PARKIND1, ONLY : JPIM
!      IMPLICIT LOGICAL (L)
      IMPLICIT NONE
      INTEGER(KIND=JPIM), INTENT(IN)    :: KWRDS
      INTEGER(KIND=JPIM), INTENT(IN)    :: KDUM1
      INTEGER(KIND=JPIM), INTENT(IN)    :: KDUM2
      REAL (KIND=8) :: PIN(KWRDS)
      REAL (KIND=4) :: PPACKED(KWRDS)

!     ------------------------------------------------------------------

      PPACKED(:)=REAL(PIN(:),4)

      RETURN
      END SUBROUTINE PACK21
