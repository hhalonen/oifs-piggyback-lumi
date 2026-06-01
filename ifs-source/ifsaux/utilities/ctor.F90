      SUBROUTINE CTOR(CDCHAR_V, PREAL_V, K)
!
!**** SUBROUTINE ctor - To convert from CHAR to 64-bit REAL
!
!        S.Saarinen     ECMWF     27/6/95
!
!**   INTERFACE
!     ---------
!
!        CALL ctor(cdChar_v, Preal_v, K)
!     WHERE:
!           cDChar_v = A character-string (INPUT) which contains the
!                      the ASCII-chars to be converted
!           Preal_v  = 64-bit REAL-vector (OUTPUT) of n elements
!                      each holding one ASCII-value [0..255]
!                      of a character after conversion
!           K        = length of the REAL-vector and character-string
!
!
!-----------------------------------------------------------------------
!
      USE PARKIND1, ONLY : JPIM, JPRD
      IMPLICIT NONE
      INTEGER(KIND=JPIM) :: K
!-- The following MUST be 64-bit REALs [i.e. REAL(8) on Fortran-90]
      REAL(KIND=JPRD) :: PREAL_V(K)
      CHARACTER(LEN=*) :: CDCHAR_V
      INTEGER(KIND=JPIM) :: I

      DO I=1,K
         PREAL_V(I) = ICHAR(CDCHAR_V(I:I))
      ENDDO
      ENDSUBROUTINE CTOR
