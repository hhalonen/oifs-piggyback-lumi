      SUBROUTINE RTOC(REAL_V, CHAR_V, N)
!
!**** SUBROUTINE rtoc - To convert from 64-bit REAL to CHAR
!
!        S.Saarinen     ECMWF     27/6/95
!
!**   INTERFACE
!     ---------
!
!        CALL rtoc(real_v, char_v, n)
!     WHERE:
!           real_v = 64-bit REAL-vector (INPUT) of n elements
!                    each holding one ASCII-value [0..255]
!                    of a character
!           char_v = A character-string (OUTPUT) where the result of
!                    each ASCII to CHAR()-conversion goes
!           n      = length of the REAL-vector and character-string
!
!
!-----------------------------------------------------------------------
!
      USE PARKIND1, ONLY : JPIM
      IMPLICIT NONE
      INTEGER(KIND=JPIM) :: N
!-- The following MUST be 64-bit REALs [i.e. REAL(8) on Fortran-90]
!#ifdef F90
!      real(8) real_v(n)
!#else
      REAL(KIND=8) :: REAL_V(N)
!#endif
      CHARACTER(LEN=*) :: CHAR_V
      INTEGER(KIND=JPIM) :: I

      DO I=1,N
         CHAR_V(I:I) = CHAR(INT(REAL_V(I)))
      ENDDO
      ENDSUBROUTINE RTOC
