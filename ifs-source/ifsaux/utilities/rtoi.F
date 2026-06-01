      SUBROUTINE RTOI(REAL_V, INT_V, N)
!
!**** SUBROUTINE rtoi - To convert from 64-bit REAL to INTEGER
!
!        S.Saarinen     ECMWF     27/6/95
!
!**   INTERFACE
!     ---------
!
!        CALL rtoi(real_v, int_v, n)
!     WHERE:
!           real_v = 64-bit REAL-vector (INPUT) of n elements
!                    each holding one integer value
!           int_v  = Integer vector (OUTPUT) of n elements:
!                    Only 32-bit range is accepted i.e.
!                    [-2147483648 ... 2147483647] which is the same as
!                    [-2**31 ... 2**31 - 1]
!           n      = length of the vectors
!
!
!-----------------------------------------------------------------------
!
      USE PARKIND1, ONLY : JPIM
      IMPLICIT NONE
      INTEGER(KIND=JPIM) :: N
!-- The following MUST be 64-bit REALs [i.e. REAL(8) on Fortran-90]
      REAL(KIND=8) :: REAL_V(N)
      INTEGER(KIND=JPIM) :: INT_V(N)
      INTEGER(KIND=JPIM) :: I

      DO I=1,N
!J       int_v(i) = min(max(-2147483647,int(real_v(i))),2147483647)
        INT_V(I)=INT(MIN(MAX(-2000000000.0_8,REAL_V(I)),2000000000.0_8))
      ENDDO

      RETURN
      ENDSUBROUTINE RTOI
