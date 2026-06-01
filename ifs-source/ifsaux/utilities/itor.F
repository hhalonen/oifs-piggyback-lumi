      SUBROUTINE ITOR(INT_V, REAL_V, N)
!
!**** SUBROUTINE itor - To convert from INTEGER to 64-bit REAL
!
!        S.Saarinen     ECMWF     27/6/95
!
!**   INTERFACE
!     ---------
!
!        CALL itor(int_v, real_v, n)
!     WHERE:
!           int_v  = Integer vector (INPUT) of n elements:
!                    Only 32-bit range is accepted i.e.
!                    [-2147483648 ... 2147483647] which is the same as
!                    [-2**31 ... 2**31 - 1]
!           real_v = 64-bit REAL-vector (OUTPUT) of n elements
!                    each holding one integer value after conversion
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
         REAL_V(I) = MIN(MAX(-2147483647_JPIM,INT_V(I)),2147483647_JPIM)
      ENDDO

      RETURN
      ENDSUBROUTINE ITOR
