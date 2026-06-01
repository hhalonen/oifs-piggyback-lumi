      SUBROUTINE GATHER(N,A,B,IND)
!
      USE PARKIND1, ONLY : JPIM, JPRB
!
      IMPLICIT NONE
!
      INTEGER(KIND=JPIM) :: N
      INTEGER(KIND=JPIM) :: IND(N)
      INTEGER(KIND=JPIM) :: I
      REAL(KIND=JPRB) :: A(N)
      REAL(KIND=JPRB) :: B(*)
!
!  GATHER - Gathers a vector from a source vector
!
      DO 100 I=1,N
      A(I)=B(IND(I))
 100  CONTINUE
!
      ENDSUBROUTINE GATHER
