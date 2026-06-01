! Find unique integers in a sorted list
      SUBROUTINE GUNIQI4 (N,ORD,A)
      USE PARKIND1, ONLY : JPIM, JPRB
!      IMPLICIT INTEGER (A-Z)
      IMPLICIT NONE

      INTEGER(KIND=4) :: N
      INTEGER(KIND=4) :: A(N)
      INTEGER(KIND=4) :: ORD(N)
      INTEGER(KIND=4) :: I, J

      IF (N == 0) RETURN

      J = 1
      ORD (1) = 1

      DO I = 2, N
        IF (A (I-1) /= A (I)) THEN
          J = J + 1
          ORD (J) = I
        ENDIF
      ENDDO

      N = J

      ENDSUBROUTINE GUNIQI4
