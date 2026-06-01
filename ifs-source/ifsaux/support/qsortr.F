! Sort character strings
      SUBROUTINE QSORTR (N, ORD, A)
      USE PARKIND1, ONLY : JPRB, JPRD
      IMPLICIT INTEGER (A-Z)

      REAL (KIND=JPRB) :: A (N)
      DIMENSION ORD(N)

      IF (JPRD == JPRB) THEN
        CALL QSORTR8 (N, ORD, A)
      ELSE
        CALL QSORTR4 (N, ORD, A)
      ENDIF

      END SUBROUTINE
