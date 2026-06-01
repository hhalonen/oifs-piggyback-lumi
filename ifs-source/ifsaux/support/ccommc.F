! Look for common elements in two sorted lists
      SUBROUTINE CCOMMC (KNC, KCOM1, KCOM2, KN1, CD1, KN2, CD2)
      USE PARKIND1, ONLY : JPIM
      IMPLICIT NONE
      
      INTEGER(KIND=JPIM) :: KNC, KN1, KN2
      INTEGER(KIND=JPIM) :: KCOM1 (KNC), KCOM2 (KNC)

      CHARACTER(LEN=*) :: CD1 (KN1), CD2 (KN2)
      INTEGER(KIND=JPIM) :: I1, I2, IC

      I1 = 1
      I2 = 1
      IC = 0

      DO WHILE ((I1 <= KN1) .AND. (I2 <= KN2))
        IF (CD1 (I1) == CD2 (I2)) THEN
          IC = IC + 1
          KCOM1 (IC) = I1
          KCOM2 (IC) = I2
          I1 = I1 + 1
          I2 = I2 + 1
        ELSEIF (CD1 (I1) > CD2 (I2)) THEN
          I2 = I2 + 1
        ELSEIF (CD1 (I1) < CD2 (I2)) THEN
          I1 = I1 + 1
        ENDIF
      ENDDO
 
      KNC = IC

      ENDSUBROUTINE CCOMMC

