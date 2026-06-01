      FUNCTION ISRCHEQ(N,ARRAY,INC,TARGET)
!     cray function isrcheq
!     description see manual SR-2081
!
      USE PARKIND1, ONLY : JPIM
      IMPLICIT NONE
      INTEGER(KIND=JPIM) :: ISRCHEQ
      INTEGER(KIND=JPIM) :: N
      INTEGER(KIND=JPIM) :: INC
      INTEGER(KIND=JPIM) :: ARRAY(1+INC*(N-1))
      INTEGER(KIND=JPIM) :: TARGET
      INTEGER :: I, J
!
      J=1
      ISRCHEQ=0
      IF(N.LE.0) RETURN
      IF(INC.LT.0) J=1-(N-1)*INC
      DO 100 I=1,N
           IF(ARRAY(J).EQ.TARGET) GOTO 200
           J=J+INC
 100  CONTINUE
 200  ISRCHEQ=I
      RETURN
      ENDFUNCTION ISRCHEQ
