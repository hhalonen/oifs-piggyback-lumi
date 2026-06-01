! Oct-2012 P. Marguinaud 64b LFI
      FUNCTION ISMIN_164  &
     &   ( KNELEM, PCHAMP) RESULT (IISMIN_1)
      USE LFI_PRECISION
      IMPLICIT NONE
 
!    Search for position of minimum value in array PCHAMP.
!    Simplified version (good for vector computers) assuming stride=1
!     Original  F. Vana - ONPP/CHMI - 23-Mar-2010

      INTEGER (KIND=JPLIKB) IISMIN_1
      INTEGER (KIND=JPLIKB) KNELEM
      REAL (KIND=JPDBLR) PCHAMP (*)

      IF (KNELEM <= 0) THEN
        IISMIN_1=0
      ELSE
        IISMIN_1=INT (MINLOC(ARRAY=PCHAMP(1:KNELEM) ,DIM=1), JPLIKB)
      ENDIF
      RETURN
      END FUNCTION ISMIN_164
