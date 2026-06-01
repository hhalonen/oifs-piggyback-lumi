SUBROUTINE RECPOL(PX,PY,PR,PANG)

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE


!     DUMMY REAL SCALARS
REAL(KIND=JPRB) :: PANG
REAL(KIND=JPRB) :: PR
REAL(KIND=JPRB) :: PX
REAL(KIND=JPRB) :: PY


!     LOCAL REAL SCALARS
REAL(KIND=JPRB) :: ZINT, ZPI

!     ---------------------------------------------------------------------------
!     **** *RECPOL *  Conversion rectangulaire > polaire.
!     ---------------------------------------------------------------------------
!     Sujet:
!     ------
!     Arguments explicites: /
!     ---------------------
!     Arguments implicites: /
!     ---------------------
!     Methode:
!     --------
!     Externes: /
!     ---------
!     Auteur:           92-10, J.M. Piriou.
!     -------
!     Modifications:
!     ---------------------------------------------------------------------------
!     En entree: px et py coordonnees rectangulaires.
!     En sortie: pr et pang rayon et azimuth.
!     ---------------------------------------------------------------------------
PR=SQRT(PX*PX+PY*PY)
ZPI=4._JPRB*ATAN(1._JPRB)
IF(PX == 0._JPRB) THEN
  IF(PY == 0._JPRB) THEN
    PANG=0._JPRB
  ELSEIF(PY > 0._JPRB) THEN
    PANG=ZPI/2._JPRB
  ELSE
    PANG=-ZPI/2._JPRB
  ENDIF
ELSE
  ZINT=ATAN(PY/PX)
  IF(PX >= 0._JPRB) THEN
    PANG=ZINT
  ELSEIF(PY >= 0._JPRB) THEN
    PANG=ZINT+ZPI
  ELSE
    PANG=ZINT-ZPI
  ENDIF
ENDIF
RETURN
END SUBROUTINE RECPOL
