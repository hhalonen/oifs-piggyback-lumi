SUBROUTINE FILLZERO(CDENT,CDSOR)
USE YOMHOOK , ONLY : LHOOK, DR_HOOK, JPHOOK
! --------------------------------------------------------------
! **** *fillzero* Remplacement des blancs de gauche par des zéros.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   1999-11, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! Exemples:
! cdent='   -233.07'  ==> cdsor='-000233.07'
! cdent='    233.07'  ==> cdsor='0000233.07'
! cdent='    856   '  ==> cdsor='0000856   '
! cdent='   -856   '  ==> cdsor='-000856   '
! --------------------------------------------------------------
IMPLICIT NONE
INTEGER(KIND=4) :: JCAR
CHARACTER*(*) CDENT,CDSOR
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FILLZERO',0,ZHOOK_HANDLE)
CDSOR=' '
DO JCAR=1,LEN_TRIM(CDENT)
        IF(CDENT(JCAR:JCAR) == ' ') THEN
                CDSOR(JCAR:JCAR)='0'
        ELSEIF(CDENT(JCAR:JCAR) == '-') THEN
                IF(CDENT(JCAR-1:JCAR-1) == ' ') THEN
                        CDSOR(JCAR:JCAR)='0'
                        CDSOR(1:1)='-'
                ELSE
                        CDSOR(JCAR:JCAR)=CDENT(JCAR:JCAR)
                ENDIF
        ELSE
                CDSOR(JCAR:JCAR)=CDENT(JCAR:JCAR)
        ENDIF
ENDDO
IF (LHOOK) CALL DR_HOOK('FILLZERO',1,ZHOOK_HANDLE)
END SUBROUTINE FILLZERO
