SUBROUTINE INTCAR(KENT,CDCAR,KCAR)
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK, JPHOOK
! --------------------------------------------------------------------------
! **** *INTCAR*  - Conversion entier > chaine de caractere
! --------------------------------------------------------------------------
! Auteur:  J.M.Piriou
! -------
! Modifications.
! --------------
! Original : 95-03
! --------------------------------------------------------------------------
! En entree:
! kent entier a convertir
! En sortie:
! cdcar chaine recevant l'entier
! kcar: nombre de caracteres ecrits sur cdcar
! --------------------------------------------------------------------------
IMPLICIT NONE
INTEGER(KIND=4) :: IENT
INTEGER(KIND=4) :: IMOD
INTEGER(KIND=4) :: ISIGN
INTEGER(KIND=4) :: JC
INTEGER(KIND=4) :: KCAR
INTEGER(KIND=4) :: KENT

CHARACTER *(*) CDCAR
CHARACTER*1 CLINT
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('INTCAR',0,ZHOOK_HANDLE)
CDCAR=' '
IENT=KENT
KCAR=0
!
! --------------------------------------------------------------------------
!
! Traitement du signe.
IF(IENT.LT.0) THEN
        ISIGN=-1
        IENT=-IENT
ELSE
        ISIGN=1
ENDIF
!
! --------------------------------------------------------------------------
!
! Conversion entier vers chaine de caracteres.
  100 CONTINUE
IMOD=MOD(IENT,10)
KCAR=KCAR+1
WRITE(CDCAR(KCAR:KCAR),FMT='(i1.1)') IMOD
IENT=IENT/10
IF(IENT.NE.0) GOTO 100
!
! --------------------------------------------------------------------------
!
! A ce stade, cdcar(1:kcar) contient l'entier cherche,
! mais ecrit en sens inverse.
! On va operer une symetrie centrale sur cdcar.
DO JC=1,KCAR/2
        CLINT=CDCAR(JC:JC)
        CDCAR(JC:JC)=CDCAR(KCAR-JC+1:KCAR-JC+1)
        CDCAR(KCAR-JC+1:KCAR-JC+1)=CLINT
ENDDO
!
! --------------------------------------------------------------------------
!
! Traitement du signe: ajout du "-" si necessaire.
IF(ISIGN.EQ.-1) THEN
        CDCAR='-'//CDCAR(1:KCAR)
        KCAR=KCAR+1
ENDIF
IF (LHOOK) CALL DR_HOOK('INTCAR',1,ZHOOK_HANDLE)
END SUBROUTINE INTCAR
