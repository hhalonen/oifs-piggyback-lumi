SUBROUTINE ETALAE(KUL,CDCHA,KDCHA,KNCHA)
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK, JPHOOK
! --------------------------------------------------------------------------
! **** *ETALAE*  Etablissement de la liste des articles du LFI d'entrée.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:  J.M.Piriou
! -------
! Modifications.
! --------------
! Original : 96-01
! --------------------------------------------------------------------------
! En entree:
! fichier LFI
! En sortie:
! kncha contient le nombre d'articles de ce fichier.
! cdcha(1, ..., kncha) contient le nom de ces articles.
! --------------------------------------------------------------------------
IMPLICIT NONE
INTEGER(KIND=4) KDCHA,KUL,KNCHA,IREP,IERR,ILONG,IPOSEX
CHARACTER*16 CLNOMA
CHARACTER*(*) CDCHA(KDCHA)
!
! On positionne le pointeur en début de fichier.
!
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('ETALAE',0,ZHOOK_HANDLE)
CALL LFIPOS(IREP,KUL)
KNCHA=0
IERR=0
  100 CONTINUE
!
! Lecture de l'article suivant.
!
CALL LFICAS(IREP,KUL,CLNOMA,ILONG,IPOSEX,.TRUE.)
IF(ILONG == 0) GOTO 200 ! on est en fin de fichier.
KNCHA=KNCHA+1
IF(KNCHA > KDCHA) THEN
        IERR=1
ELSE
        CDCHA(KNCHA)=CLNOMA
ENDIF
GOTO 100
  200 CONTINUE
IF(IERR == 1) THEN
        WRITE(*,*) 'ERREUR etalae: kdcha plus petit'
        WRITE(*,*) 'que le nombre de champs du fichier!...'
        WRITE(*,*) KNCHA, KDCHA
        STOP 'call abort'
ENDIF
IF (LHOOK) CALL DR_HOOK('ETALAE',1,ZHOOK_HANDLE)
END SUBROUTINE ETALAE
