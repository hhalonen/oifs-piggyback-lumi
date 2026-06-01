SUBROUTINE PRIDOCF(KDOCFI,KDATEF,CDNAMX,PECH,PECHDV,KULSOR)
USE YOMHOOK , ONLY : LHOOK, DR_HOOK, JPHOOK
! --------------------------------------------------------------------------
! **** *PRIDOCF* Impression du contenu de l'article de doc. DOCFICHIER.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! --------
! Externes: /
! ---------
! Original :
! ----------
! 92-03-01, J.M. Piriou.
!
! Modifications:
! --------------
! 92-06-05, J.M. Piriou: echeance en secondes lue sur article 'ECHEANCE
! 92-12-04, J.M. Piriou: adaptation au calcul sur CRAY.
!
! --------------------------------------------------------------------------
IMPLICIT NONE
INTEGER(KIND=4) :: ILNAMX
INTEGER(KIND=4) :: KULSOR
REAL(KIND=8) :: PECH
REAL(KIND=8) :: PECHDV
REAL(KIND=8) :: ZECH
REAL(KIND=8) :: ZECHDV

CHARACTER*(*) CDNAMX
INTEGER(KIND=4) KDOCFI(17)
INTEGER(KIND=4) KDATEF(11)
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('PRIDOCF',0,ZHOOK_HANDLE)
ILNAMX=LEN_TRIM(CDNAMX)
WRITE(KULSOR,'(3a,i4.4,4(a,i2.2),a,$)') &
& ' Run ',CDNAMX(1:ILNAMX),', base ',KDATEF(1) &
& ,'-',KDATEF(2),'-',KDATEF(3),' ',KDATEF(4),':',KDATEF(5),','
!
!-------------------------------------------------
! Echéance totale.
!-------------------------------------------------
!
IF(PECH < 3600.) THEN
        !
        ! On fournit l'échéance en minutes jusqu'à une heure.
        !
        ZECH=PECH/60.
        WRITE(KULSOR,'(a,f7.2,a,$)')' ech tot ',ZECH,' mn'
ELSEIF(PECH < 259200.) THEN
        !
        ! On fournit l'échéance en heures jusqu'à 3 jours.
        !
        ZECH=PECH/3600.
        WRITE(KULSOR,'(a,f7.2,a,$)')' ech tot ',ZECH,' h'
ELSE
        !
        ! On fournit l'échéance en jours au-delà.
        !
        ZECH=PECH/86400.
        WRITE(KULSOR,'(a,f7.2,a,$)')' ech tot ',ZECH,' j'
ENDIF
!
!-------------------------------------------------
! Echéance variables.
!-------------------------------------------------
!
IF(PECHDV /= PECH) THEN
        IF(PECHDV < 3600.) THEN
                !
                ! On fournit l'échéance en minutes jusqu'à une heure.
                !
                ZECHDV=PECHDV/60.
                WRITE(KULSOR,'(a,f7.2,a)')', ech var ',ZECHDV,' mn.'
        ELSEIF(PECHDV < 259200.) THEN
                !
                ! On fournit l'échéance en heures jusqu'à 3 jours.
                !
                ZECHDV=PECHDV/3600.
                WRITE(KULSOR,'(a,f7.2,a)')', ech var ',ZECHDV,' h.'
        ELSE
                !
                ! On fournit l'échéance en jours au-delà.
                !
                ZECHDV=PECHDV/86400.
                WRITE(KULSOR,'(a,f7.2,a)')', ech var ',ZECHDV,' j.'
        ENDIF
ELSE
        WRITE(KULSOR,'(a)') '.'
ENDIF
!
!-------------------------------------------------
! Type de domaines.
!-------------------------------------------------
!
IF(KDOCFI(1) == 1) THEN
        WRITE(KULSOR,'(a,$)')' DOMAINES LIMITES'
ELSEIF(KDOCFI(1) == 5) THEN
        WRITE(KULSOR,'(a,$)')' DOMAINE GLOBAL'
ELSEIF(KDOCFI(1) == 6) THEN
        WRITE(KULSOR,'(a,$)')' DOMAINES ZONAUX'
ELSE
        WRITE(KULSOR,'(a,$)')' PRIDOCF/ERREUR: type de domaine inconnu!...'
ENDIF
WRITE(KULSOR,'(i7,a,$)') KDOCFI(5),' pas de temps,'
WRITE(KULSOR,'(2(i3,a))') KDOCFI(6),' niveaux, ',KDOCFI(15),' domaines.'
IF (LHOOK) CALL DR_HOOK('PRIDOCF',1,ZHOOK_HANDLE)
END SUBROUTINE PRIDOCF
