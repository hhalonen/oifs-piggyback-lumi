SUBROUTINE CASC(CDOP,KCRIM,CDMOT,KMOT)
USE YOMHOOK , ONLY : LHOOK, DR_HOOK, JPHOOK
! --------------------------------------------------------------------------
! **** *CASC*  - Cassure d'une chaine en ses differents mots.
! --------------------------------------------------------------------------
! Auteur:  J.M.Piriou
! -------
! Modifications.
! --------------
! Original : 93-06
! --------------------------------------------------------------------------
! En entree: cdop chaine a casser.
! kcrim critere de mot:
! 1 si mot=suite continue de "non (blancs ou tabulations)".
! 2 si mot=suite continue de "a-z,A-Z,0-9,.,_".
! 3 si mot=suite continue de "0-9,E,e,+,-,.".
! 4 si mot=suite continue de "a-z,A-Z,0-9".
! 5 si mot=suite continue de "a-z,A-Z,0-9,_,-".
! 6 si mot=suite continue de "non (- ou . ou blancs ou tabulations)".
! 7 si mot=suite continue de "non .".
! 8 si mot=suite continue de "0-9".
! 9 si mot=suite continue de "non /".
! 10 si mot=suite continue de "non ,".
! < 0 si mot=suite continue de "non CHAR(-kcrim)".
!
! En sortie: kmot nombre de mots trouves sur cdop.
! cdmot tableau contenant ces mots.
! --------------------------------------------------------------------------
IMPLICIT NONE
INTEGER(KIND=4) :: IASC
INTEGER(KIND=4) :: IC
INTEGER(KIND=4) :: IMOT
INTEGER(KIND=4) :: IPRESMOT
INTEGER(KIND=4) :: JC
INTEGER(KIND=4) :: KCRIM
INTEGER(KIND=4) :: KMOT

CHARACTER*(*) CDOP
CHARACTER*(*) CDMOT(180)
CHARACTER*01 CL1
LOGICAL LLLET
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('CASC',0,ZHOOK_HANDLE)
KMOT=0
IPRESMOT=0
IC=LEN(CDOP)
DO JC=1,IC
        CL1=CDOP(JC:JC)
        IASC=ICHAR(CL1)
        !
        ! lllet: vrai si le caractère est
        ! une lettre ASCII accentuée.
        !
        LLLET=.FALSE.
        IF(IASC.GE.192.AND.IASC.LE.198) THEN
                LLLET=.TRUE.
        ELSEIF(IASC.EQ.199) THEN
                LLLET=.TRUE.
        ELSEIF(IASC.GE.200.AND.IASC.LE.203) THEN
                LLLET=.TRUE.
        ELSEIF(IASC.GE.204.AND.IASC.LE.207) THEN
                LLLET=.TRUE.
        ELSEIF(IASC.GE.210.AND.IASC.LE.214) THEN
                LLLET=.TRUE.
        ELSEIF(IASC.GE.217.AND.IASC.LE.220) THEN
                LLLET=.TRUE.
        ELSEIF(IASC.GE.221.AND.IASC.LE.221) THEN
                LLLET=.TRUE.
        ELSEIF(IASC.GE.224.AND.IASC.LE.229) THEN
                LLLET=.TRUE.
        ELSEIF(IASC.GE.231.AND.IASC.LE.231) THEN
                LLLET=.TRUE.
        ELSEIF(IASC.GE.232.AND.IASC.LE.235) THEN
                LLLET=.TRUE.
        ELSEIF(IASC.GE.236.AND.IASC.LE.239) THEN
                LLLET=.TRUE.
        ELSEIF(IASC.GE.242.AND.IASC.LE.246) THEN
                LLLET=.TRUE.
        ELSEIF(IASC.GE.249.AND.IASC.LE.252) THEN
                LLLET=.TRUE.
        ENDIF
        IF(KCRIM.EQ.1) THEN
                IF(CL1.NE.' '.AND.CL1.NE.'      ') THEN
                        IMOT=1
                ELSE
                        IMOT=0
                ENDIF
        ELSEIF(KCRIM.EQ.2) THEN
                IF((CL1.GE.'a'.AND.CL1.LE.'z') &
&               .OR.(CL1.GE.'A'.AND.CL1.LE.'Z') &
&               .OR.(CL1.GE.'0'.AND.CL1.LE.'9') &
&               .OR.CL1.EQ.'_' &
&               .OR.CL1.EQ.'.'.OR.LLLET) THEN
                        IMOT=1
                ELSE
                        IMOT=0
                ENDIF
        ELSEIF(KCRIM.EQ.3) THEN
                IF(CL1.EQ.'E'.OR.CL1.EQ.'e') THEN
                        IF(JC.EQ.1.OR.JC.EQ.IC) THEN
                                !
                                ! On est en présence d'un "E" en début
                                ! ou en fin de ligne. Ce mot n'est pas un
                                ! nombre valide.
                                !
                                IMOT=0
                        ELSE
                                !
                                ! On est en présence d'un "E"
                                ! à l'intérieur de la ligne.
                                ! Ce "E" sera celui d'une exponentiation
                                ! ssi le caractère qui le précède est
                                ! un chiffre et que le suivant est un chiffre
                                ! ou un signe.
                                !
                                IF(((CDOP(JC-1:JC-1).GE.'0' &
&                               .AND.CDOP(JC-1:JC-1).LE.'9').OR.CDOP(JC-1:JC-1).EQ.'.') &
&                               .AND.((CDOP(JC+1:JC+1).EQ.'+' &
&                               .OR.CDOP(JC+1:JC+1).EQ.'-'.OR.(CDOP(JC+1:JC+1).GE.'0' &
&                               .AND.CDOP(JC+1:JC+1).LE.'9')))) THEN
                                        IMOT=1
                                ELSE
                                        IMOT=0
                                ENDIF
                        ENDIF
                ELSEIF(CL1.EQ.'+'.OR.CL1.EQ.'-') THEN
                        IF(JC.EQ.1) THEN
                                !
                                ! On est en présence d'un signe en début
                                ! de ligne.
                                !
                                IMOT=0
                                IF(IC.EQ.1) THEN
                                        !
                                        ! La ligne a un seul caractère, ce signe!...
                                        !
                                        IMOT=0
                                ELSE
                                        !
                                        ! Le caractère suivant doit être
                                        ! un chiffre ou séparateur décimal.
                                        !
                                        IF((CDOP(2:2).GE.'0'.AND.CDOP(2:2).LE.'9') &
&                                       .OR.CDOP(2:2).EQ.'.') THEN
                                                IMOT=1
                                        ELSE
                                                IMOT=0
                                        ENDIF
                                ENDIF
                        ELSEIF(JC.EQ.IC) THEN
                                !
                                ! On est en présence d'un signe en fin
                                ! de ligne.
                                !
                                IMOT=0
                        ELSE
                                !
                                ! On est en présence d'un signe à l'intérieur de la ligne.
                                ! Le caractère suivant doit être
                                ! un chiffre ou séparateur décimal,
                                ! et le précédent doit être un "e" ou "E" ou un non chiffre non point
                                ! (ce afin de séparer les cas d'entrée incorrectes du type 3.63E00-3.7E07,
                                ! qui conduirait au deux mots 3.63E00 et 3.7E07;
                                ! la sortie de mots sera en ce dernier cas fausse, mais l'entrée
                                ! de l'utilisateur l'était également!...).
                                !
                                IF(((CDOP(JC+1:JC+1).GE.'0'.AND.CDOP(JC+1:JC+1).LE.'9') &
&                               .OR.CDOP(JC+1:JC+1).EQ.'.') &
&                               .AND.(CDOP(JC-1:JC-1).NE.'.'.AND. &
&                               (CDOP(JC-1:JC-1).LT.'0'.OR.CDOP(JC-1:JC-1).GT.'9'))) THEN
                                        IMOT=1
                                ELSE
                                        IMOT=0
                                ENDIF
                        ENDIF
                ELSEIF(CL1.GE.'0'.AND.CL1.LE.'9') THEN
                        IMOT=1
                ELSEIF(CL1.EQ.'.') THEN
                        IF(JC.EQ.1) THEN
                                !
                                ! On est en présence d'un point en début
                                ! de ligne.
                                ! Le caractère suivant doit être
                ! un chiffre.
                                !
                                IF(CDOP(2:2).GE.'0'.AND.CDOP(2:2).LE.'9') THEN
                                        IMOT=1
                                ELSE
                                        IMOT=0
                                ENDIF
                        ELSEIF(JC.EQ.IC) THEN
                                !
                                ! On est en présence d'un point en fin
                                ! de ligne.
                                ! Le caractère précédent doit être
                                ! un chiffre.
                                !
                                IF(CDOP(JC-1:JC-1).GE.'0'.AND.CDOP(JC-1:JC-1).LE.'9') THEN
                    IMOT=1
                ELSE
                    IMOT=0
                ENDIF
                        ELSE
                                !
                                ! On est en présence d'un point à l'intérieur de la ligne.
                                ! Le caractère suivant ou précédent doit être
                                ! un chiffre.
                                !
                                IF((CDOP(JC+1:JC+1).GE.'0'.AND.CDOP(JC+1:JC+1).LE.'9') &
&                               .OR.(CDOP(JC-1:JC-1).GE.'0'.AND.CDOP(JC-1:JC-1).LE.'9')) THEN
                    IMOT=1
                ELSE
                    IMOT=0
                ENDIF
                        ENDIF
                ELSE
                        IMOT=0
                ENDIF
        ELSEIF(KCRIM.EQ.4) THEN
                IF((CL1.GE.'a'.AND.CL1.LE.'z') &
&               .OR.(CL1.GE.'A'.AND.CL1.LE.'Z') &
&               .OR.(CL1.GE.'0'.AND.CL1.LE.'9') &
&               .OR.LLLET) THEN
                        IMOT=1
                ELSE
                        IMOT=0
                ENDIF
        ELSEIF(KCRIM.EQ.5) THEN
                IF((CL1.GE.'a'.AND.CL1.LE.'z') &
&               .OR.(CL1.GE.'A'.AND.CL1.LE.'Z') &
&               .OR.(CL1.GE.'0'.AND.CL1.LE.'9') &
&               .OR.CL1.EQ.'_' &
&               .OR.CL1.EQ.'-' &
&               .OR.LLLET) THEN
                        IMOT=1
                ELSE
                        IMOT=0
                ENDIF
        ELSEIF(KCRIM.EQ.6) THEN
                IF(CL1.NE.' '.AND.CL1.NE.'      '.AND.CL1.NE.'-'.AND.CL1.NE.'.') THEN
                        IMOT=1
                ELSE
                        IMOT=0
                ENDIF
        ELSEIF(KCRIM.EQ.7) THEN
                IF(CL1.NE.'.') THEN
                        IMOT=1
                ELSE
                        IMOT=0
                ENDIF
        ELSEIF(KCRIM.EQ.8) THEN
                IF(CL1.GE.'0'.AND.CL1.LE.'9') THEN
                        IMOT=1
                ELSE
                        IMOT=0
                ENDIF
        ELSEIF(KCRIM.EQ.9) THEN
                IF(CL1.NE.'/') THEN
                        IMOT=1
                ELSE
                        IMOT=0
                ENDIF
        ELSEIF(KCRIM.EQ.10) THEN
                IF(CL1.NE.',') THEN
                        IMOT=1
                ELSE
                        IMOT=0
                ENDIF
        ELSEIF(KCRIM.LT.0) THEN
                IF(CL1.NE.CHAR(-KCRIM)) THEN
                        IMOT=1
                ELSE
                        IMOT=0
                ENDIF
        ELSE
                PRINT*,'CASC/ERREUR: option kcrim=',KCRIM,' non reconnue!...'
                STOP 'call abort'
        ENDIF
        IF(IMOT.EQ.0.AND.IPRESMOT.EQ.0) THEN
                !
                ! Le caractère courant est hors d'un mot, le précédent aussi.
                !
        ELSEIF(IMOT.EQ.0) THEN
                !
                ! Le caractère courant est hors d'un mot, le précédent était dedans.
                !
                KMOT=KMOT+1
                CDMOT(KMOT)=CDOP(IPRESMOT:JC-1)
                IPRESMOT=0
        ELSEIF(IPRESMOT.EQ.0) THEN
                !
                ! Le caractère courant est dans un mot, le précédent était en dehors.
                !
                IPRESMOT=JC
        ENDIF
        IF(JC.EQ.IC.AND.IMOT.NE.0) THEN
                !
                ! Le dernier caractère de la ligne est dans un mot.
                !
                KMOT=KMOT+1
                CDMOT(KMOT)=CDOP(IPRESMOT:JC)
        ENDIF
ENDDO
IF (LHOOK) CALL DR_HOOK('CASC',1,ZHOOK_HANDLE)
END SUBROUTINE CASC
