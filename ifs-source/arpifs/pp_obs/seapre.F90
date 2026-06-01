SUBROUTINE SEAPRE(PARA,KPARA,PSTPRE,KLEV)

!**** *SEAPRE*   - Search corresponding level to input pressure

!     Purpose.
!     --------
!           Serach corresponding level of the model to an input pressure
!           through standard atmosphere.

!**   Interface.
!     ----------
!        *CALL* *SEAPRE(PARA,KPARA,PSTPRE,KLEV)

!        Explicit arguments :
!        --------------------
!        PARA  : Pressure                           (input)
!        KPARA  : Level                              (output)
!        PSTPRE : Standard atmosphere                (input)
!        KLEV   : Number of level of the model       (input)

!        Implicit arguments :
!        --------------------
!        none

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------

!     Reference.
!     ----------
!        Documentation ARPEGE

!     Author.
!     -------
!        A. Lasserre-Bigorry

!     Modifications.
!     --------------
!        Original : 91-06-10
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PARA 
INTEGER(KIND=JPIM),INTENT(OUT)   :: KPARA 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSTPRE(KLEV) 
INTEGER(KIND=JPIM) :: JLEV
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!*      1.  SEARCH OF CORRESPONDING LEVEL
!           -----------------------------
IF (LHOOK) CALL DR_HOOK('SEAPRE',0,ZHOOK_HANDLE)
KPARA=KLEV
DO JLEV=KLEV,1,-1
  IF(PARA <= PSTPRE(JLEV)) KPARA=JLEV
ENDDO

IF (LHOOK) CALL DR_HOOK('SEAPRE',1,ZHOOK_HANDLE)
END SUBROUTINE SEAPRE

