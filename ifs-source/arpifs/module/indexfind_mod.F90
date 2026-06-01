MODULE INDEXFIND_MOD

!   Purpose
!   -------
!     Find position of value in array. Like Fortran functions
!     MINLOC and MAXLOC, INTLOC and REALOC assume lower bound
!     index is 1.

!   Author
!   ------
!     Yannick Tremolet

!   Modifications
!   -------------
!     Original    23-Aug-04
! ------------------------------------------------------------------

USE PARKIND1, ONLY: JPIM, JPRB
USE YOMHOOK , ONLY: LHOOK, DR_HOOK, JPHOOK

IMPLICIT NONE
PRIVATE
PUBLIC INDXFIND

INTERFACE INDXFIND
MODULE PROCEDURE INDINLOC, INDRELOC
END INTERFACE

! ------------------------------------------------------------------
CONTAINS
! ------------------------------------------------------------------

FUNCTION INDINLOC(KARRAY,KVAL)

INTEGER(KIND=JPIM) :: INDINLOC
INTEGER(KIND=JPIM), INTENT(IN) :: KARRAY(:),KVAL
INTEGER(KIND=JPIM) :: II,JJ
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('INDEXFIND_MOD:INDINLOC',0,ZHOOK_HANDLE)
JJ=0
II=1
DO WHILE (II<=SIZE(KARRAY) .AND. JJ==0)
  IF (KARRAY(II)==KVAL) JJ=II
  II=II+1
ENDDO
INDINLOC=JJ
IF (LHOOK) CALL DR_HOOK('INDEXFIND_MOD:INDINLOC',1,ZHOOK_HANDLE)
END FUNCTION INDINLOC

! ------------------------------------------------------------------

FUNCTION INDRELOC(PARRAY,PVAL)

INTEGER(KIND=JPIM) :: INDRELOC
REAL(KIND=JPRB), INTENT(IN) :: PARRAY(:),PVAL
INTEGER(KIND=JPIM) :: II,JJ
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('INDEXFIND_MOD:INDRELOC',0,ZHOOK_HANDLE)
JJ=0
II=1
DO WHILE (II<=SIZE(PARRAY) .AND. JJ==0)
  IF (PARRAY(II)==PVAL) JJ=II
  II=II+1
ENDDO
INDRELOC=JJ
IF (LHOOK) CALL DR_HOOK('INDEXFIND_MOD:INDRELOC',1,ZHOOK_HANDLE)
END FUNCTION INDRELOC

! ------------------------------------------------------------------

END MODULE INDEXFIND_MOD
