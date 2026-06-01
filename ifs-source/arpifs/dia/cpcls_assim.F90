SUBROUTINE CPCLS_ASSIM(YDGEOMETRY,YDSURF,KST,KND,PSP_CL, &
 & PXUCLS, PXVCLS, PXNUCLS, PXNVCLS, PXTCLS, PXRHCLS )

!**** *CPCLS_ASSIM* - INTERFACE FOR CLS FIELDS

!     Purpose.
!     --------
!           DIAGNOSTICS OF PHYSICAL FLUXES IN CLS ARRAYS

!**   Interface.
!     ----------
!        *CALL* *CPCLS_ASSIM*

!        Explicit arguments :
!        --------------------

!       NPROMA                 - HORIZONTAL DIMENSION                 (INPUT)
!       KST to KND             - NB OF POINTS                         (INPUT)
!       FLUXES COMING FROM THE PHYSICAL PARAMETERIZATIONS             (INPUT)

!        --------------------

!     Method.
!     -------

!     Externals.
!     ----------

!     Reference.
!     ----------

!     Author.
!     -------
!      F. Taillefer
!      Original : 06/2016  from cpxfu

!     Modifications.
!     --------------
!     ------------------------------------------------------------------

USE GEOMETRY_MOD , ONLY : GEOMETRY
USE SURFACE_FIELDS_MIX , ONLY : TSURF
USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(GEOMETRY), INTENT(IN) :: YDGEOMETRY
TYPE(TSURF), INTENT(IN) :: YDSURF
INTEGER(KIND=JPIM),INTENT(IN)    :: KST 
INTEGER(KIND=JPIM),INTENT(IN)    :: KND 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSP_CL(YDGEOMETRY%YRDIM%NPROMA,YDSURF%YSP_CLD%NDIM)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PXUCLS(YDGEOMETRY%YRDIM%NPROMA) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PXVCLS(YDGEOMETRY%YRDIM%NPROMA) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PXNUCLS(YDGEOMETRY%YRDIM%NPROMA) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PXNVCLS(YDGEOMETRY%YRDIM%NPROMA) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PXTCLS(YDGEOMETRY%YRDIM%NPROMA) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PXRHCLS(YDGEOMETRY%YRDIM%NPROMA) 

!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JROF
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('CPCLS_ASSIM',0,ZHOOK_HANDLE)
ASSOCIATE(YSP_CL=>YDSURF%YSP_CL)
!     ------------------------------------------------------------------

DO JROF = KST,KND
  PSP_CL(JROF,YSP_CL%YUCLS%MP1)=PXUCLS(JROF)
  PSP_CL(JROF,YSP_CL%YVCLS%MP1)=PXVCLS(JROF)
  PSP_CL(JROF,YSP_CL%YNUCLS%MP1)=PXNUCLS(JROF)
  PSP_CL(JROF,YSP_CL%YNVCLS%MP1)=PXNVCLS(JROF)
  PSP_CL(JROF,YSP_CL%YTCLS%MP1)=PXTCLS(JROF)
  PSP_CL(JROF,YSP_CL%YHUCLS%MP1)=MAX(0.0_JPRB,MIN(1.0_JPRB,PXRHCLS(JROF)))
ENDDO

!     ------------------------------------------------------------------

END ASSOCIATE

IF (LHOOK) CALL DR_HOOK('CPCLS_ASSIM',1,ZHOOK_HANDLE)

END SUBROUTINE CPCLS_ASSIM
