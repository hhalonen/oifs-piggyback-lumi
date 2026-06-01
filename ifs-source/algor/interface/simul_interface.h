INTERFACE
SUBROUTINE SIMUL_INTERFACE(YDGEOMETRY,YDFIELDS,YDMTRAJ,YDMODEL,KINDIC,KDPROB,YDVAZX,PZF,&
  &              YDVAZG,KITER,YDJOT,YDVARBC,YDTCV5,YDGOM,YDTCV,YDTCV_BGC,YDGOM5,YDODB)

!**** *SIM4D*  - Controls 4-D variational job at level 2.

!     Purpose.
!     --------
!           Simulator (in minimization language). Compute the cost-function
!        and its gradient by calling the model and its adjoint.

!     Interface.
!     ----------
!        *CALL* *SIM4D(...)

!        Explicit arguments :
!        --------------------
!        KINDIC : not used (for interface with M1QN3)
!        KDPROB : not used (for interface with M1QN3)
!        YDVAZX  : STATE VARIABLE
!        PZF    : COST FUNCTION
!        YDVAZG  : GRADIENT
!        KITER  : index of iteration given by the caller
!     ----------------------------------------------------------------------

USE TYPE_MODEL          , ONLY : MODEL
USE GEOMETRY_MOD        , ONLY : GEOMETRY
USE FIELDS_MOD          , ONLY : FIELDS
USE MTRAJ_MOD           , ONLY : MTRAJ
USE PARKIND1            , ONLY : JPIM, JPRB
USE JO_TABLE_MOD        , ONLY : JO_TABLE
USE VARBC_CLASS         , ONLY : CLASS_VARBC
USE TOVSCV_BGC_MOD      , ONLY : TOVSCV_BGC
USE TOVSCV_MOD          , ONLY : TOVSCV
USE SUPERGOM_CLASS      , ONLY : CLASS_SUPERGOM
USE DBASE_MOD           , ONLY : DBASE
USE CONTROL_VECTORS_MOD , ONLY : CONTROL_VECTOR

!     ----------------------------------------------------------------------

IMPLICIT NONE

TYPE(GEOMETRY)      ,INTENT(INOUT) :: YDGEOMETRY !! INOUT needed for call to CNT
TYPE(FIELDS)        ,INTENT(INOUT) :: YDFIELDS
TYPE(MTRAJ)         ,INTENT(INOUT) :: YDMTRAJ
TYPE(MODEL)         ,INTENT(INOUT) :: YDMODEL
TYPE(CONTROL_VECTOR),INTENT(INOUT) :: YDVAZX
REAL(KIND=JPRB)     ,INTENT(INOUT) :: PZF
TYPE(CONTROL_VECTOR),INTENT(INOUT) :: YDVAZG
INTEGER(KIND=JPIM)  ,INTENT(IN)    :: KITER
TYPE(JO_TABLE)      ,INTENT(INOUT) :: YDJOT
TYPE(CLASS_VARBC)   ,INTENT(INOUT) :: YDVARBC
TYPE(TOVSCV)        ,INTENT(IN)   , OPTIONAL :: YDTCV5
TYPE(CLASS_SUPERGOM),INTENT(INOUT), OPTIONAL :: YDGOM
TYPE(TOVSCV)        ,INTENT(INOUT), OPTIONAL :: YDTCV
TYPE(CLASS_SUPERGOM),INTENT(INOUT), OPTIONAL :: YDGOM5
CLASS(DBASE)        ,INTENT(INOUT), OPTIONAL :: YDODB
TYPE(TOVSCV_BGC)    ,INTENT(IN)   , OPTIONAL :: YDTCV_BGC
INTEGER(KIND=JPIM)                 :: KINDIC ! Argument NOT used
INTEGER(KIND=JPIM)                 :: KDPROB ! Argument NOT used

!     ------------------------------------------------------------------

END SUBROUTINE
END INTERFACE
