SUBROUTINE NOGWDRAG(KDIM, &
 ! Output quantities
 &  FLUX, PDIAG, AUXL )

!**** *NOGWDRAG* - Routine called when convection is bypassed

!     PURPOSE.
!     --------

!**   INTERFACE.
!     ----------

!        EXPLICIT ARGUMENTS :
!        --------------------
!     ==== INPUTS ===
! KDIM     : Derived variable for dimensions

!     ==== Input/Output ====
! FLUX         : Derived variable for fluxes
! LLKEYS       : Derived variable with keys
! PDIAG        : Derived variable for diagnostic quantities
! AUXL         : Derived variables for local auxiliary quantities

!        --------------------

!     METHOD.
!     -------
!        SEE DOCUMENTATION

!     EXTERNALS.
!     ----------

!     REFERENCE.
!     ----------
!        ECMWF RESEARCH DEPARTMENT DOCUMENTATION OF THE IFS

!     AUTHOR.
!     -------
!      Original : 2012-11-22  F. VANA (c) ECMWF

!     MODIFICATIONS.
!     --------------

!-----------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM ,   JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

USE YOMPHYDER ,ONLY : DIMENSION_TYPE, AUX_DIAG_TYPE, AUX_DIAG_LOCAL_TYPE, &
  &  FLUX_TYPE

!-----------------------------------------------------------------------

IMPLICIT NONE

TYPE (DIMENSION_TYPE)          , INTENT (IN)   :: KDIM
TYPE (FLUX_TYPE)               , INTENT(INOUT) :: FLUX
TYPE (AUX_DIAG_TYPE)           , INTENT(INOUT) :: PDIAG
TYPE (AUX_DIAG_LOCAL_TYPE)     , INTENT(INOUT) :: AUXL

!-----------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JK, JL

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('NOGWDRAG',0,ZHOOK_HANDLE)

!     ------------------------------------------------------------------

!*         1.     NECESSARY COMPUTATIONS IF GWDRAG IS BY-PASSSED

!* set gwd tendency coefficients, momentum flux, and dissipation to zero
DO JK=1,KDIM%KLEV
  DO JL=KDIM%KIDIA,KDIM%KFDIA
    AUXL%ZSOTEU(JL,JK)=0.0_JPRB
    AUXL%ZSOTEV(JL,JK)=0.0_JPRB
    AUXL%ZSOBETA(JL,JK)=0.0_JPRB
  ENDDO
ENDDO

DO JK=0,KDIM%KLEV
  DO JL=KDIM%KIDIA,KDIM%KFDIA
    FLUX%PSTRDU(JL,JK)=0.0_JPRB
    FLUX%PSTRDV(JL,JK)=0.0_JPRB    
  ENDDO
ENDDO

DO JL=KDIM%KIDIA,KDIM%KFDIA
  PDIAG%PUSTRG(JL)=0.0_JPRB
  PDIAG%PVSTRG(JL)=0.0_JPRB
  PDIAG%PVDISG(JL)=0.0_JPRB
ENDDO


!     ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('NOGWDRAG',1,ZHOOK_HANDLE)
END SUBROUTINE NOGWDRAG
