SUBROUTINE EVOLVE_SPP( YDCONF, YDSPP )

! Purpose :
! -------
!    Evolve patterns used in the SPP scheme, which represents model uncertainties
!    with stochastically perturbed parameterisations.
!

! Interface :
! ---------
!    Empty.

! External :
! --------
!    None.

! Method :
! ------
!    See Documentation.

! Reference :
! ---------

! Author :
! ------
!    M. Leutbecher (ECMWF)
!    Original : October 2020

! Modifications :
! -------------
!
!
!-----------------------------------------------------------------------------
USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE SPECTRAL_ARP_MOD , ONLY : EVOLVE_ARP
USE SPP_MOD  , ONLY : TSPP_CONFIG, TSPP_DATA
!     ------------------------------------------------------------------
IMPLICIT NONE

TYPE(TSPP_CONFIG), INTENT(IN)     :: YDCONF
TYPE(TSPP_DATA)  , INTENT(INOUT)  :: YDSPP
!     ------------------------------------------------------------------
INTEGER(KIND=JPIM) :: JPERT, JRF, IRF
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('EVOLVE_SPP',0,ZHOOK_HANDLE)

DO JPERT=1, YDCONF%SM%NACT
  !
  !   loop over perturbations
  !
  IRF  = YDCONF%SM%PN(JPERT)%MP - 1 ! points before random field for this perturbations
  DO JRF=1, YDCONF%SM%PN(JPERT)%NRF
    IRF = IRF + 1
    !
    !   loop over random fields for jpert-th perturbation
    !
    CALL EVOLVE_ARP(   YDSPP%SP_ARP(IRF) )
  ENDDO
ENDDO

IF (LHOOK) CALL DR_HOOK('EVOLVE_SPP',1,ZHOOK_HANDLE)

END SUBROUTINE EVOLVE_SPP
