SUBROUTINE DDR_LAMINAR_RES(KIDIA, KFDIA, KLON, PUSTAR, PXDIMO, PWRB)

!!****  *SUBROUTINE COMP_LAMINAR_RES * - subroutine to compute laminar resistances for wesely scheme
!!       
!!
!!    INTERFACE.
!!    ----------
!!    Called from depvel_wsl.F90
!!  
!!    PURPOSE
!!    -------
!!    The purpose of this routine is to compute laminar resistances.
!!   
!!    METHOD
!!    ------
!!
!! INPUTS:
!! -------
!! PUSTAR    : U*
!! PXDIMO     : Molecular diffusivities
!!
!! OUTPUTS:
!! -------
!! PWRB      : Laminar resistance
!!
!! LOCAL:
!! -------
!!
!!    AUTHOR
!!    ------
!!    M. Michou 
!!
!!
!!    MODIFICATIONS
!!    -------------
!!    Original   July 1999
!!    J Flemming 23.4.2018   Adapted for IFS 
!!
!!-------------------------------------------------------------------------------
  

USE PARKIND1 ,ONLY : JPIM, JPRB
USE DRYDEP_PAR   ,ONLY :PPRMAX, PPKARMAN, PPINVI, PPDIMOH2O, PPRAND
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

IMPLICIT NONE

INTEGER (KIND=JPIM), INTENT(IN) :: KIDIA, KFDIA, KLON 
REAL (KIND=JPRB), INTENT(IN)    :: PXDIMO
REAL (KIND=JPRB), DIMENSION(KLON), INTENT(IN)  :: PUSTAR
REAL (KIND=JPRB), DIMENSION(KLON), INTENT(OUT) :: PWRB

!! Local variables		 	   	       
INTEGER (KIND=JPIM) :: jl  ! Loop controls
REAL(KIND=JPHOOK) ::    ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('DDR_LAMINAR_RES',0,ZHOOK_HANDLE)

DO JL = KIDIA, KFDIA 
    PWRB(JL) = (2.0_JPRB / (PPKARMAN * PUSTAR(JL))) * (PPINVI * PXDIMO / (PPDIMOH2O * PPRAND))**(2.0_JPRB / 3.0_JPRB)
ENDDO

  ! Resistances limited to a maximum value
WHERE (PWRB(KIDIA:KFDIA) >  PPRMAX) PWRB(KIDIA:KFDIA) = PPRMAX

IF (LHOOK) CALL DR_HOOK('DDR_LAMINAR_RES',1,ZHOOK_HANDLE)

END SUBROUTINE DDR_LAMINAR_RES
