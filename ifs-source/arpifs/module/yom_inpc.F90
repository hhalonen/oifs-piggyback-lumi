MODULE YOM_INPC

! -- 98-05-29    Author: S. Valcke

!  Contents : variables describing pools formed of shared memory segments
!  --------

! -- mpoolinit(r/w) : handles associated to model pools for passing initial info

! -- mpoolwrit : handles associated to pools used to pass fields exchanged
!               from model to coupler (see libsipc/SIPC_Write_Model.f)

! -- mpoolread : handles associated to pools used to pass fields exchanged
!               from model to coupler (see libsipc/SIPC_Read_Model.f)

!-------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM

USE PAR_COU   , ONLY : JPMAXFLD

IMPLICIT NONE
SAVE
INTEGER(KIND=JPIM) ::  MPOOLINITR
INTEGER(KIND=JPIM) ::  MPOOLINITW
INTEGER(KIND=JPIM), DIMENSION(JPMAXFLD)  ::  MPOOLWRIT
INTEGER(KIND=JPIM), DIMENSION(JPMAXFLD)  ::  MPOOLREAD
!     -------------------------------------------------------------------
END MODULE YOM_INPC
