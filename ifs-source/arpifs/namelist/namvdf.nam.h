!****--------------------------------------------------------------------
!**** NAMVDF : Namelist for the vert diffusion scheme
!****
!****  Author   : P. Bechtold ECMWF 12/2014
!****
!***  RLAM   : ASYMPTOTIC MIXING LENGTH FOR MOMENTUM
!***  RVDIFTS: FACTOR FOR TIME STEP WEIGHTING IN *VDF....*
!***  LWDS   : .T. for Wood/Diamantakis/Staniforth scheme      
!***  NSUBST : Number of substeps in VDF           

!****
!****--------------------------------------------------------------------
NAMELIST / NAMVDF / RLAM, NSUBST, LWDS, RVDIFTS, RTOFDALPHA, REISTHSC


