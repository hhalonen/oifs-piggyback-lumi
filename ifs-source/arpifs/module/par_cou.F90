MODULE PAR_COU
!  Modified : 00-02-18 JPh Piedelievre (MPI2)
!  Modified : 09-10-15 A.Alias  gelato et courants oceans (J.F. Gueremy)

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE
SAVE
INTEGER(KIND=JPIM), PARAMETER  :: JPMAXFLD=50  ! Number of maximum fields exchanged
! between ocean and atmosphere
INTEGER(KIND=JPIM) :: JPFLDA2O   ! Number of fields exchanged from
! atmosphere to ocean, initialized in inicou
INTEGER(KIND=JPIM) :: JPFLDO2A   ! Number of fields exchanged from
! ocean to atmosphere, initialized in inicou

END MODULE PAR_COU
