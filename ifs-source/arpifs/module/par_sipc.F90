MODULE PAR_SIPC

! -- jpbyteint : number of bytes per integer

! -- jpbyterea : number of bytes per real

! -- jpbytecha : number of bytes per character

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE
SAVE
INTEGER(KIND=JPIM), PARAMETER :: JPBYTEINT=4
INTEGER(KIND=JPIM), PARAMETER :: JPBYTEREA=8
INTEGER(KIND=JPIM), PARAMETER :: JPBYTECHA=1

! -- jptest :  The models will test during 2*jptest seconds if the file
!              DUMMY_SIPC has been created by OASIS, signaling that the
!              SHM pools are opened. After, they will abort.

INTEGER(KIND=JPIM), PARAMETER ::  JPTEST=100

END MODULE PAR_SIPC

