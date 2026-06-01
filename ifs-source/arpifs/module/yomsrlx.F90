MODULE YOMSRLX

USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

! -----------------------------------------------------------------------------

!     SPECTRAL ARRAYS FOR RELAXATION

! XPRLXG: temporal coefficient.

! TRLXTE: coefficient for temperature.
! TRLXDI: coefficient for divergence.
! TRLXVO: coefficient for vorticity.
! TRLXQ : coefficient for specific humidity.
! TRLXLP: coefficient for log(prehyds).
! TRLXVO3: coefficient for ozone .

REAL(KIND=JPRB),ALLOCATABLE:: XPRLXG(:)

REAL(KIND=JPRB),ALLOCATABLE:: TRLXTE(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE:: TRLXDI(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE:: TRLXVO(:,:,:)
REAL(KIND=JPRB),ALLOCATABLE:: TRLXQ(:,:,:,:)
REAL(KIND=JPRB),ALLOCATABLE:: TRLXQI(:,:,:,:)
REAL(KIND=JPRB),ALLOCATABLE:: TRLXQL(:,:,:,:)
REAL(KIND=JPRB),ALLOCATABLE:: TRLXQC(:,:,:,:)
REAL(KIND=JPRB),ALLOCATABLE:: TRLXO3(:,:,:,:)
REAL(KIND=JPRB),ALLOCATABLE:: TRLXLP(:,:)

! -----------------------------------------------------------------------------

END MODULE YOMSRLX
