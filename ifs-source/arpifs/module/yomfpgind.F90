MODULE YOMFPGIND

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

! CONTROL ARRAYS FOR FULLPOS TRANSPOSITION FROM DEPARTURE TO TARGET DISTRIBUTION

! NFP2SEND      : Number of points my source distribution will send to the target distribution
! NFP2RECV      : Number of points my target distribution will get from the source distribution
! NFP2SENDG     : Number of points of a global field to send to the target distribution
! NFPSOURCE     : local indexes of the gridpoints in my source task to send to the target distribution
! NFPTARGET     : Local indexes of the gridpoints in my target task to get from the source distribution
! NFPSOURCEG    : global indexes of the gridpoints to send to the target distribution

TYPE TFPGIND

INTEGER(KIND=JPIM),ALLOCATABLE:: NFP2SEND(:)
INTEGER(KIND=JPIM),ALLOCATABLE:: NFP2RECV(:)
INTEGER(KIND=JPIM),ALLOCATABLE:: NFP2SENDG(:)
INTEGER(KIND=JPIM),ALLOCATABLE:: NFPSOURCE(:,:)
INTEGER(KIND=JPIM),ALLOCATABLE:: NFPTARGET(:,:)
INTEGER(KIND=JPIM),ALLOCATABLE:: NFPSOURCEG(:,:)

END TYPE TFPGIND

!     ------------------------------------------------------------------      
END MODULE YOMFPGIND
