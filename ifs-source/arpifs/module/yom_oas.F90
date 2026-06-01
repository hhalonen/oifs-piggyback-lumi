MODULE YOM_OAS

! -- 98-05-29       Author: S. Valcke

!  Contents : choice for the OASIS version: clim or pipe or sipc
!  --------
!  Modified : 00-02-18 JPh Piedelievre (MPI2)

IMPLICIT NONE
SAVE
CHARACTER(LEN=8), PARAMETER ::  CPHAN='SIPC' ! as $CHANNEL in namcouple
!     CHARACTER(len=3), PARAMETER ::  cpjobnam= 'IPC'

!     CHARACTER(len=8), PARAMETER ::  cphan='MPI2' ! as $CHANNEL in namcouple
CHARACTER(LEN=6), DIMENSION(2) :: CLBID

CHARACTER(LEN=6), PARAMETER ::  CPMODNAM= 'arpege'
! as $NBMODEL in namcouple
CHARACTER(LEN=3), PARAMETER ::  CPJOBNAM= 'CLI'
! as $JOBNAM in namcouple
CHARACTER(LEN=3), PARAMETER ::  CPMODINF= 'NOT'

END MODULE YOM_OAS
