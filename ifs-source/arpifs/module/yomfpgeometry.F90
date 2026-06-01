MODULE YOMFPGEOMETRY

USE PARKIND1  ,ONLY : JPIM, JPRB
USE TYPE_FPUSERGEO, ONLY : TFPUSERGEO
USE YOMFPGEO      , ONLY : TFPGEO
USE YOMFPGIND     , ONLY : TFPGIND

IMPLICIT NONE

SAVE

PRIVATE
PUBLIC :: TFPGEOMETRY, LFPOSBUF, LFPDISTRIB
!     ------------------------------------------------------------------

! FULLPOS GEOMETRY
! ================

! YFPUSERGEO(:) : User geometries

! YFPGEO      : target mixed grids parameters
! YFPGEO_DEP  : interpolation mixed grids parameters
! YFPGIND     : control arrays for transposition from interpolation geometry to target geometry and vice-versa

! NMDLRESOL   : tag of the input resolution (from the spectral transforms package)
! LFPOSBUF    : .TRUE. if fullpos-shaped buffer should be used (meaning : any change of horizontal geometry)
! LFPOSHOR    : .TRUE. for actual horizontal interpolations and not sampling
! LFPDISTRIB  : .TRUE. if additional transposition between departure geometry and arrival geometry is active

TYPE TFPGEOMETRY

TYPE(TFPUSERGEO), ALLOCATABLE :: YFPUSERGEO(:)

TYPE(TFPGEO)  :: YFPGEO
TYPE(TFPGEO)  :: YFPGEO_DEP
TYPE(TFPGIND) :: YFPGIND

INTEGER(KIND=JPIM) :: NMDLRESOL = 0
LOGICAL :: LFPOSHOR = .TRUE.

END TYPE TFPGEOMETRY

CONTAINS

LOGICAL FUNCTION LFPOSBUF(YDFPGEOMETRY)

TYPE(TFPGEOMETRY), INTENT(IN) :: YDFPGEOMETRY

! Use model-shaped buffer only if the interpolation grid (incl. e-zone) is exactly identical to the model grid
LFPOSBUF = ANY(YDFPGEOMETRY%YFPUSERGEO(:)%LFPOSBUFSHAPE)

END FUNCTION LFPOSBUF

LOGICAL FUNCTION LFPDISTRIB(YDFPGEOMETRY)

TYPE(TFPGEOMETRY), INTENT(IN) :: YDFPGEOMETRY

#include "abor1.intfb.h"

! for now we can't mix the values of nfpdistrib :
IF (MAXVAL(YDFPGEOMETRY%YFPUSERGEO(:)%NFPDIST) /= MINVAL(YDFPGEOMETRY%YFPUSERGEO(:)%NFPDIST)) THEN
  CALL ABOR1('LFPDISTRIB : NFPDIST CANNOT HAVE DIFFERENT VALUES (YET)')
ELSE
  LFPDISTRIB = (YDFPGEOMETRY%YFPUSERGEO(1)%NFPDIST > 0)
ENDIF

END FUNCTION LFPDISTRIB

!     ------------------------------------------------------------------
END MODULE YOMFPGEOMETRY
