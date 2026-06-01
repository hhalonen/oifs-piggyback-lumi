MODULE YOMSCM

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!-------------------------------------------------
! SCM: extraction of Single Column Model profiles from 3D model.
!-------------------------------------------------

! NFRSCM
! NSCMTS

! NSCM_SPACE_S < 0: choose one profile over -NSCM_SPACE_S.
! NSCM_SPACE_S = 0: choose profiles close to the location
!                   given by (GSCM_LON1,GSCM_LAT1) (in rad).
! NSCM_SPACE_S = 1: choose profiles inside a given box (in (lon, lat)).
! NSCM_SPACE_S = 10: choose profiles depending on physical output,
! for example precipitating profiles.
!-------------------------------------------------

! NSCM_SPACE_S: kind of space sampling.

! NSCM_SPACE_S < 0: choose one profile over -NSCM_SPACE_S.
! NSCM_SPACE_S = 0: choose profiles close to the location
!                   given by (GSCM_LON1,GSCM_LAT1) (in rad).
! NSCM_SPACE_S = 1: choose profiles inside a given box (in (lon, lat)).
! NSCM_SPACE_S = 10: choose profiles depending on physical output,
! for example precipitating profiles.
!-------------------------------------------------
! GSCM_LON1: longitude in rad [-pi,pi].
! GSCM_LON2: longitude in rad [-pi,pi].
! GSCM_LAT1: latitude  in rad [-pi/2,pi/2].
! GSCM_LAT2: latitude  in rad [-pi/2,pi/2].
!-------------------------------------------------
! GSCM_RADIUS: search radius in meters (case NSCM_SPACE_S = 0).
!-------------------------------------------------

LOGICAL :: LGSCM
INTEGER(KIND=JPIM) :: NFRSCM
INTEGER(KIND=JPIM), PARAMETER :: JPNPST=240
INTEGER(KIND=JPIM) :: NSCMTS(0:JPNPST)
INTEGER(KIND=JPIM) :: NSCM_SPACE_S
REAL(KIND=JPRB) :: GSCM_LON1
REAL(KIND=JPRB) :: GSCM_LON2
REAL(KIND=JPRB) :: GSCM_LAT1
REAL(KIND=JPRB) :: GSCM_LAT2
REAL(KIND=JPRB) :: GSCM_RADIUS
INTEGER(KIND=JPIM) :: NSCM_ADD_SAMPL

END MODULE YOMSCM
