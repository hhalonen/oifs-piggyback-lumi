Interface

SUBROUTINE RTTOV_EC_ALLOC( &
     & errorstatus, ksat, nchannels, nprofiles, nlevels,  &
     & ec_opts, &
     & asw)

  Use parkind1, Only : jpim
  Use rttov_ec_mod, only : rttov_ec_opts

  IMPLICIT NONE

 ! Subroutine arguments
  ! Scalar arguments with intent(in):
  Integer(Kind=jpim), INTENT(in) :: ksat        ! Satellite index (see rttvi)
  Integer(Kind=jpim), INTENT(in) :: nchannels   ! Number of radiances
  Integer(Kind=jpim), INTENT(in) :: nprofiles   ! Number of profiles
  Integer(Kind=jpim), INTENT(in) :: nlevels     ! Number of input levels
  Integer(Kind=jpim), INTENT(in) :: asw         ! Allocate/Deallocate flag

  Type(rttov_ec_opts), INTENT(in) :: ec_opts ! Options

  ! Array  arguments with intent(out):
  Integer(Kind=jpim) , INTENT(out):: errorstatus  !  return flag 

END SUBROUTINE RTTOV_EC_ALLOC

End Interface
