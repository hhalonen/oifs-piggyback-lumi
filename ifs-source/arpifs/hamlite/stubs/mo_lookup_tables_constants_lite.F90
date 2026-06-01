MODULE mo_lookup_tables_constants_lite
  USE mo_kind_lite, ONLY: wp
  IMPLICIT NONE
  PUBLIC
  ! Lookup table bounds for saturation computations (from ICON)
  REAL(wp), PARAMETER :: jptlucu1 = 50._wp   ! lower bound temp (K)
  REAL(wp), PARAMETER :: jptlucu2 = 400._wp  ! upper bound temp (K)
  INTEGER, PARAMETER :: n_lkup = 35001       ! table size
  
  ! c1es and friends from ICON/ECHAM saturation pressure
  REAL(wp), PARAMETER :: c1es  = 610.78_wp         ! Pa
  REAL(wp), PARAMETER :: c2es  = 17.2693882_wp     ! water
  REAL(wp), PARAMETER :: c3les = 35.86_wp           ! water offset
  REAL(wp), PARAMETER :: c3ies = 7.66_wp            ! ice offset
  REAL(wp), PARAMETER :: c4les = 0._wp
  REAL(wp), PARAMETER :: c4ies = 0._wp
  REAL(wp), PARAMETER :: c5les = c2es * (273.16_wp - c3les)
  REAL(wp), PARAMETER :: c5ies = 21.8745584_wp * (273.16_wp - c3ies)
END MODULE mo_lookup_tables_constants_lite
