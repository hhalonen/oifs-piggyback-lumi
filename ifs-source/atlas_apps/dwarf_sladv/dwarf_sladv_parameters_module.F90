module dwarf_sladv_parameters_module

use dwarf_sladv_kind_module

implicit none

real(wp), parameter, public :: rpi=2.0_wp*asin(1.0_wp)
real(wp), parameter, public :: Deg2Rad=RPI/180._wp
real(wp), parameter, public :: Rad2Deg=180._wp/RPI
real(wp), parameter, public :: rndoff = epsilon(1.0_wp)
real(wp), parameter, public :: earth_radius = 6371229._wp

end module
