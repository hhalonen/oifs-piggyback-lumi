module dwarf_sladv_kind_module

use, intrinsic :: iso_c_binding, only : c_double, c_float, c_int32_t, c_int64_t


!
!     *** Define usual kinds for strong typing ***
!
implicit none

!
!     integer Kinds
!     -------------
!

integer, parameter :: i4 = c_int32_t
integer, parameter :: i8 = c_int64_t

integer, parameter :: ip = i4

!
!     real Kinds
!     ----------
!
integer, parameter :: sp = c_float
integer, parameter :: dp = c_double

#ifdef HAVE_SINGLE_PRECISION
integer, parameter :: wp = sp
#else
integer, parameter :: wp = dp
#endif

! ifS kinds
INTEGER, PARAMETER :: JPIM = i4
INTEGER, PARAMETER :: JPIB = i8
INTEGER, PARAMETER :: JPRM = sp
INTEGER, PARAMETER :: JPRB = wp
INTEGER, PARAMETER :: JPRD = dp

end module
