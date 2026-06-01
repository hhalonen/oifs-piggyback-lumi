MODULE BUFR_MOD
USE PARKIND1  ,ONLY : JPIM, JPRD

implicit none
private :: JPIM, JPRD

interface bufr_missing_data
  module procedure bufr_missing_data_real, bufr_missing_data_int
end interface

interface bufr_valid_data
  module procedure bufr_valid_data_real, bufr_valid_data_int
end interface

!-- BUFR variable


    REAL(KIND=JPRD)   , parameter      :: rvind=1.7E38_JPRD
    REAL(KIND=JPRD)   , parameter      :: eps  =1.0E-3_JPRD
    REAL(KIND=JPRD)   , parameter      :: xround = 100000. 
    INTEGER(KIND=JPIM), parameter      :: nvind=2147483647

    INTEGER(KIND=JPIM), parameter :: jsup =   9
    INTEGER(KIND=JPIM), parameter :: jsec0=   3
    INTEGER(KIND=JPIM), parameter :: jsec1=  40
    INTEGER(KIND=JPIM), parameter :: jsec2=4096
    INTEGER(KIND=JPIM), parameter :: jsec3=   4
    INTEGER(KIND=JPIM), parameter :: jsec4=   2
    INTEGER(KIND=JPIM), parameter :: jelem=320000
    INTEGER(KIND=JPIM), parameter :: KELEM=320000
    INTEGER(KIND=JPIM), parameter :: JBUFL=512000
    INTEGER(KIND=JPIM), parameter :: jbpw =  32
    INTEGER(KIND=JPIM), parameter :: jwork=4096000
    INTEGER(KIND=JPIM), parameter :: KVALS=4096000
    INTEGER(KIND=JPIM), parameter :: jkey=46
    INTEGER(KIND=JPIM), parameter :: jbyte=2400000

    INTEGER(KIND=JPIM), ALLOCATABLE      :: kbuff(:), kbufr(:)
    INTEGER(KIND=JPIM), dimension(jsup)  :: ksup
    INTEGER(KIND=JPIM), dimension(jsec0) :: ksec0
    INTEGER(KIND=JPIM), dimension(jsec1) :: ksec1
    INTEGER(KIND=JPIM), dimension(jsec2) :: ksec2
    INTEGER(KIND=JPIM), dimension(jsec3) :: ksec3
    INTEGER(KIND=JPIM), dimension(jsec4) :: ksec4
    INTEGER(KIND=JPIM), dimension(jkey)  :: key
    REAL(KIND=JPRD), ALLOCATABLE         :: VALUES(:), VALS(:)
    INTEGER(KIND=JPIM), ALLOCATABLE      :: ktdlst(:)
    INTEGER(KIND=JPIM), ALLOCATABLE      :: ktdexp(:)
    INTEGER(KIND=JPIM), ALLOCATABLE      :: kdata(:)
    INTEGER(KIND=JPIM)                   :: kbox

    INTEGER(KIND=JPIM)                   :: kbufl
    INTEGER(KIND=JPIM)                   :: ktdlen
    INTEGER(KIND=JPIM)                   :: ktdexl
    INTEGER(KIND=JPIM)                   :: kapp
    INTEGER(KIND=JPIM)                   :: klen
    INTEGER(KIND=JPIM), ALLOCATABLE      :: KBOXR(:)
    INTEGER(KIND=JPIM), ALLOCATABLE      :: krq(:)
    INTEGER(KIND=JPIM), ALLOCATABLE      :: kreq(:)
    INTEGER(KIND=JPIM)                   :: krql
    REAL(KIND=JPRD),    ALLOCATABLE      :: rqv(:)


    character(len= 64), ALLOCATABLE      :: cnames(:), cname(:)
    character(len= 24), ALLOCATABLE      :: cunits(:), cunit(:)
    character(len= 80), ALLOCATABLE      :: CVALS(:)
    character(len= 24), ALLOCATABLE      :: cboxu(:)
    character(len= 64), ALLOCATABLE      :: cboxn(:)

 contains

       function my_nint(r)
!
!
!**** *my_nint*
!          M. Dragosavac    *ECMWF*       
!
!
!     Modifications.
!     --------------
!
! 31-Oct-2013      A. Geer   Make it work for -ve numbers too
!---------------------------------------------------------------------------

      REAL(KIND=JPRD)    :: r
      INTEGER(KIND=JPIM) :: my_nint

      if( r > float(nvind)) then
          my_nint=nvind 
      elseif ( r < -float(nvind)) then
          my_nint=-nvind 
      else
          my_nint=nint(r)
      end if

      return
 end function my_nint
!================================
function bufr_round(x)

REAL(KIND=JPRD) :: bufr_round
REAL(KIND=JPRD) :: x
if (abs(x) .lt. 10000000.) then 
  bufr_round= (anint(xround*x))/xround
else
  bufr_round=x
endif

end function bufr_round
!
!**** *bufr_missing_data*
!
! Design note: "eps" is chosen as 1e-3 because differences between floating
! point representations can lead to surprisingly large differences between numbers that 
! are ostensibly "the same". 
!
! For example, on intel desktop, abs(1.7E38-1.7D38)/1.7D38 = 1.4E-8
!
! Here, 1.7E38 is a real*4 and 1.7D38 is a real*8. And this is just internally to one 
! machine. Given that some data formats (and BUFR?) use their own mechanisms for packing 
! and representing floating point numbers, and that encoding can be done on all sorts of
! different platforms, there could potentially be even larger differences, so 1e-3 (+/- 0.1%) 
! is chosen to give a cautious but realistic range around the expected value of 
! the missing data indicator.
!
! 31-Oct-2013      A. Geer   First version - replaces lots of ad-hoc, non-standard tests
!----------------------------------------------------------------------------------------
function bufr_missing_data_real(r)

real(kind=jprd) :: r
logical         :: bufr_missing_data_real

bufr_missing_data_real = ((abs (r - rvind) / rvind) < eps)

return
end function bufr_missing_data_real

function bufr_missing_data_int(k)

integer(kind=jpim) :: k
logical            :: bufr_missing_data_int

bufr_missing_data_int = (k == nvind)

return
end function bufr_missing_data_int

!
!**** *bufr_valid_data*
!
! 31-Oct-2013      A. Geer   First version - replaces lots of ad-hoc, non-standard tests
!----------------------------------------------------------------------------------------
function bufr_valid_data_real(r)

real(kind=jprd) :: r
logical         :: bufr_valid_data_real

bufr_valid_data_real = ((abs (r - rvind) / rvind) >= eps)

return
end function bufr_valid_data_real

function bufr_valid_data_int(k)

integer(kind=jpim) :: k
logical            :: bufr_valid_data_int

bufr_valid_data_int = (k /= nvind)

return
end function bufr_valid_data_int


!**** *b2o_missing_value*

! Converts missing value in bufr data into missing value for ODB

! 31-Oct-2013      A. Geer   First version 
!----------------------------------------------------------------------------------------
subroutine b2o_missing_value(r)

real(kind=jprd) :: r(:,:)

where ((abs (r - rvind) / rvind) < eps)
  r(:,:)=-nvind
endwhere

end subroutine b2o_missing_value

!================================

SUBROUTINE BUFR_ALLOC

USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK

INTEGER(KIND=JPIM) :: IRET
REAL(KIND=JPRD) :: ZHOOK_HANDLE

#include "abor1.intfb.h"

IF (LHOOK) CALL DR_HOOK('BUFR_MOD:BUFR_ALLOC',0,ZHOOK_HANDLE)

ALLOCATE(KBUFF(JBUFL),STAT=IRET)
IF (IRET /= 0) CALL ABOR1('BUFR_ALLOC:COULD NOT ALLOCATE KBUFF')
ALLOCATE(KBUFR(JBUFL),STAT=IRET)
IF (IRET /= 0) CALL ABOR1('BUFR_ALLOC:COULD NOT ALLOCATE KBUFR')

ALLOCATE(VALUES(KVALS),STAT=IRET)
IF (IRET /= 0) CALL ABOR1('BUFR_ALLOC:COULD NOT ALLOCATE VALUES')
ALLOCATE(VALS(KVALS),STAT=IRET)
IF (IRET /= 0) CALL ABOR1('BUFR_ALLOC:COULD NOT ALLOCATE VALS')
ALLOCATE(KBOXR(KVALS),STAT=IRET)
IF (IRET /= 0) CALL ABOR1('BUFR_ALLOC:COULD NOT ALLOCATE KBOXR')
ALLOCATE(CVALS(KVALS),STAT=IRET)
IF (IRET /= 0) CALL ABOR1('BUFR_ALLOC:COULD NOT ALLOCATE CVALS')

ALLOCATE(ktdlst(KELEM),STAT=IRET)
IF (IRET /= 0) CALL ABOR1('BUFR_ALLOC:COULD NOT ALLOCATE ktdlst')
ALLOCATE(ktdexp(KELEM),STAT=IRET)
IF (IRET /= 0) CALL ABOR1('BUFR_ALLOC:COULD NOT ALLOCATE ktdexp')
ALLOCATE(kdata(KELEM),STAT=IRET)
IF (IRET /= 0) CALL ABOR1('BUFR_ALLOC:COULD NOT ALLOCATE kdata')
ALLOCATE(krq(KELEM),STAT=IRET)
IF (IRET /= 0) CALL ABOR1('BUFR_ALLOC:COULD NOT ALLOCATE krq')
ALLOCATE(kreq(KELEM),STAT=IRET)
IF (IRET /= 0) CALL ABOR1('BUFR_ALLOC:COULD NOT ALLOCATE kreq')
ALLOCATE(rqv(KELEM),STAT=IRET)
IF (IRET /= 0) CALL ABOR1('BUFR_ALLOC:COULD NOT ALLOCATE rqv')
ALLOCATE(cnames(KELEM),STAT=IRET)
IF (IRET /= 0) CALL ABOR1('BUFR_ALLOC:COULD NOT ALLOCATE cnames')
ALLOCATE(cname(KELEM),STAT=IRET)
IF (IRET /= 0) CALL ABOR1('BUFR_ALLOC:COULD NOT ALLOCATE cname')
ALLOCATE(cunits(KELEM),STAT=IRET)
IF (IRET /= 0) CALL ABOR1('BUFR_ALLOC:COULD NOT ALLOCATE cunits')
ALLOCATE(cunit(KELEM),STAT=IRET)
IF (IRET /= 0) CALL ABOR1('BUFR_ALLOC:COULD NOT ALLOCATE cunit')
ALLOCATE(cboxu(KELEM),STAT=IRET)
IF (IRET /= 0) CALL ABOR1('BUFR_ALLOC:COULD NOT ALLOCATE cboxu')
ALLOCATE(cboxn(KELEM),STAT=IRET)
IF (IRET /= 0) CALL ABOR1('BUFR_ALLOC:COULD NOT ALLOCATE cboxn')

IF (LHOOK) CALL DR_HOOK('BUFR_MOD:BUFR_ALLOC',1,ZHOOK_HANDLE)

END SUBROUTINE BUFR_ALLOC
!==================================
END MODULE BUFR_MOD
