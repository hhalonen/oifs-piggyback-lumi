! 
! based on mo_time_conversion.f90
! in https://git.iac.ethz.ch/hammoz/icon-hammoz.git (hammoz/gpu/master)
!
! Philipp Weiss, University of Oxford  
!
! ICON
!
! ---------------------------------------------------------------
! Copyright (C) 2004-2024, DWD, MPI-M, DKRZ, KIT, ETH, MeteoSwiss
! Contact information: icon-model.org
!
! See AUTHORS.TXT for a list of authors
! See LICENSES/ for license information
! SPDX-License-Identifier: BSD-3-Clause
! ---------------------------------------------------------------

MODULE mo_util_timeconv_lite

  USE mo_kind_lite,   ONLY:  wp, i8
  ! USE mtime,     ONLY:  getPTStringFromSeconds, MAX_TIMEDELTA_STR_LEN, &
  !                     & timedelta, newTimedelta, OPERATOR(+),          &
  !                     & deallocateDatetime, deallocateTimedelta,       &
  !                     & datetime, newDatetime, MAX_DATETIME_STR_LEN,   &
  !                     & datetimeToString, getPTStringFromMS
  USE mtime_lite,     ONLY: datetime
  USE mtime_constants_lite, ONLY: MAX_DATETIME_STR_LEN, MAX_TIMEDELTA_STR_LEN


  IMPLICIT NONE
  PRIVATE

  TYPE datetimes
    TYPE(datetime), POINTER :: datetime
  END TYPE datetimes  

  CHARACTER(LEN=*), PARAMETER :: thismodule='mo_time_conv'

  PUBLIC  :: datetimes
  ! PUBLIC  :: TC_split
  ! PUBLIC  :: TC_get
  ! PUBLIC  :: TC_set
  ! PUBLIC  :: add_date
  ! PUBLIC  :: write_date
  ! PUBLIC  :: add_n_month_to_date

  ! INTERFACE get_date_str
  !   MODULE PROCEDURE get_date_str_from_comp
  !   MODULE PROCEDURE get_date_str_from_mdate
  ! END INTERFACE get_date_str

  CONTAINS

!---------------------------------------------------------------------------------------------------

!   SUBROUTINE TC_split( my_date,  year, month, day, hour, minute, second )
!     TYPE(datetime), POINTER, INTENT ( IN ) :: my_date
!     INTEGER, INTENT( OUT ) :: year, month, day, hour, minute, second

!     year   = INT(my_date%date%year) 
!     month  = my_date%date%month
!     day    = my_date%date%day
!     hour   = my_date%time%hour 
!     minute = my_date%time%minute
!     second = my_date%time%second

!   END SUBROUTINE TC_split

! !---------------------------------------------------------------------------------------------------

!   SUBROUTINE TC_get( my_date_in,  year, month, day, hour, minute, second, lprevious, lnext )

!     USE mo_run_config,             ONLY: dtime ! time step length (s)

!     TYPE(datetime), POINTER, INTENT ( IN ) :: my_date_in
!     INTEGER, INTENT( OUT ), OPTIONAL :: year, month, day, hour, minute, second
!     LOGICAL, INTENT( IN ) , OPTIONAL :: lprevious, lnext ! get component of previous or next time step
    
!     TYPE(datetime), POINTER :: my_date

!     my_date => newDatetime( my_date_in )
    
!     IF ( PRESENT(lprevious) ) THEN
!       IF ( lprevious ) THEN
!          CALL add_date(my_date, milliseconds=-1e3_wp*dtime)
!       END IF
!     END IF

!     IF ( PRESENT(lnext) ) THEN
!       IF ( lnext ) THEN
!          CALL add_date(my_date, milliseconds=1e3_wp*dtime)
!       END IF
!     END IF

!     IF ( PRESENT( year ) ) THEN 
!       year   = INT(my_date%date%year)
!     END IF
!     IF ( PRESENT( month ) ) THEN 
!       month  = my_date%date%month
!     END IF
!     IF ( PRESENT( day ) ) THEN 
!       day    = my_date%date%day
!     END IF
!     IF ( PRESENT( hour ) ) THEN 
!       hour   = my_date%time%hour
!     END IF
!     IF ( PRESENT( minute ) ) THEN  
!       minute = my_date%time%minute
!     END IF
!     IF ( PRESENT( second ) ) THEN 
!       second = my_date%time%second
!     END IF

!     CALL deallocateDatetime( my_date )
!   END SUBROUTINE TC_get

! !---------------------------------------------------------------------------------------------------

!   SUBROUTINE TC_set( year, month, day, hour, minute, second, my_date )
    
!     INTEGER, INTENT( IN ) :: year, month, day, hour, minute, second
!     TYPE( datetime ), POINTER, INTENT( INOUT ) :: my_date
!     CHARACTER(LEN=MAX_DATETIME_STR_LEN) ::  mdatestr


!     mdatestr = get_date_str( year, month, day, hour, minute, second )
!     my_date => newDatetime( mdatestr )


!   END SUBROUTINE TC_set

! !---------------------------------------------------------------------------------------------------

  ! SUBROUTINE add_date(  my_date, years, months, days, hours, minutes, seconds, milliseconds )

  !   USE mo_exception,     ONLY: finish

  !   TYPE( datetime ),  POINTER, INTENT( INOUT ) :: my_date
  !   INTEGER,                    INTENT( IN ),  OPTIONAL    :: years, months, days, hours, minutes, seconds
  !   REAL(wp)                                ,  OPTIONAL    :: milliseconds

  !   ! local vars
  !   CHARACTER(LEN=MAX_TIMEDELTA_STR_LEN)  :: dt_string, dtm_string
  !   REAL(wp)                              :: add_sec
  !   TYPE( timedelta ), POINTER            :: dtime 

  !   INTEGER  :: cday, chour, cminute, csecond, ierr

  !   IF ( PRESENT (  days ) ) THEN
  !     cday=days
  !   ELSE
  !     cday=0
  !   END IF

  !   IF ( PRESENT (  hours ) ) THEN
  !     chour=hours
  !   ELSE
  !     chour=0
  !   END IF

  !   IF ( PRESENT (  minutes ) ) THEN
  !     cminute=minutes
  !   ELSE
  !     cminute=0
  !   END IF

  !   IF ( PRESENT (  seconds ) ) THEN
  !     csecond=seconds
  !   ELSE
  !     csecond=0
  !   END IF

  !   add_sec=86400._wp*cday+3600._wp*chour+60._wp*cminute+REAL(csecond,wp)
  !   CALL getPTStringFromSeconds(add_sec , dt_string)
   
  !   dtime   => newTimedelta(dt_string,ierr)
  !   IF ( ierr .NE. 0 ) THEN
  !     CALL finish(thismodule, "Error: dtime")
  !   END IF

  !   !CALL print_date( my_date )

  !   my_date = my_date + dtime

  !   CALL deallocateTimedelta(dtime)

  !   IF ( PRESENT (months ) ) THEN
  !      CALL add_n_month_to_date( my_date, months )
  !   END IF
  !   IF ( PRESENT (years ) ) THEN
  !      CALL add_n_year_to_date( my_date, years )
  !   END IF

  !   IF ( PRESENT ( milliseconds )) THEN
  !     CALL getPTStringFromMS(INT(milliseconds, i8),dtm_string)
  !     dtime   => newTimedelta(dtm_string)
  !     my_date = my_date + dtime 
  !     CALL deallocateTimedelta(dtime)
  !   END IF 
  !   !CALL print_date( my_date )
 
  ! END SUBROUTINE add_date

! !---------------------------------------------------------------------------------------------------

!   FUNCTION get_date_str_from_comp( year, month, day, hour, minute, second ) RESULT( date_str )
    
!     INTEGER, INTENT ( IN ) ::  year, month, day, hour, minute, second 
!     CHARACTER(LEN=MAX_DATETIME_STR_LEN) :: date_str

!     CHARACTER(LEN=4) :: cyear
!     CHARACTER(LEN=2) :: cday, cmonth, chour, cminute, csecond
!     CHARACTER(LEN=1), PARAMETER :: s='-', d=':'

!     WRITE(cyear,   FMT='(I4.4)') year 
!     WRITE(cmonth,  FMT='(I2.2)') month
!     WRITE(cday,    FMT='(I2.2)') day
!     WRITE(chour,   FMT='(I2.2)') hour 
!     WRITE(cminute, FMT='(I2.2)') minute
!     WRITE(csecond, FMT='(I2.2)') second

!     date_str = cyear//s//cmonth//s//cday//'T'//chour//d//cminute//d//csecond//'.000' 

!   END FUNCTION get_date_str_from_comp

! !---------------------------------------------------------------------------------------------------

!   FUNCTION get_date_str_from_mdate (  my_date ) RESULT( date_str )
!     TYPE( datetime ), INTENT( IN ) :: my_date     

!     CHARACTER(LEN=MAX_DATETIME_STR_LEN) :: date_str

!      date_str = get_date_str_from_comp( INT(my_date%date%year), my_date%date%month, my_date%date%day, &
!                                       & my_date%time%hour,  my_date%time%minute,  my_date%time%second )

!   END FUNCTION get_date_str_from_mdate

! !---------------------------------------------------------------------------------------------------

!   FUNCTION get_date_str_in_local_format( my_date ) RESULT( cdate )
!     TYPE( datetime ), POINTER, INTENT( IN ) :: my_date
!     CHARACTER(LEN=19) :: cdate     
!     CHARACTER(LEN=4) :: cyear
!     CHARACTER(LEN=2) :: cday, cmonth, chour, cminute, csecond
!     CHARACTER(LEN=1), PARAMETER :: s='-', d=':'
   
!     WRITE(cyear,   FMT='(I4.4)') my_date%date%year 
!     WRITE(cmonth,  FMT='(I2.2)') my_date%date%month
!     WRITE(cday,    FMT='(I2.2)') my_date%date%day
!     WRITE(chour,   FMT='(I2.2)') my_date%time%hour 
!     WRITE(cminute, FMT='(I2.2)') my_date%time%minute
!     WRITE(csecond, FMT='(I2.2)') my_date%time%second

!     cdate=cyear//s//cmonth//s//cday//' '//chour//d//cminute//d//csecond

!   END FUNCTION get_date_str_in_local_format

! !---------------------------------------------------------------------------------------------------

!   SUBROUTINE write_date( my_date, text, all_print )
!     USE mo_impl_constants, ONLY: MAX_CHAR_LENGTH 
!     USE mo_exception, ONLY: message
!     TYPE( datetime ), POINTER, INTENT( IN ) :: my_date
!     CHARACTER(len=*), OPTIONAL, INTENT(in) :: text
!     LOGICAL, OPTIONAL, INTENT(in) :: all_print
!     CHARACTER(LEN=19) :: cdate
!     CHARACTER(len=MAX_CHAR_LENGTH)   :: dmessage

!     cdate=get_date_str_in_local_format( my_date )
 
!     IF ( PRESENT(text) ) THEN
!       dmessage=TRIM(text) // ' ' // cdate
!     ELSE
!       dmessage=cdate
!     END IF
  
!     IF ( PRESENT( all_print ) ) THEN
!       CALL message('',dmessage, all_print=all_print)
!     ELSE
!       CALL message('',dmessage)
!     END IF
!   END SUBROUTINE write_date

! !---------------------------------------------------------------------------------------------------

!   SUBROUTINE print_date( my_date )
!     USE mo_exception, ONLY: message
!     TYPE( datetime ), POINTER, INTENT( IN ) :: my_date
!     CHARACTER(LEN=19) :: cdate
!     cdate=get_date_str_in_local_format( my_date )
 
!     CALL message('',cdate)
    
!   END SUBROUTINE print_date

! !---------------------------------------------------------------------------------------------------

!   FUNCTION get_dtc(x, unit) RESULT ( dtc )
!     INTEGER,          INTENT(IN) :: x
!     CHARACTER(LEN=1), INTENT(IN) :: unit

!     CHARACTER(LEN=20) :: cx
!     CHARACTER(LEN=30) :: dtc 

!      cx=''
!      WRITE(cx, FMT='(I20)') ABS(x)

!      SELECT CASE ( x )
!         CASE ( :-1 ) 
!            dtc='-P'//TRIM(ADJUSTL(cx))//unit
!         CASE ( 1: )
!            dtc='P'//TRIM(ADJUSTL(cx))//unit
!         CASE DEFAULT 
!            dtc='P0' 
!      END SELECT 

!   END FUNCTION get_dtc

! !---------------------------------------------------------------------------------------------------

!   SUBROUTINE add_n_year_to_date( my_date, nyear )
!     USE mtime,                 ONLY: datetime, timedelta,                  &
!                                 &  OPERATOR(+),                             &
!                                 &  newTimedelta, deallocateTimedelta
!      TYPE( datetime ), POINTER, INTENT( IN ) :: my_date
!      INTEGER, INTENT( IN ) :: nyear  

!      TYPE( timedelta ), POINTER :: dtime
!      CHARACTER(LEN=30) ::  dtc

!      dtc=get_dtc(  nyear, 'Y')

!      dtime => newTimedelta(TRIM(dtc))

!      my_date = my_date + dtime

!      CALL deallocateTimedelta( dtime  )
  
!   END SUBROUTINE add_n_year_to_date

! !---------------------------------------------------------------------------------------------------

!   SUBROUTINE add_n_month_to_date( my_date, nmonth_in )
!      ! som mtime operators such as multiplication apparently only work with TXX
!      USE mtime,                 ONLY: datetime, timedelta,                  &
!                                 &  OPERATOR(+),                             &
!                                 &  newTimedelta, deallocateTimedelta
!      TYPE( datetime ), POINTER, INTENT( IN ) :: my_date
!      INTEGER, INTENT( IN ) :: nmonth_in   

!      TYPE( timedelta ), POINTER :: dtime
!      INTEGER :: nyear, nmonth

!      CHARACTER(LEN=30) ::  dtc
     
!      nmonth=MOD(nmonth_in, 12)
!      nyear=(nmonth_in-nmonth)/12
 
!      dtc=get_dtc(  nmonth, 'M')

!      dtime => newTimedelta(TRIM(dtc))

!      my_date = my_date + dtime

!      CALL deallocateTimedelta( dtime  )
  
!      IF ( ABS(nyear) .GT. 0 ) THEN
!        CALL add_n_year_to_date(  my_date, nyear )
!      END IF 
  
!   END SUBROUTINE add_n_month_to_date   
  
END MODULE mo_util_timeconv_lite
