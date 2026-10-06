!eehol: module mtime for hamlite boxmodel
MODULE mtime_lite
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int, c_int64_t, c_char, c_null_char, c_ptr, &
       &                                 c_loc, c_f_pointer, c_associated
  USE mtime_constants_lite, ONLY: max_timedelta_str_len, max_datetime_str_len

  IMPLICIT NONE

  TYPE, BIND(C) :: date
    INTEGER(c_int64_t) :: year
    INTEGER(c_int) :: month
    INTEGER(c_int) :: day
  END TYPE date

  TYPE, BIND(C) :: time
    INTEGER(c_int) :: hour
    INTEGER(c_int) :: minute
    INTEGER(c_int) :: second
    INTEGER(c_int) :: ms
  END TYPE time

  TYPE, BIND(C) :: datetime
    TYPE(date) :: date
    TYPE(time) :: time
  END TYPE datetime

  TYPE, PUBLIC :: t_datetime
    TYPE(datetime) :: value
  END TYPE t_datetime

  TYPE, PUBLIC :: timeDelta
    INTEGER :: days    = 0
    INTEGER :: hours   = 0
    INTEGER :: minutes = 0
    INTEGER :: seconds = 0
    INTEGER :: milliseconds = 0
  END TYPE timeDelta

CONTAINS

  FUNCTION newDatetime(str, errno) RESULT(dt_ptr)
    CHARACTER(LEN=*), INTENT(IN) :: str
    INTEGER, INTENT(OUT) :: errno
    TYPE(datetime), POINTER :: dt_ptr
    ALLOCATE(dt_ptr)
    dt_ptr%date%year = 0
    dt_ptr%date%month = 1
    dt_ptr%date%day = 1
    dt_ptr%time%hour = 0
    dt_ptr%time%minute = 0
    dt_ptr%time%second = 0
    dt_ptr%time%ms = 0
    errno = 0
  END FUNCTION newDatetime

  SUBROUTINE datetimeToString(dt_ptr, str, errno)
    TYPE(datetime), POINTER, INTENT(IN) :: dt_ptr
    CHARACTER(LEN=*), INTENT(OUT) :: str
    INTEGER, INTENT(OUT) :: errno
    str = ''
    errno = 0
  END SUBROUTINE datetimeToString

  SUBROUTINE deallocateDatetime(dt_ptr)
    TYPE(datetime), POINTER, INTENT(INOUT) :: dt_ptr
    IF (ASSOCIATED(dt_ptr)) DEALLOCATE(dt_ptr)
  END SUBROUTINE deallocateDatetime

END MODULE mtime_lite