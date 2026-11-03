!>
!! @par Copyright
!! This code is subject to the MPI-M-Software - License - Agreement in it's most recent form.
!! Please see URL http://www.mpimet.mpg.de/en/science/models/model-distribution.html and the
!! file COPYING in the root of the source tree for this code.
!! Where software is supplied by third parties, it is indicated in the headers of the routines.
!!
MODULE mo_io_units_lite

  IMPLICIT NONE
  
  PUBLIC
  
  ! This paramter is taken from /usr/include/stdio.h (ANSI C standard). If 
  ! problems with filename length appear, check the before mentioned file.
  
  INTEGER, PARAMETER :: filename_max = 1024
  
  ! Standard I/O-units
  
#ifdef hpux
  INTEGER, PARAMETER :: nerr  = 7     ! error output
#else
  INTEGER, PARAMETER :: nerr  = 0     ! error output
#endif
  INTEGER, PARAMETER :: nlog  = 1     ! standard log file unit
  INTEGER, PARAMETER :: nin   = 5     ! standard input
  INTEGER, PARAMETER :: nout  = 6     ! standard output  
  
  INTEGER, PARAMETER, PRIVATE :: none = -1  ! unit given back, when nothing 
                                            ! in the allowed range is available

  INTEGER :: nnml_output ! unit of the ASCII output that contains the
  ! namelist variables and their actual values.
!-------------------------------------------------------------------------

CONTAINS
  !
  FUNCTION find_next_free_unit(istart, istop) RESULT(iunit)
    INTEGER :: iunit
    INTEGER, INTENT(IN) :: istart, istop
    !
    INTEGER :: kstart, kstop
    LOGICAL :: lfound, lopened
    INTEGER :: i
    !
    lfound = .FALSE.
    !
    kstart = istart
    kstop = istop
    IF (kstart < 10) kstart = 10
    IF (kstop <= kstart) kstop = kstart + 10
    !
    DO i = kstart, kstop
      INQUIRE (unit=i, opened=lopened)
      IF (.NOT. lopened) THEN
        iunit = i
        lfound = .TRUE.
        EXIT
      END IF
    END DO
    !
    IF (.NOT. lfound) THEN
      iunit = NONE
    END IF
    !
  END FUNCTION find_next_free_unit

  
END MODULE mo_io_units_lite











