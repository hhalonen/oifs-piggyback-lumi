MODULE mo_master_control_lite
  IMPLICIT NONE
  PUBLIC :: get_my_process_type, hamocc_process, ocean_process, process_exists, &
       & get_my_process_name, my_process_is_hamocc, my_process_is_ocean
  INTEGER, PARAMETER :: hamocc_process = -1
  INTEGER, PARAMETER :: ocean_process  = -2
CONTAINS
  FUNCTION get_my_process_type() RESULT(itype)
    INTEGER :: itype
    itype = 0  ! atmosphere
  END FUNCTION get_my_process_type
  FUNCTION process_exists(itype) RESULT(lexists)
    INTEGER, INTENT(IN) :: itype
    LOGICAL :: lexists
    lexists = .FALSE.
  END FUNCTION process_exists
  FUNCTION get_my_process_name() RESULT(cname)
    CHARACTER(LEN=32) :: cname
    cname = 'OpenIFS'
  END FUNCTION get_my_process_name
  FUNCTION my_process_is_hamocc() RESULT(l)
    LOGICAL :: l
    l = .FALSE.
  END FUNCTION my_process_is_hamocc
  FUNCTION my_process_is_ocean() RESULT(l)
    LOGICAL :: l
    l = .FALSE.
  END FUNCTION my_process_is_ocean
END MODULE mo_master_control_lite
