SUBROUTINE IO_SERV_FREE (YDIOS)
use yomio_serv, only:&
 & io_serv
TYPE(IO_SERV), INTENT (INOUT) :: YDIOS
call abor1("io_serv_free.F90 should never be called with OpenIFS - EXITING")
END SUBROUTINE IO_SERV_FREE
