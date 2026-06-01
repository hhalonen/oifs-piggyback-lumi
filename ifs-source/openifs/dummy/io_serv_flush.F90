SUBROUTINE IO_SERV_FLUSH (YDIOS)
use yomio_serv, only:&
 & io_serv
TYPE(IO_SERV), INTENT (INOUT) :: YDIOS
call abor1("io_serv_flush.F90 should never be called with OpenIFS - EXITING")
END SUBROUTINE IO_SERV_FLUSH
