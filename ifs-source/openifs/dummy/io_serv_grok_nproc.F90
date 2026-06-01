SUBROUTINE IO_SERV_GROK_NPROC (KULNAM, KPROC)
use parkind1, only:&
 & jpim
INTEGER (KIND=JPIM), INTENT (IN) :: KULNAM
INTEGER (KIND=JPIM), INTENT (INOUT) :: KPROC
call abor1("io_serv_grok_nproc.F90 should never be called with OpenIFS - EXITING")
END SUBROUTINE IO_SERV_GROK_NPROC
