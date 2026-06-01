SUBROUTINE GRIBIOFLUSH

        !**** *GRIBIOFLUSH*  - Flush grib I/Os at the end of a step

!     Purpose.
!     --------
!      Flush grib I/Os at the end of a step

!**   Interface.
!     ----------
!        *CALL* *GRIBIOFLUSH

!        Explicit arguments :
!        --------------------
!        None

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!      R. El Khatib *Meteo-France
!      Original : 02-Apr-2015 from CNT4.

! Modifications
! -------------
! End Modifications
!      ----------------------------------------------------------------

USE PARKIND1 , ONLY : JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMMP0   , ONLY : NOUTTYPE
USE IOSTREAM_MIX  , ONLY : FLUSH_IOSTREAM, Y_IOSTREAM_FDB
USE MPL_MODULE, ONLY: MPL_BARRIER

!      ----------------------------------------------------------------

IMPLICIT NONE

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!      ----------------------------------------------------------------


!      ----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GRIBIOFLUSH',0,ZHOOK_HANDLE)
!      ----------------------------------------------------------------

IF(NOUTTYPE == 2) THEN
  CALL GSTATS(1710,0)
  CALL FLUSH_IOSTREAM(Y_IOSTREAM_FDB)
  CALL MPL_BARRIER(CDSTRING='GRIBIOFLUSH:')
  CALL GSTATS(1710,1)
ENDIF

!      ----------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('GRIBIOFLUSH',1,ZHOOK_HANDLE)
END SUBROUTINE GRIBIOFLUSH
