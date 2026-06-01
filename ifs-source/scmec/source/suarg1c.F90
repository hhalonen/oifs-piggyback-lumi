SUBROUTINE SUARG1C

!*** *SUARG1C* - Routine to initialize common containing command line argument

!     Purpose.
!     --------
!           Initialize common YOMARG
!**   Interface.
!     ----------
!        *CALL* *SUARG1C

!        Explicit arguments :
!        --------------------
!        None

!        Implicit arguments :
!        --------------------
!        COMMON YOMLUN

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------
!        Called by SU0YOM1C

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the SCM

!     Author.
!     -------
!        Joao Teixeira  *ECMWF*

!     Modifications.
!     --------------
!        Original      94-04-19
!        M. Ko"hler    6-6-2006  Single Column Model integration within IFS 

USE PARKIND1 , ONLY : JPIM, JPRB
!USE YOMARG  , ONLY : NCONF, NUSTOP, NECMWF, NSLAG, UTSTEP
USE YOMLUN   , ONLY : NULNAM
USE YOMARG   , ONLY : NUCONF=>NCONF   ,NUECMWF=>NECMWF  ,&
 &                    NECMWF, NCONF, NSUPERSEDE, NFPSERVER

!     ------------------------------------------------------------------

IMPLICIT NONE

CHARACTER :: CUSTOP, CNMEXP
LOGICAL :: LECMWF,LELAM
!INTEGER(KIND=JPIM) :: nsupersede
!INTEGER(KIND=JPIM) :: NFPSERVER
REAL (KIND=JPRB) :: TSTEP=-9999._JPRB

#include "posnam.intfb.h"
#include "namarg.nam.h"
#include "abor1.intfb.h"


!*       1.    SET DEFAULT VALUES FOR PARAMETERS      
!              ---------------------------------      

NCONF=-1
NUECMWF=1  ! Yes, alwas ECMWF here.

CALL POSNAM(NULNAM,'NAMARG')
READ(NULNAM,NAMARG)

! Security for surplus parameter TSTEP
IF (TSTEP /= -9999._JPRB) THEN
  WRITE(*,*) ' For influencing the  model timestep UTSTEP should be used rather than TSTEP.'
  CALL ABOR1('SUARG: ABOR1 CALLED')
ENDIF

!!IF( LUSLAG )THEN
!!  NUSLAG=3
!!ELSE
!!  NUSLAG=1
!!ENDIF
END SUBROUTINE SUARG1C
