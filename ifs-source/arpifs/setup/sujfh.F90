SUBROUTINE SUJFH

! Purpose :
! -------
!   *SUJFH* Read namelist NAMJFH & Initialize module YOMJFH

! Interface :
! ---------

! Externals :
! ---------
!   POSNAM

! Method :
! ------

! Reference :
! ---------

! Author :
! ------
!   15-Mar-2005 R. El Khatib  *METEO-FRANCE*

! Modifications :
! -------------
!   K. Yessad (July 2014): Move some variables.
!   R. El Khatib 02-Sep-2014 N_VMASS=-1 : attempt to isolate non-vectorizing function
! End Modifications
!-----------------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

USE YOMLUN   , ONLY : NULOUT   ,NULNAM
USE YOMMP0   , ONLY : LOPT_RS6K
USE YOMJFH   , ONLY : N_VMASS
USE YOMCT0   , ONLY : LELAM

!-----------------------------------------------------------------------------

IMPLICIT NONE

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!-----------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SUJFH',0,ZHOOK_HANDLE)
!-----------------------------------------------------------------------------

IF (LOPT_RS6K.AND.(.NOT.LELAM)) THEN
  N_VMASS=8
ELSE
#ifdef NECSX
  N_VMASS=0
#else
  N_VMASS=-1
#endif
ENDIF

WRITE(UNIT=NULOUT,FMT='('' MODULE YOMJFH'')')
WRITE(UNIT=NULOUT,FMT='(''   N_VMASS ='',I2)') N_VMASS 

!-----------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SUJFH',1,ZHOOK_HANDLE)
END SUBROUTINE SUJFH
