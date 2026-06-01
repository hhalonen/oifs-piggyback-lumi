SUBROUTINE SUTRANS(YDGEOMETRY)

!**** *SUTRANS * - Resolution dependent Initialization of the transform package

!     Purpose.  Resolution dependent Initialization of the transform package
!     --------

!**   Interface.  CALL SUTRANS
!     ---------- 

!     Explicit arguments :
!     --------------------

!     Externals.
!     ----------
!        SETUP_TRANS  - resolution dependent initialization

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!        Mats Hamrud *ECMWF*

!     Modifications.
!     --------------
!        Original : 00-10-25
!        R. El Khatib 03-01-24 LMPOFF
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!        R. El Khatib  07-Apr-2005 NPROMATR saved in YOMTRANS
!        D.Salmond     22-Nov-2005 Mods for coarser/finer physics
!        G.Mozdzynski  13-Sep-2006 LEQ_REGION partitioning
!        G. Radnoti    16-03-2009  Option to switch to/from mono tasking transforms 
!        M.Hamrud/N.Wedi 16-Nov-2011 Fast Legendre Transform
!        R. El Khatib  02-Mar-2012 Support for mixed multi-resolutions
!        R. El Khatib  24-Jul-2012 Improved support for mixed multi-resolution
!        T.Wilhelmsson 16-Aug-2013 Move resolution independent setup to SUTRANS0
!        G.Mozdzynski  3-Feb-2015  Move setup of FLT from SUTRANS0
!     ------------------------------------------------------------------

USE GEOMETRY_MOD , ONLY : GEOMETRY
USE PARKIND1 , ONLY : JPIM, JPRB, JPRD
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMLUN   , ONLY : NULOUT, NULNAM
USE YOMMP0   , ONLY : LSPLIT
USE YOMTRANS , ONLY : LUSEFLT, LUSERPNM, LKEEPRPNM, LALLOPERM, LFFTW

!      -----------------------------------------------------------------

IMPLICIT NONE

!      -----------------------------------------------------------------

TYPE(GEOMETRY), INTENT(INOUT) :: YDGEOMETRY
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!      -----------------------------------------------------------------

#include "namtrans.nam.h"

#include "setup_trans.h"

#include "posnam.intfb.h"

!      -----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SUTRANS',0,ZHOOK_HANDLE)
ASSOCIATE(YDDIM=>YDGEOMETRY%YRDIM,YDDIMV=>YDGEOMETRY%YRDIMV,YDGEM=>YDGEOMETRY%YRGEM, YDMP=>YDGEOMETRY%YRMP)
ASSOCIATE(NDGLG=>YDDIM%NDGLG, NRESOL=>YDDIM%NRESOL, NSMAX=>YDDIM%NSMAX, &
 & NLOENG=>YDGEM%NLOENG)

LUSEFLT = NSMAX > 1280
LUSERPNM = .NOT.LUSEFLT
LKEEPRPNM = .NOT.LUSEFLT

LFFTW = .FALSE.

CALL POSNAM(NULNAM,'NAMTRANS')
READ(NULNAM,NAMTRANS)

WRITE(NULOUT,*) ' LUSEFLT=',LUSEFLT
WRITE(NULOUT,*) ' LUSERPNM=',LUSERPNM,' LKEEPRPNM=',LKEEPRPNM
WRITE(NULOUT,*) ' LFFTW=',LFFTW

!*     1.  Setup

CALL SETUP_TRANS(KSMAX=NSMAX,KDGL=NDGLG,KLOEN=NLOENG(1:NDGLG),&
 & LDSPLIT=LSPLIT,&
 & KRESOL=NRESOL,LDUSEFLT=LUSEFLT,LDUSERPNM=LUSERPNM,&
 & LDKEEPRPNM=LKEEPRPNM,LDUSEFFTW=LFFTW)

!      -----------------------------------------------------------------

END ASSOCIATE
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SUTRANS',1,ZHOOK_HANDLE)
END SUBROUTINE SUTRANS
