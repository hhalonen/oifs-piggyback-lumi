MODULE YOMTRANS

USE PARKIND1  ,ONLY : JPIM, JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!*    SPECTRAL TRANSFORMS CONTROL VARIABLES

!     NPROMATR : max. number of fields to be transformed at each call in
!                arpege/ifs inverse transforms
!                = 0 : all fields
!                > 0 : such number of fields
!     NEPROMATR : max. number of fields to be transformed at each call in
!                aladin inverse transforms
!                = 0 : all fields
!                > 0 : such number of fields
!     NMAX_RESOL: number of computed resolutions
!     RDISTR_E  : between 0. and 1. (debault 0.) part of E zone that will
!                 be distributed
!     LUSEFLT,LUSERPNM,LKEEPRPNM - see TRANS package
!     LALLOPERM : allocate certain arrays permanently
!     LFFTW     : Use FFTW if true (see TRANS package)


INTEGER(KIND=JPIM) :: NPROMATR
INTEGER(KIND=JPIM) :: NEPROMATR
INTEGER(KIND=JPIM) :: NMAX_RESOL
REAL(KIND=JPRB) :: RDISTR_E 
LOGICAL         :: LMONO_TRANS
LOGICAL         :: LUSEFLT
LOGICAL         :: LUSERPNM
LOGICAL         :: LKEEPRPNM
LOGICAL         :: LALLOPERM
LOGICAL         :: LFFTW
!     ------------------------------------------------------------------
END MODULE YOMTRANS
