SUBROUTINE TM5_SUNDIS(KIDIA,KFDIA,KLON,KLEV,KMONTH,KDAY,PRJ)

!**   DESCRIPTION 
!     ----------
!
!   Part of TM5 routines for IFS chemistry: 
!   The evaluation of photolysis rates
!
!=  calculate earth-sun distance variation for a given date.  based on       =*
!=  fourier coefficients originally from:  spencer, j.w., 1971, fourier      =*
!=  series representation of the position of the sun, search, 2:172          =*
!-----------------------------------------------------------------------------*
!= this program is free software;  you can redistribute it and/or modify     =*
!= it under the terms of the gnu general public license as published by the  =*
!= free software foundation;  either version 2 of the license, or (at your   =*
!= option) any later version.                                                =*
!= the tuv package is distributed in the hope that it will be useful, but    =*
!= without any warranty;  without even the implied warranty of merchantibi-  =*
!= lity or fitness for a particular purpose.  see the gnu general public     =*
!= license for more details.                                                 =*
!= to obtain a copy of the gnu general public license, write to:             =*
!= free software foundation, inc., 675 mass ave, cambridge, ma 02139, usa.   =*
!-----------------------------------------------------------------------------*
!= to contact the authors, please mail to:                                   =*
!= sasha madronich, ncar/acd, p.o.box 3000, boulder, co, 80307-3000, usa  or =*
!= send email to:  sasha@ucar.edu                                            =*
!-----------------------------------------------------------------------------*
!= copyright (c) 1994,95,96  university corporation for atmospheric research =*
!-----------------------------------------------------------------------------*  
!   Part of TM5 routines for IFS chemistry: 
!   The evaluation of photolysis rates
!
!
!
!**   INTERFACE.
!     ----------
!          *TM5_SUNDIS* IS CALLED FROM *CHEM_tm5*.
! INPUTS:
! -------
! KMONTH,KDAY  - integer, specification of the date, from yymmdd     
!
! OUTPUTS:
! -------
!  PDISTFAC     - real, variation of the earth-sun distance           
!-----------------------------------------------------------------------------*
!
! AUTHOR
!
! ------
! VINCENT HUIJNEN    *KNMI*
! TM5-community
!
!
!
!-----------------------------------------------------------------------------*  
!
! !USES:
!
USE PARKIND1 , ONLY : JPIM     ,JPRB
USE YOMHOOK  , ONLY : LHOOK,   DR_HOOK, JPHOOK
USE YOMCST   , ONLY : RPI
USE TM5_PHOTOLYSIS, ONLY : NPHOTO
!
    IMPLICIT NONE
!*   ARGUMENTS

INTEGER(KIND=JPIM), INTENT(IN)  ::KIDIA,KFDIA,KLON,KLEV,KDAY,KMONTH
REAL(KIND=JPRB), INTENT(INOUT)    ::PRJ(KLON,KLEV,NPHOTO)

! * LOCAL
REAL(KIND=JPHOOK)    :: ZHOOK_HANDLE

INTEGER(KIND=JPIM) :: JMONTH, IDAYF,JT,JLEV,JL
REAL(KIND=JPRB)    :: ZDAYN, ZTHET0, ZSINTH,ZCOSTH,ZSIN2TH,ZCOS2TH

REAL(KIND=JPRB)    ::ZDISTFAC



INTEGER(KIND=JPIM),DIMENSION(12) ::IMN=(/ 31,28,31,30,31,30,31,31,30,31,30,31 /) 

IF (LHOOK) CALL DR_HOOK('TM5_SUNDIS',0,ZHOOK_HANDLE )
! start

! parse date to find day number (julian day)

IDAYF = 0
DO  JMONTH = 1, KMONTH-1
  IDAYF = IDAYF + IMN(JMONTH)
ENDDO
IDAYF = IDAYF + KDAY
ZDAYN = FLOAT(IDAYF - 1_JPIM) + 0.5_JPRB

! define angular day number and compute PDISTFAC:

ZTHET0 = 2.*RPI*ZDAYN/365.

! calculate sin(2*ZTHET0), cos(2*ZTHET0) from
! addition theoremes for trig functions for better
! performance;  the computation of sin2th, cos2th
! is about 5-6 times faster than the evaluation
! of the intrinsic functions sin and cos
!
ZSINTH = SIN(ZTHET0)
ZCOSTH = COS(ZTHET0)
ZSIN2TH = 2.*ZSINTH*ZCOSTH
ZCOS2TH = ZCOSTH*ZCOSTH - ZSINTH*ZSINTH
ZDISTFAC  = 1.000110 +   0.034221*ZCOSTH  +  0.001280*ZSINTH + &
  &   0.000719*ZCOS2TH +  0.000077*ZSIN2TH
!
! apply correction
DO JT = 1, NPHOTO
  DO JLEV=1,KLEV
    DO JL=KIDIA,KFDIA
     PRJ(JL,JLEV,JT) =ZDISTFAC *  PRJ(JL,JLEV,JT)
    ENDDO
  ENDDO
ENDDO

IF (LHOOK) CALL DR_HOOK('TM5_SUNDIS',1,ZHOOK_HANDLE )
END SUBROUTINE TM5_SUNDIS
