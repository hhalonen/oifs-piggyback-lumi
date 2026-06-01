! (C) Copyright 2005- ECMWF.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

MODULE STACK_OVERWRITE 
!----------------------------------------------------------------------------------
! This module allows you to check whether an application thread is writing into 
! the stack of its neighbouring thread
!   To use
!     Set N= parthds value in XLSMPOPTS environment variable
!     Set NN= to that area of stack which should not be used
!       <---thread 1----------> <-----thread 2-------> <---...
!       !======|===============|========|==============|====...
!       <--NN-->                <--NN--> 
!       <---------N-----------> <---------N---------->
!     Add "CALL STACK_OWRITE_SET" in first routine
!     Add "CALL STACK_OWRITE_CHK" to any routines called after stack may have been corrupted
!     Add "USE STACK_OVERWRITE" to all modified routines
!   Compile this routine and all modified routines
!   Link and run
!     Get "STACK_OWRITE_CHK: nt,ii,zstk=" if stack overwriten
!----------------------------------------------------------------------------------
  USE EC_PARKIND , ONLY : JPIM
!$ USE OMP_LIB
  SAVE
  INTEGER*8 :: STACK_OWRITE_BEG(16)         ! Change if max threads > 16
  INTEGER(KIND=JPIM) N,NN
  PARAMETER(N=25*1000*1000,NN=5*1000*1000)

  CONTAINS
  SUBROUTINE STACK_OWRITE_SET
  IMPLICIT NONE
  INTEGER(KIND=JPIM) N,NN,NT,MT,IER
  PARAMETER(N=25*1000*1000)
  PARAMETER(NN=5*1000*1000)
  INCLUDE "mpif.h"
  INTEGER(KIND=JPIM) ZSTK(N)
  INTEGER(KIND=JPIM) II,ZOFF,NNN
  INTEGER*8 ZTMP
  NT=0
  MT=1
!$  NT=OMP_GET_THREAD_NUM()
!$  MT=OMP_GET_MAX_THREADS()
  ZSTK(1:NN)=9999
  IF(NT>0) THEN
    ZTMP=LOC(ZSTK(1))
    STACK_OWRITE_BEG(NT)=ZTMP
    ZOFF=(STACK_OWRITE_BEG(NT)-ZTMP)/4
!   write(0,*) "STACK_OWRITE_SET: nt,stack_owrite_beg,loc(zstk(1)),zoff=",nt,stack_owrite_beg(nt),loc(zstk(1)),zoff
!   do ii=1,2
!     if(zstk(zoff+ii).ne.9999) then
!       write(0,*) "STACK_OWRITE_SET: nt,ii,zstk=",nt,ii,zstk(zoff+ii)
!     endif
!   enddo
!-----deliberate overwrite of neighbouring stack---------
    NNN=N+NN/2
    WRITE(0,*) "mt,nnn=",MT,NNN
    IF(NT<MT-1) ZSTK(NNN)=0
!-----------------------------------
  ENDIF
  CALL STACK_OWRITE_DUM(ZSTK)
  END SUBROUTINE STACK_OWRITE_SET

  SUBROUTINE STACK_OWRITE_CHK
  IMPLICIT NONE
  INTEGER(KIND=JPIM) OMP_GET_THREAD_NUM
  INTEGER(KIND=JPIM) N,NN,NT,II
  INTEGER(KIND=JPIM) ZOFF
  PARAMETER(N=25*1000*1000)
  PARAMETER(NN=5*1000*1000)
  INTEGER(KIND=JPIM) ZSTK(N)
  INTEGER*8 ZTMP
  NT=0
!$  NT=OMP_GET_THREAD_NUM()
  IF(NT>0) THEN
    ZTMP=LOC(ZSTK(1))
    ZOFF=(STACK_OWRITE_BEG(NT)-ZTMP)/4
!   write(0,*) "STACK_OWRITE_CHK: nt,owrite_beg,loc(zstk(1)),zoff=",nt,stack_owrite_beg(nt),loc(zstk(1)),zoff
!   write(0,*) "STACK_OWRITE_CHK: nt,1,zstk=",nt,1,zstk(zoff+1)
    DO II=1,NN
      IF(ZSTK(ZOFF+II).NE.9999) THEN
        WRITE(0,*) "STACK_OWRITE_CHK: nt,ii,zstk=",NT,II,ZSTK(ZOFF+II)
      ENDIF
    ENDDO
!   write(0,*) "STACK_OWRITE_CHK: nt,NN,zstk=",nt,NN,zstk(zoff+NN)
  ENDIF
  END SUBROUTINE STACK_OWRITE_CHK
  
  SUBROUTINE STACK_OWRITE_DUM(IDUM)
  INTEGER(KIND=JPIM) IDUM(N)
  END SUBROUTINE STACK_OWRITE_DUM

END MODULE STACK_OVERWRITE
