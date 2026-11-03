! 
! based on mo_filename_util.f90 
! in https://git.iac.ethz.ch/hammoz/icon-hammoz.git (hammoz/gpu/master)
!
! Philipp Weiss, University of Oxford  
!
! ICON
!
! ---------------------------------------------------------------
! Copyright (C) 2004-2024, DWD, MPI-M, DKRZ, KIT, ETH, MeteoSwiss
! Contact information: icon-model.org
!
! See AUTHORS.TXT for a list of authors
! See LICENSES/ for license information
! SPDX-License-Identifier: BSD-3-Clause
! ---------------------------------------------------------------

MODULE mo_util_filename_lite

  USE mo_exception_lite, ONLY:message, message_text, finish

  IMPLICIT NONE
  PRIVATE                    

  PUBLIC :: str_filter          

CONTAINS

!---------------------------------------------------------------------------------------------------

  FUNCTION str_filter(vorlage,yr,mo,dy,hr,mi,se,nn,cc,tg,ll) RESULT(str1)

    CHARACTER(len=*)   ,INTENT(in)           :: vorlage
    INTEGER            ,INTENT(in) ,OPTIONAL :: yr, mo, dy, hr, mi, se, nn
    CHARACTER(len=*)   ,INTENT(in) ,OPTIONAL :: cc
    CHARACTER(len=*)   ,INTENT(in) ,OPTIONAL :: tg, ll   ! resolution 'Tnnn' (for ECHAM), 'Gnn' (for ICON)  
    CHARACTER(len=256)                       :: str1
    
    !---------------------------------------------------
    ! filter the string until all token are solved
    ! same token can be multiple used
    ! 
    ! Token    Replacement                       format
    !
    !  %Yx     yr (year)                               Ix.x
    !  %Mx     mo (month as number)                    Ix.x
    !  %Ax     mo (month as short name 'JAN'...)       A3
    !  %Dx     dy (day)                                Ix.x
    !  %Hx     hr (hour)                               Ix.x
    !  %Ix     mi (minute)                             Ix.x
    !  %Sx     se (second)                             Ix.x
    !  %Nx     nn (a number)                           Ix.x
    !  %Cx     cc (a character string)                 A
    !  %Tx     tg ECHAM spectral truncation as char    A
    !  %Gx     tg ICON grid name                       Ax
    !  %Lx     ll (number of levels as char)           A
    !
    !  x is a digit (0...9). If x is 0 the minimum length
    !    required to write the integer number is used.
    !----------------------------------------------------

    CHARACTER(256) :: str2
    CHARACTER(1), PARAMETER :: token = '%'

    CHARACTER(3)   :: mon(12) = (/&
         'JAN','FEB','MAR','APR','MAY','JUN',&
         'JUL','AUG','SEP','OCT','NOV','DEC' /)

    CHARACTER(1)  :: str
    CHARACTER(64) :: transfer
    CHARACTER(20) :: form, cval
    INTEGER       :: pos_token, str_len, value

    str1 = TRIM(vorlage)
    DO
       ! find token position

       pos_token = INDEX(str1, token)
       IF (pos_token == 0) EXIT  ! all token expanded

       str_len  = LEN_TRIM(str1)
       IF (pos_token+2 > str_len) &
         CALL finish('str_filter','String length too large.')

       ! determine parameter to insert

       SELECT CASE(str1(pos_token+1:pos_token+1))    ! expand token and insert
       CASE('y','Y'); value = yr        !   year
       CASE('m','M'); value = mo        !   month as number
       CASE('d','D'); value = dy        !   day
       CASE('h','H'); value = hr        !   hour
       CASE('i','I'); value = mi        !   minute
       CASE('s','S'); value = se        !   second
       CASE('n','N'); value = nn        !   a number
       CASE('a','A'); value = mo        !   month as short name
       CASE('c','C'); value = 0         !   character string
       CASE('t','T'); value = 0         !   spectral truncation (character string)
       CASE('l','L'); value = 0         !   number of levels (character string)
       CASE('g','G'); value = 0         !   grid name (character string)
       CASE default
          CALL finish('str_filter','Token not defined.')
       END SELECT

       ! determine format

       str = str1(pos_token+2:pos_token+2)

       ! determine length automatically if zero length is given

       IF (str == '0') THEN
         cval = ''
         WRITE (cval, *    ) value
         do
           if(cval(1:1)/=' ') exit
           cval = cval(2:)
         end do
         WRITE (str ,'(i1)') LEN_TRIM (cval)
       ENDIF

       SELECT CASE(str)
       CASE('1','2','3','4','5','6','7','8','9')
          WRITE(form,'(a2,a1,a1,a1,a1)') '(i',str,'.',str,')'
       CASE default
          CALL finish('str_filter','Token format not correct.')
       END SELECT
       SELECT CASE(str1(pos_token+1:pos_token+1))  ! expand token and insert
         CASE('a','A')                             !   month as short name
           str2 = str1(1:pos_token-1) // mon(mo) // str1(pos_token+3:str_len)
         CASE('c','C')                             ! string
           str2 = str1(1:pos_token-1) // TRIM(cc)// str1(pos_token+3:str_len)
         CASE('t','T','g','G')                     ! either ECHAM spectral truncation string or ICON grid name
           str2 = str1(1:pos_token-1) // TRIM(tg)// str1(pos_token+3:str_len)
         CASE('l','L')                             ! nlev string
           str2 = str1(1:pos_token-1) // TRIM(ll)// str1(pos_token+3:str_len)
         CASE default                              !   integer value
           WRITE(transfer,form)   value
           str2 = str1(1:pos_token-1) // TRIM(transfer) //&
                  str1(pos_token+3:str_len)
       END SELECT
       str1 = TRIM(str2)

    END DO

  END FUNCTION str_filter

END MODULE mo_util_filename_lite
