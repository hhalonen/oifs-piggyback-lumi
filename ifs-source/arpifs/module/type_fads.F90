MODULE TYPE_FADS

! Purpose :
! -------
!    To define the Field Arpege Descriptors :
!     - %CLNAME : ARPEGE field name
!     - %NBITS  : number of bits to code in Arpege/Aladin file
!                 (-1 : default, 0 : no packing, >0 : nb of bits)
         
! Interface :
! ---------
!    Empty.

! External :
! --------
!    None.

! Method :
! ------

! Reference :
! ---------
!    Arpege Aladin Files package

! Author :
! ------
!    Ryad El Khatib *METEO-FRANCE*

! Modifications :
! -------------
! Original : 2003-08-19
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!        F.Bouyssel    10-Nov-2005 Change in second descriptor of FAD
!-----------------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM ,   JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK
IMPLICIT NONE
SAVE

PRIVATE
PUBLIC FAD, YSUFAD

TYPE FAD
CHARACTER(LEN=16) :: CLNAME
INTEGER(KIND=JPIM):: NBITS
END TYPE FAD

CONTAINS

!-----------------------------------------------------------------------------

TYPE(FAD) FUNCTION YSUFAD(CDNAME,KBITS)

! Purpose :
! -------
!    To set default values to the type

CHARACTER(LEN=*)  , INTENT(IN) :: CDNAME
INTEGER(KIND=JPIM), INTENT(IN) :: KBITS
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('TYPE_FADS:YSUFAD',0,ZHOOK_HANDLE)
YSUFAD%NBITS =KBITS
YSUFAD%CLNAME=CDNAME
IF (LHOOK) CALL DR_HOOK('TYPE_FADS:YSUFAD',1,ZHOOK_HANDLE)

END FUNCTION YSUFAD

END MODULE TYPE_FADS
