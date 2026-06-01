MODULE YOS_SW

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!       ----------------------------------------------------------------
!*    ** *YOS_SW* - COEFFICIENTS FOR SHORTWAVE RADIATION TRANSFER
!       ----------------------------------------------------------------

TYPE :: TSW
REAL(KIND=JPRB), ALLOCATABLE :: RSUN(:)      ! SOLAR FRACTION IN SPECTRAL INTERVALS
REAL(KIND=JPRB), ALLOCATABLE :: RALBICE_AR(:,:) ! monthly sea-ice albedo 
                                       ! in SW spectral intervals 
!                            for Antarctica
REAL(KIND=JPRB), ALLOCATABLE :: RALBICE_AN(:,:) ! for Arctic
!                     1 - ocean (flat response)
!                     2 - sea-ice (as snow or ice depending on month)
!                     3 - wet skin (flat)
!                     4 - vegetation low and snow-free (BR, 1986)
!                     5 - snow on low vegetation       (Warren, 1982)
!                     6 - vegetation high and snow-free (BR, 1986)
!                     7 - snow under high vegetation   (Warren, 1982)
!                     8 - bare soil (Briegleb, Ramanathan, 1986)
!     -----------------------------------------------------------------
END TYPE TSW

END MODULE YOS_SW
