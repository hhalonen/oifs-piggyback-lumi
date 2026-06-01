INTERFACE

SUBROUTINE FACONO_MT                                           &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, PCHAMP, &
&            LDCOSP, CDNOMA, KLNOMA, PVALCO, KLONGD,           &
&            LDUNDF, PUNDF)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDPREF                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIVAU                                 ! IN   
CHARACTER (LEN=*)      CDSUFF                                 ! IN   
REAL (KIND=JPDBLR)     PCHAMP     (*)                         ! IN   
LOGICAL                LDCOSP                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKM)  KLNOMA                                 !   OUT
REAL (KIND=JPDBLR)     PVALCO     (*)                         !   OUT
INTEGER (KIND=JPLIKM)  KLONGD                                 !   OUT
LOGICAL,               OPTIONAL :: LDUNDF                     ! IN
REAL (KIND=JPDBLR),    OPTIONAL :: PUNDF                      ! IN
END SUBROUTINE

END INTERFACE
