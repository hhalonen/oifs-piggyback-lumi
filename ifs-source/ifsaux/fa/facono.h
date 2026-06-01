INTERFACE

SUBROUTINE FACONO                                          &
&           (KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, PCHAMP, &
&            LDCOSP, CDNOMA, KLNOMA, PVALCO, KLONGD,       &
&            LDUNDF, PUNDF)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
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
