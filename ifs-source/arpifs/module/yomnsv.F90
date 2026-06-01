MODULE YOMNSV

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

!*
!    -----------------------------------------------------------------

!    VARIABLES DE BORNE DU VECTEUR GFL_EXT POUR L'UTILISATION de LA CHIMIE et AEROSOL  AROME :

!    NSV_CHEMBEG  : premier indice chimie gazeux
!    NSV_CHEMEND  : dernier indice chimie gazeux
!    NSV_AERBEG  : premier indice chimie aerosol
!    NSV_AEREND  : dernier indice chimie aerosol
!    NSV_DSTBEG  : premier indice poussieres desertiques
!    NSV_DSTEND  : dernier indice poussieres desertiques
!    NSV_DSTDEPBEG  : premier indice poussieres desertiques microphysique
!    NSV_DSTDEPEND  : dernier indice poussieres desertiques microphysique
!    NSV_CO2        : Indice du CO2


INTEGER(KIND=JPIM) :: NSV_CHEMBEG
INTEGER(KIND=JPIM) :: NSV_CHEMEND
INTEGER(KIND=JPIM) :: NSV_AERBEG
INTEGER(KIND=JPIM) :: NSV_AEREND
INTEGER(KIND=JPIM) :: NSV_DSTBEG
INTEGER(KIND=JPIM) :: NSV_DSTEND
INTEGER(KIND=JPIM) :: NSV_DSTDEPBEG
INTEGER(KIND=JPIM) :: NSV_DSTDEPEND
INTEGER(KIND=JPIM) :: NSV_CO2=0

!    -------------------------------------------------------------------
END MODULE YOMNSV

