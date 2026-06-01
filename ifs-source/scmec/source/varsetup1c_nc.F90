subroutine varsetup1c_nc (kncid, xtype, nvdims, vdims, &
                          varname, longname, unitsname)

!        M. Ko"hler    6-6-2006  Single Column Model integration within IFS 

USE PARKIND1  ,ONLY : JPIM

implicit none

!---global variables
integer(KIND=JPIM)  :: kncid, xtype, nvdims, vdims(nvdims)
character (len = *) :: varname, unitsname, longname

!---local variables
integer(KIND=JPIM)  :: ivarid, istatus

#include "netcdf.inc"

!     ------------------------------------------------------------------
#include "handle_err_nc.intfb.h"
!     ------------------------------------------------------------------

istatus = NF_DEF_VAR (kncid, varname, xtype, nvdims, vdims, ivarid)
call handle_err_nc(istatus)
istatus = NF_PUT_ATT_TEXT (kncid, ivarid, 'units', len(unitsname), unitsname)
call handle_err_nc(istatus)
istatus = NF_PUT_ATT_TEXT (kncid, ivarid, 'long_name', len(longname), longname)
call handle_err_nc(istatus)

end subroutine varsetup1c_nc
