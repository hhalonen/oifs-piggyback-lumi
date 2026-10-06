MODULE hamlite_driver_input
!----------------------------------------------------------------------------
!!$   Copyright 2014 Atmospheric Research Centre of Eastern Finland,
!!$         Finnish Meteorological Institute, Kuopio, Finland
!!$
!!$   Licensed under the Apache License, Version 2.0 (the "License");
!!$   you may not use this file except in compliance with the License.
!!$   You may obtain a copy of the License at
!!$
!!$       http://www.apache.org/licenses/LICENSE-2.0
!!$
!!$   Unless required by applicable law or agreed to in writing, software
!!$   distributed under the License is distributed on an "AS IS" BASIS,
!!$   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!!$   See the License for the specific language governing permissions and
!!$   limitations under the License.
!----------------------------------------------------------------------------
  !-------------------------------------------------------
  !-------------------------------------------------------
  !
  ! These are defined in host model - here given for
  !	testing purposes	
  !
  !
  !-------------------------------------------------------

  USE mo_kind_lite

  INTEGER, PARAMETER :: &
       
       kproma = 1, &  ! number of horiz. grid points in the slab (NOTE! in hamlite kproma equals kbdim in full HAM!)
       kbdim = 1,  &  ! defines one dimension of matrices
                                ! (for regular grid kbdim = kproma ????)
       klev = 1,   &      ! number of vertical levels
       krow = 1,   &      ! eehol: added here krow information
       ktdia = 1,  &      ! eehol: added highest vertical level for "diagnostics"

       jg = 1,     &       ! eehol: grid index (for hamlite but not used in processes...)
       jb = 1,     &       ! eehol: index of block loop (for hamlite but not used in processes...)
       jcs = 1,    &       ! eehol: start index of columns loop (for hamlite same as KIDIA (OIFS) or 1 (HAM))
       jce = 1             ! eehol: end index of columns loop (for hamlite same as KFDIA (OIFS) or kproma (HAM))
       !jks = 1,    &       ! eehol: index of top level
END MODULE hamlite_driver_input
