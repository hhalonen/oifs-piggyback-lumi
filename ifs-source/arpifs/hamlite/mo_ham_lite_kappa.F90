! 
! based on mo_ham_kappa.f90 
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
!
! parameterisation of hygroscopic growth of aerosols 
!
!            [    A   ]      (gf**3 -1)
!      RH*exp[- ----- ] = ----------------
!            [  Rd*gf ]   gf**3 - (1-kappa)
! 
! equation 11 in 
! M. Petters and S. Kreidenweis, A single parameter representation of hygroscopic 
! growth and cloud condensation nucleus activity, ACP 7, 1961-1971, 2007
! 
! lookup table contains growth factor (gf) as a function of 
! dry radius (rd), temperature (t), relative humidity (rh), hygroscopicity (kappa) 
!
! lookup table implements a rh axis that has uneven value spacing, 
! monotonically decreasing from the minimum rh (0.3) to the maximum rh (0.995)
!
! nth value on rh axis is given by
!
!   rh(n) = rh(n-1) + delta_rh * c**(n-1), n = 1,2,3,...
!
! where c < 1 is a constant and delta_rh = rh(1) - rh(0) is the initial spacing.
! c is chosen to be 0.96, rh(0) = 0.30 and delta_rh = 0.03 for practical reasons. 
! c and delta_rh are obtained from the lookup table by:
!
!   delta_rh = rh(1) - rh(0)           
!   c = (rh(2) - rh(1)) / delta_rh     
!
! comments:
! the water uptake is almost totally insensitive to temperature. 
! the temperature axis is reduced in range and more coarsely spaced. 
! the dry radius axis is more finely spaced.
! the kappa axis is slightly reduced by removing values above 1.12.

MODULE mo_ham_lite_kappa
 
  USE mo_kind_lite, ONLY: dp
  
  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: start_kappa, end_kappa

  PUBLIC :: Rd, T, rh, kappa, gf 
 
  PUBLIC :: Rd_min, Rd_max,         &
            ln_Rd_min, ln_Rd_max,   &
            T_min, T_max,           &
            rh_min, rh_max,         &
            kappa_min, kappa_max
 
  PUBLIC :: N_Rd, N_T, N_rh, N_kappa
 
  PUBLIC :: rh_init_step,           &
            inv_rh_init_step,       &
            rh_step_compress,       &
            log_rh_step_compress
 
  !--------------------
  ! module parameters
  !--------------------

  ! file name for lookup table
  CHARACTER(LEN=256), PARAMETER :: lut_fn = "/scratch/project_462000807/" // &
  "hhermann/oifs-hamlite-coupling/ifs-source/arpifs/hamlite/orig_nc/" // &
  "lut_kappa.nc"

  ! dimension names in lookup table
  CHARACTER(LEN=16), PARAMETER :: cdimn_Rd = "DryRadius"
  CHARACTER(LEN=16), PARAMETER :: cdimn_T  = "Temperature"
  CHARACTER(LEN=16), PARAMETER :: cdimn_rh = "RelativeHumidity"
  CHARACTER(LEN=16), PARAMETER :: cdimn_k = "kappa"

  ! variable names in lookup table
  CHARACTER(LEN=16), PARAMETER :: cvarn_Rd = "DryRadius"
  CHARACTER(LEN=16), PARAMETER :: cvarn_T = "Temperature"
  CHARACTER(LEN=16), PARAMETER :: cvarn_rh = "RelativeHumidity"
  CHARACTER(LEN=16), PARAMETER :: cvarn_k = "kappa"
  CHARACTER(LEN=16), PARAMETER :: cvarn_gf= "GF"

  ! attribute names in lookup table
  CHARACTER(LEN=7),  PARAMETER :: cattr_version = "Version"

  !--------------------
  ! module variables
  !--------------------
  
  ! number of increments on each dimension
  INTEGER :: N_Rd, N_T, N_rh, N_kappa

  REAL(dp), ALLOCATABLE :: Rd(:)
  REAL(dp), ALLOCATABLE :: T(:)
  REAL(dp), ALLOCATABLE :: rh(:)
  REAL(dp), ALLOCATABLE :: kappa(:)

  ! aerosol hygroscopic growth factor lookup table
  REAL(dp), ALLOCATABLE :: gf(:,:,:,:)

  ! max and min values in each coordinate
  REAL(dp) :: Rd_min, Rd_max, ln_Rd_min, ln_Rd_max
  REAL(dp) :: T_min, T_max
  REAL(dp) :: rh_min, rh_max
  REAL(dp) :: kappa_min, kappa_max
 
  ! for non-linear spacing of the RH axis in version 2: initial rh,
  ! initial step size (plus its reciprocal) and step compression factor (plus its log)
  REAL(dp) :: rh_init_step
  REAL(dp) :: inv_rh_init_step
  REAL(dp) :: rh_step_compress
  REAL(dp) :: log_rh_step_compress
 
  CONTAINS
  
!---------------------------------------------------------------------------------------------------

  SUBROUTINE start_kappa
 
    USE mo_exception_lite,         ONLY: finish, message, print_value 
    USE mo_netcdf_parallel_lite,   ONLY: p_nf90_open, p_nf90_get_att, p_nf90_inq_dimid, &
                                    p_nf90_inquire_dimension, p_nf90_close 
    USE mo_util_netcdf77_lite,     ONLY: read_var_nf77_1d, read_var_nf77_4d 
    !USE mo_mpi_lite,               ONLY: p_pe, p_io, p_bcast, p_comm_work
    USE mo_util_string_lite,       ONLY: separator          
    
    IMPLICIT NONE

    INCLUDE 'netcdf.inc'  
     
    INTEGER :: zncid                    ! netcdf file identifier
    INTEGER :: zdimid_Rd, zdimid_T, &   ! netcdf dimension identifiers
               zdimid_rh, zdimid_k
    INTEGER :: idum  
    LOGICAL :: lexist                
    CHARACTER(LEN=32) :: attrname  
    CHARACTER(LEN=3) :: versval     ! version number, as string (x.y) 
 
    !--- check that the lookup table exists
    INQUIRE(file=TRIM(lut_fn), exist=lexist)
    IF (.NOT. lexist) CALL finish('start_kappa', 'lookup table '//TRIM(lut_fn)//' not found')

    !--- open netcdf file 
    CALL nf_check(p_nf90_open(TRIM(lut_fn), nf_nowrite, zncid))

    attrname = cattr_version
    CALL nf_check(p_nf90_get_att(zncid, nf_global, attrname, versval))
       
    IF (TRIM(attrname) == "not available") THEN   
      CALL finish('start_kappa', 'lookup table '//TRIM(lut_fn)//' version '//versval//' not supported')
    END IF

    !--- get dimensions

    ! dry radius
    CALL nf_check(p_nf90_inq_dimid(zncid, TRIM(cdimn_Rd), zdimid_Rd))
    CALL nf_check(p_nf90_inquire_dimension(zncid, zdimid_Rd, len = N_Rd))
    ! temperature
    CALL nf_check(p_nf90_inq_dimid(zncid, TRIM(cdimn_T), zdimid_T))
    CALL nf_check(p_nf90_inquire_dimension(zncid, zdimid_T, len = N_T)) 
    ! relative humidity
    CALL nf_check(p_nf90_inq_dimid(zncid, TRIM(cdimn_rh), zdimid_rh))
    CALL nf_check(p_nf90_inquire_dimension(zncid, zdimid_rh, len = N_rh)) 
    ! kappa
    CALL nf_check(p_nf90_inq_dimid(zncid, TRIM(cdimn_k), zdimid_k))
    CALL nf_check(p_nf90_inquire_dimension(zncid, zdimid_k, len = N_kappa))

    !--- allocate arrays

    ! coordinates 
    !IF (  p_pe .EQ. p_io  ) THEN 
      ALLOCATE(Rd(N_Rd))
      ALLOCATE(T(N_T))
      ALLOCATE(rh(N_rh))
      ALLOCATE(kappa(N_kappa))
    !END IF

    ! lookup table for growth factor
    ALLOCATE(gf(N_Rd, N_T, N_rh, N_kappa))
  
    !IF ( p_pe .EQ. p_io ) THEN 

      ! coordinate variables
      CALL read_var_nf77_1d(lut_fn, TRIM(cdimn_Rd), TRIM(cvarn_Rd), Rd, idum)
      CALL read_var_nf77_1d(lut_fn, TRIM(cdimn_T), TRIM(cvarn_T), T, idum)
      CALL read_var_nf77_1d(lut_fn, TRIM(cdimn_rh), TRIM(cvarn_rh), rh, idum)
      CALL read_var_nf77_1d(lut_fn, TRIM(cdimn_k), TRIM(cvarn_k), kappa, idum)

      ! lookup table for lookup table
      CALL read_var_nf77_4d(lut_fn, TRIM(cdimn_Rd), TRIM(cdimn_T),    & 
                            TRIM(cdimn_rh), TRIM(cdimn_k), TRIM(cvarn_gf), gf, idum)   

      ! get min and max values of the coordinate variables
      Rd_min = Rd(1)
      Rd_max = Rd(N_Rd)
      ln_Rd_min = LOG(Rd_min)
      ln_Rd_max = LOG(Rd_max)
      T_min = T(1)
      T_max = T(N_T)
      rh_min = rh(1)
      rh_max = rh(N_rh)
      kappa_min = kappa(1)
      kappa_max = kappa(N_kappa)
      
      rh_init_step = rh(2)-rh(1)
      inv_rh_init_step = 1._dp/rh_init_step
      rh_step_compress = (rh(3)-rh(2))/rh_init_step ! compression ratio 
      log_rh_step_compress = LOG(rh_step_compress)

    !END IF

    !--- close file
    CALL nf_check(p_nf90_close(zncid))
 
    !--- broadcast over processors  
    !CALL p_bcast(Rd_min,    p_io, p_comm_work)
    !CALL p_bcast(Rd_max,    p_io, p_comm_work)
    !CALL p_bcast(ln_Rd_min, p_io, p_comm_work)
    !CALL p_bcast(ln_Rd_max, p_io, p_comm_work)
    !CALL p_bcast(N_Rd,      p_io, p_comm_work)
    !CALL p_bcast(T_min,     p_io, p_comm_work)
    !CALL p_bcast(T_max,     p_io, p_comm_work)
    !CALL p_bcast(N_T,       p_io, p_comm_work)
    !CALL p_bcast(rh_min,    p_io, p_comm_work)
    !CALL p_bcast(rh_max,    p_io, p_comm_work)
    !CALL p_bcast(N_rh,      p_io, p_comm_work)
    !
    !CALL p_bcast(rh_init_step,          p_io, p_comm_work)
    !CALL p_bcast(inv_rh_init_step,      p_io, p_comm_work)
    !CALL p_bcast(rh_step_compress,      p_io, p_comm_work)
    !CALL p_bcast(log_rh_step_compress,  p_io, p_comm_work)
    !
    !CALL p_bcast(kappa_min,  p_io, p_comm_work)
    !CALL p_bcast(kappa_max,  p_io, p_comm_work)
    !CALL p_bcast(N_kappa,    p_io, p_comm_work)
    !
    !CALL p_bcast(gf,  p_io, p_comm_work) 

    !--- report lookup table data
    CALL message('','')
    CALL message('',separator) 
    CALL message('start_kappa','Aerosol hygroscopic growth lookup table data:')
    CALL message('','')
    CALL message(' Table version: ', versval)
    CALL print_value(' Size of radius dimension : ', N_Rd)
    CALL print_value(' Min. radius: ', Rd_min)
    CALL print_value(' Max. radius: ', Rd_max)
    CALL print_value(' Size of temperature dimension : ', N_T) 
    CALL print_value(' Min. temperature: ', T_min)
    CALL print_value(' Max. temperature: ', T_max)
    CALL print_value(' Size of relative humidity dimension : ', N_rh) 
    CALL print_value(' Min. RH : ', rh_min)
    CALL print_value(' Max. RH : ', rh_max)
    CALL print_value(' Size of hygroscopicity dimension : ', N_kappa )
    CALL print_value(' Min. kappa: ', kappa_min)
    CALL print_value(' Max. kappa: ', kappa_max)
    CALL message('','')

    !--- check lookup table
    CALL check_kappa

    !--- release memory holding the coordinate variables
    !IF ( p_pe .eq. p_io ) THEN
       DEALLOCATE(Rd)
       DEALLOCATE(T)
       DEALLOCATE(rh)
       DEALLOCATE(kappa)
    !END IF
 
  END SUBROUTINE start_kappa

!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE check_kappa 
     
    !USE mo_mpi_lite,       ONLY: p_pe, p_io 
   
    INTEGER, PARAMETER :: n_check = 100
    LOGICAL, PARAMETER :: lcheck_kappa = .FALSE.

    INTEGER :: irh, jk   
    REAL(dp) :: rhm, rhv

    IF (lcheck_kappa) THEN 
      !IF (p_pe == p_io) THEN

        write(*,*) 'check lookup table for hygroscopic growth factor' 

        DO jk = 1,n_check
            
          rhv = REAL(jk - 1,dp)/REAL(n_check,dp) 
          rhv = MAX(rhv, 0._dp)
          rhv = MIN(rhv, 1._dp) 

          rhm = LOG(1._dp - (1._dp - rh_step_compress) * (rhv - rh_min) * inv_rh_init_step)
          irh = CEILING(rhm / log_rh_step_compress)
          irh = MAX(irh, 1)
          irh = MIN(irh, N_rh)

          write(*,'(a,5x,i10,e12.4,e12.4)') 'irh, rh(irh), rh', irh, rh(irh), rhv

        END DO
        
      !END IF
    END IF
 
  END SUBROUTINE check_kappa

!---------------------------------------------------------------------------------------------------

  SUBROUTINE end_kappa
 
    IMPLICIT NONE
 
    DEALLOCATE(gf)

  END SUBROUTINE end_kappa

!---------------------------------------------------------------------------------------------------

  SUBROUTINE nf_check(iret)

    USE mo_exception_lite,           ONLY: finish 
    USE netcdf,                 ONLY: nf90_noerr, nf90_strerror

    IMPLICIT NONE

    INTEGER, INTENT(in) :: iret

    IF (iret /= nf90_noerr) THEN
      CALL finish('mo_ham_lite_kappa', nf90_strerror(iret))
    ENDIF

  END SUBROUTINE nf_check
  
END MODULE mo_ham_lite_kappa
