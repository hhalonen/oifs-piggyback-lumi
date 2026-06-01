! 
! based on mo_ham_rad.f90 
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
 
MODULE mo_ham_lite_rad

  USE mo_kind_lite,                ONLY: dp
  USE mo_math_constants_lite,      ONLY: pi
  USE mo_subm_spec_lite,           ONLY: speclist 
  USE mo_ham_lite_main,            ONLY: nraddiag, nrad, &
                                    sizeclass, nclass 
  USE mo_ham_lite_spec,            ONLY: id_wat   
  USE mo_ham_lite_rad_data,        ONLY: Nwv_sw, Nwv_sw_opt, Nwv_lw,     &
                                    Nwv_tot, Nwv_sw_tot,            &
                                    cnr, cni,                       &
                                    log_x0_min, log_x0_max,         &
                                    log_ni_min,                     &
                                    x0_min, x0_max,                 &
                                    nnrmax, nnimax, ndismax,        &
                                    nr_min, nr_max, ni_min, ni_max, &
                                    inc_nr, inc_ni,                 &
                                    lambda, lambda_sw_opt,          &
                                    ham_lite_rad_data_init

  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: ham_lite_rad_init,          & 
            ham_lite_rad_calc,          &  
            ham_lite_rad,               & 
!            ham_lite_rad_diag,          &  
            ham_lite_rad_cache,         &  
            ham_lite_rad_cache_cleanup, &
            ham_lite_rad_gp2g,          &
            ham_lite_rad_check 
  
  PUBLIC :: ix_tp, ix_rh, &
            sigma, omega, asym, nr, ni 

  !--------------------
  ! module parameters
  !--------------------
 
  INTEGER, PARAMETER :: nbndlw = 16, nbndsw = 14

  ! wavenumber limits in RTE-RRTMG [cm-1]
  ! 
  ! shortwave bands will be rearranged from RRTMG to RRTMGP  
  ! by rearrange_bands2rrtmgp in rte_rrtmgp_interface

  REAL(dp), PARAMETER :: wavenum1_sw(nbndsw) = (/ & ! lower wavenumber limit
    2680._dp, 3250._dp, 4000._dp, 4650._dp, &
    5150._dp, 6150._dp, 7700._dp, 8050._dp, 12850._dp, &
    16000._dp, 22650._dp, 29000._dp, 38000._dp, 820._dp /) 
  REAL(dp), PARAMETER :: wavenum2_sw(nbndsw) = (/ & ! upper wavenumber limit
    3250._dp, 4000._dp, 4650._dp, 5150._dp, &
    6150._dp, 7700._dp, 8050._dp, 12850._dp, 16000._dp, &
    22650._dp, 29000._dp, 38000._dp, 50000._dp, 2680._dp /)

  REAL(dp), PARAMETER :: wavenum1_lw(nbndlw) = (/ & ! lower wavenumber limit
    10._dp, 250._dp, 500._dp, 630._dp, 700._dp, 820._dp, &
    980._dp, 1080._dp, 1180._dp, 1390._dp, 1480._dp, 1800._dp, &
    2080._dp, 2250._dp, 2390._dp, 2680._dp /)
  REAL(dp), PARAMETER :: wavenum2_lw(nbndlw) = (/ & ! upper wavenumber limit
    250._dp, 500._dp, 630._dp, 700._dp, 820._dp, 980._dp, &
    1080._dp, 1180._dp, 1390._dp, 1480._dp, 1800._dp, 2080._dp, &
    2250._dp, 2390._dp, 2680._dp, 3250._dp /)

  REAL(dp), PARAMETER :: z4piover3 = 4.0_dp*pi/3.0_dp
  REAL(dp), PARAMETER :: zeps = EPSILON(1.0_dp)

  REAL(dp), PARAMETER ::  lamdia_sw = 533.0E-9_dp, lamdia_lw = 9732.0E-9_dp

  !--------------------
  ! module variables
  !-------------------- 

  INTEGER :: idxdia_sw, idxdia_lw

  INTEGER, DIMENSION(:,:), ALLOCATABLE :: ix_tp, ix_rh    
 
  REAL(dp), DIMENSION(:,:,:,:), ALLOCATABLE ::  lite_sigma, lite_omega, lite_asym, lite_nr, lite_ni
  REAL(dp), DIMENSION(:,:,:), ALLOCATABLE ::    lut1_sigma, lut1_g, lut1_omega, lut1_pp180, &
                                                lut2_sigma, lut2_g, lut2_omega, lut2_pp180, &
                                                lut3_sigma, lut4_sigma
  REAL(dp), DIMENSION(:,:,:,:), ALLOCATABLE ::  sigma, omega, asym, nr, ni
  REAL(dp), DIMENSION(:,:,:), ALLOCATABLE :: znum ! tracer number [#/m2] 

  !$OMP THREADPRIVATE (ix_tp, ix_rh)
  !$OMP THREADPRIVATE (sigma, omega, asym, nr, ni)
  !$OMP THREADPRIVATE (znum) 

CONTAINS

!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_rad_init 
 
!    USE mo_mpi_lite,             ONLY: p_io, p_pe, p_bcast, p_comm_work
    USE mo_exception_lite,       ONLY: finish, message, message_text 
    USE mo_util_netcdf77_lite,   ONLY: read_var_nf77_3d 
    USE mo_util_string_lite,     ONLY: separator 
    USE mo_ham_lite_main,        ONLY: nrad 

    IMPLICIT NONE

    INTEGER :: jwv, iwv, jt, ierr
    LOGICAL :: lexist
    CHARACTER(len=256) :: cfile
 
    CALL message('','')
    CALL message('',separator)
    CALL message('ham_rad_initialize','parameter settings for radiation scheme')
    CALL message('','')
 
    CALL ham_lite_rad_data_init
 
    !--- check number of refractive indices

    IF ( Nwv_sw /= nbndsw .OR. Nwv_lw /= nbndlw)  &
      CALL finish('ham_rad_initialize','inconsistent number of wavelengths')

    IF (ANY(nrad(:)==1)) THEN
       IF ( SIZE(cnr,1)<Nwv_sw+Nwv_sw_opt .OR. SIZE(cni,1)<Nwv_sw+Nwv_sw_opt) THEN
          CALL finish('ham_rad_initialize', 'insufficient number of refractive indices defined for nrad = 1')
       END IF
    END IF
    IF (ANY(nrad(:)==2)) THEN
       IF ( SIZE(cnr,1)<Nwv_lw .OR. SIZE(cni,1)<Nwv_lw) THEN
          CALL finish('ham_rad_initialize', 'insufficient number of refractive indices defined for nrad = 2')
       END IF
    END IF
    IF (ANY(nrad(:)==3)) THEN
       IF ( SIZE(cnr,1)<Nwv_sw+Nwv_sw_opt+Nwv_lw .OR. SIZE(cni,1)<Nwv_sw+Nwv_sw_opt+Nwv_lw) THEN
          CALL finish('ham_rad_initialize', 'insufficient number of refractive indices defined for nrad = 3')
       END IF
    END IF

    !--- allocate memory for lookup tables 

    CALL ham_rad_mem

    !--- read in lookup tables 
    IF (ANY(nrad(:)==1) .OR. ANY(nrad(:)==3)) THEN

!      IF (p_pe == p_io) THEN

        cfile = 'lut_optical_properties_M7.nc'

        CALL message('ham_rad_initialize', 'reading lookup table from ' // TRIM(ADJUSTL(cfile)))
        INQUIRE (file=cfile, exist=lexist)

        IF (lexist) THEN
          CALL read_var_nf77_3d ( cfile, "nr", "ni", "dis", "sigma_1", lut1_sigma, ierr )
          CALL read_var_nf77_3d ( cfile, "nr", "ni", "dis", "sigma_2", lut2_sigma, ierr )
          CALL read_var_nf77_3d ( cfile, "nr", "ni", "dis", "omega_1", lut1_omega, ierr )
          CALL read_var_nf77_3d ( cfile, "nr", "ni", "dis", "omega_2", lut2_omega, ierr )
          CALL read_var_nf77_3d ( cfile, "nr", "ni", "dis", "asym_1",  lut1_g,     ierr )
          CALL read_var_nf77_3d ( cfile, "nr", "ni", "dis", "asym_2",  lut2_g,     ierr )
          CALL read_var_nf77_3d ( cfile, "nr", "ni", "dis", "pp180_1", lut1_pp180, ierr )
          CALL read_var_nf77_3d ( cfile, "nr", "ni", "dis", "pp180_2", lut2_pp180, ierr )
        ELSE
          CALL message('ham_rad_initialize','file ' // TRIM(ADJUSTL(cfile)) // ' not available')
        END IF

!      END IF

      !CALL p_bcast(lut1_sigma, p_io, p_comm_work)
      !CALL p_bcast(lut1_g    , p_io, p_comm_work)
      !CALL p_bcast(lut1_omega, p_io, p_comm_work)
      !CALL p_bcast(lut1_pp180, p_io, p_comm_work)
      !CALL p_bcast(lut2_sigma, p_io, p_comm_work)
      !CALL p_bcast(lut2_g    , p_io, p_comm_work)
      !CALL p_bcast(lut2_omega, p_io, p_comm_work)
      !CALL p_bcast(lut2_pp180, p_io, p_comm_work)

    END IF
 
    IF (ANY(nrad(:)==2) .OR. ANY(nrad(:)==3)) THEN

!      IF (p_pe == p_io) THEN

        cfile = 'lut_optical_properties_lw_M7.nc'

        CALL message('ham_rad_initialize', 'reading lookup table from ' // TRIM(ADJUSTL(cfile)))
        INQUIRE (file=cfile,exist=lexist)

        IF (lexist) THEN
          CALL read_var_nf77_3d ( cfile, "nr", "ni", "dis", "sigma_1_lw", lut3_sigma, ierr )
          CALL read_var_nf77_3d ( cfile, "nr", "ni", "dis", "sigma_2_lw", lut4_sigma, ierr )
        ELSE
          CALL message('ham_rad_initialize','file '//TRIM(ADJUSTL(cfile))//' not available')
        END IF

!      END IF

      !CALL p_bcast(lut3_sigma,p_io, p_comm_work)
      !CALL p_bcast(lut4_sigma,p_io, p_comm_work)

    END IF

    idxdia_sw = 1
    idxdia_lw = 1

    !--- initialize SW bands

    IF (ANY(nrad==1) .OR. ANY(nrad==3)) THEN 

      !---  interpolate wavelengths [m] from RRTM-SW wavenumbers [cm-1] 
      !     (hardcoded to RRTM-SW wavenumber indices)

      CALL message('', '')
      CALL message('', 'shortwave     wavelengths [nm]     limits [cm-1]')

      DO jwv = 1,Nwv_sw 
        !--- mid-wavelength 
        lambda(jwv) = (1.0_dp/wavenum1_sw(jwv) + 1.0_dp/wavenum2_sw(jwv)) / 2._dp * 0.01_dp
        WRITE(message_text,'(i3.3,10x,f10.1,5x,f10.1,5x,f10.1)') &
          jwv, lambda(jwv)*1.E9_dp, wavenum1_sw(jwv), wavenum2_sw(jwv) 
        CALL message('', message_text)
        IF ((lamdia_sw >= 0.01_dp/wavenum2_sw(jwv)) .AND.  & 
          (lamdia_sw < 0.01_dp/wavenum1_sw(jwv))) idxdia_sw = jwv
      END DO

      !--- add optional SW wavelengths
      CALL message('', '')
      CALL message('', 'shortwave     wavelengths (optional) [nm]')

      DO jwv=1, Nwv_sw_opt
        iwv=Nwv_sw + jwv
        lambda(iwv) = lambda_sw_opt(jwv)
        WRITE(message_text,'(i3.3,10x,f10.1)') iwv, lambda(iwv)*1.E9_dp
        CALL message('', message_text)
      END DO

    END IF

    !--- initialize LW bands

    IF (ANY(nrad==2) .OR. ANY(nrad==3)) THEN

      !---  interpolate wavelengths [m] from RRTM-LW wavenumbers [cm-1]:

      CALL message('', '')
      CALL message('', 'longwave     wavelengths [nm]     limits [cm-1]')

      DO jwv=1, Nwv_lw
        iwv = Nwv_sw+Nwv_sw_opt+jwv
        !--- mid-wavelength    
        lambda(iwv) = (1.0_dp/wavenum1_lw(jwv) + 1.0_dp/wavenum2_lw(jwv)) / 2._dp * 0.01_dp
        WRITE(message_text,'(i3.3,10x,f10.1,5x,f10.1,5x,f10.1)') &
          iwv, lambda(iwv)*1.E9_dp, wavenum1_lw(jwv), wavenum2_lw(jwv)
        CALL message('', message_text)
        IF ((lamdia_lw >= 0.01_dp/wavenum2_lw(jwv)) .AND.  &
          (lamdia_lw < 0.01_dp/wavenum1_lw(jwv))) idxdia_lw = jwv
      END DO
    END IF

    CALL message('', '')
    CALL message('', 'wavelengths for diagnostics [nm]')
    WRITE(message_text,'(i3.3,5x,f10.1)') idxdia_sw, lamdia_sw*1.E9_dp
    CALL message('', message_text)
    WRITE(message_text,'(i3.3,5x,f10.1)') Nwv_sw+Nwv_sw_opt+idxdia_lw, lamdia_lw*1.E9_dp
    CALL message('', message_text)

  END SUBROUTINE ham_lite_rad_init

!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_rad_mem
 
    USE mo_ham_lite_main,   ONLY: nrad

    IMPLICIT NONE

    IF (ANY(nrad(:)==1) .OR. ANY(nrad(:)==3))THEN

      ALLOCATE( lut1_sigma(0:Nnrmax(1), 0:Nnimax(1), 0:Ndismax(1)), &
                lut1_g    (0:Nnrmax(1), 0:Nnimax(1), 0:Ndismax(1)), &
                lut1_omega(0:Nnrmax(1), 0:Nnimax(1), 0:Ndismax(1)), &
                lut1_pp180(0:Nnrmax(1), 0:Nnimax(1), 0:Ndismax(1))  )

      ALLOCATE( lut2_sigma(0:Nnrmax(2), 0:Nnimax(2), 0:Ndismax(2)), &
                lut2_g    (0:Nnrmax(2), 0:Nnimax(2), 0:Ndismax(2)), &
                lut2_omega(0:Nnrmax(2), 0:Nnimax(2), 0:Ndismax(2)), &
                lut2_pp180(0:Nnrmax(2), 0:Nnimax(2), 0:Ndismax(2))  )

    END IF

    IF (ANY(nrad(:)==2) .OR. ANY(nrad(:)==3))THEN

      ALLOCATE( lut3_sigma(0:Nnrmax(3), 0:Nnimax(3), 0:Ndismax(3)), &
                lut4_sigma(0:Nnrmax(4), 0:Nnimax(4), 0:Ndismax(4))  )

    END IF

  END SUBROUTINE ham_rad_mem

!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_rad_calc
  
!    USE mo_mpi_lite,               ONLY: p_pe, p_io
    USE mo_exception_lite,         ONLY: finish   
    USE mo_ham_lite_main,          ONLY: nrad, sigma_fine, sigma_coarse, sigma_mode  
    USE mo_ham_lite_misc,          ONLY: lite_rwet
    USE mo_ham_lite_kappa,         ONLY: N_T,N_rh  

    IMPLICIT NONE
  
    INTEGER :: jtm, jrh, jclass, itable, &  
               jwv, jlwv ! RRTM SW and LW band numbers 
    REAL(dp) :: rdry, rwet, & ! dry and wet radii [m]
                znr, zni,   & ! real and imaginary part of refractive index
                zxx           ! size parameter  
    REAL(dp) :: zsigma, zomega, zasym            

    IF (.NOT. ALLOCATED(lite_sigma)) ALLOCATE(lite_sigma(N_T,N_rh,Nwv_tot,nclass))
    IF (.NOT. ALLOCATED(lite_omega)) ALLOCATE(lite_omega(N_T,N_rh,Nwv_sw_tot,nclass))
    IF (.NOT. ALLOCATED(lite_asym)) ALLOCATE(lite_asym(N_T,N_rh,Nwv_sw_tot,nclass))
    IF (.NOT. ALLOCATED(lite_nr)) ALLOCATE(lite_nr(N_T,N_rh,Nwv_tot,nclass))
    IF (.NOT. ALLOCATED(lite_ni)) ALLOCATE(lite_ni(N_T,N_rh,Nwv_tot,nclass))
 
    DO jclass = 1,nclass
      DO jwv = 1,Nwv_tot
        DO jrh = 1,N_rh
          DO jtm = 1,N_T
            lite_sigma(jtm,jrh,jwv,jclass) = 0._dp 
            lite_nr(jtm,jrh,jwv,jclass)    = 0._dp
            lite_ni(jtm,jrh,jwv,jclass)    = 0._dp 
          END DO
        END DO
      END DO
    END DO

    DO jclass = 1,nclass
      DO jwv = 1,Nwv_sw_tot
        DO jrh = 1,N_rh
          DO jtm = 1,N_T
            lite_omega(jtm,jrh,jwv,jclass) = 0._dp
            lite_asym(jtm,jrh,jwv,jclass)  = 0._dp 
          END DO
        END DO
      END DO
    END DO
 
    !--- 1) calculate optical properties for SW bands:
      
    IF (ANY(nrad(:)==1) .OR. ANY(nrad(:)==3)) THEN

      DO jclass = 1,nclass
        IF (nrad(jclass)==1 .OR. nrad(jclass)==3) THEN

          DO jwv = 1,Nwv_sw+Nwv_sw_opt
          
            ! loop over relative humidity and temperature 
            DO jrh = 1,N_rh
              DO jtm = 1,N_T
              
                ! get dry and wet radii [m]
                rdry = sizeclass(jclass)%lite_radi  
                rwet = lite_rwet(jtm,jrh,jclass)  

                !--- 1.1) calculate volume averaged refractive index nr and ni:
                CALL ham_rad_refrac( jclass, jwv, rdry, rwet, znr,  zni )
  
                !--- 1.2) calculate size parameter:
                zxx = 2.0_dp * pi * rwet / lambda(jwv)

                !--- 1.3) call look up table for optical properties:
                IF (ABS(sigma_mode(jclass) - sigma_fine) < zeps) THEN
                  itable=1
                ELSE IF  ((ABS(sigma_mode(jclass) - sigma_coarse) < zeps)) THEN
                  itable=2
                ELSE 
                  CALL finish('ham_rad','incompatible standard deviation in modal setup')
                END IF
   
                IF (itable == 1) THEN
                  CALL ham_rad_fitplus( zxx, znr, zni, itable,  &
                                        lut1_sigma, zsigma,     & 
                                        lut1_omega, zomega,     &
                                        lut1_g, zasym           )
                ELSE 
                  CALL ham_rad_fitplus( zxx, znr, zni, itable,  &
                                        lut2_sigma, zsigma,     &
                                        lut2_omega, zomega,     &
                                        lut2_g, zasym           )
                END IF
  
                lite_sigma(jtm,jrh,jwv,jclass)  = zsigma 
                lite_omega(jtm,jrh,jwv,jclass)  = zomega 
                lite_asym(jtm,jrh,jwv,jclass)   = zasym
                lite_nr(jtm,jrh,jwv,jclass)     = znr
                lite_ni(jtm,jrh,jwv,jclass)     = zni
    
                lite_sigma(jtm,jrh,jwv,jclass) = lite_sigma(jtm,jrh,jwv,jclass)*lambda(jwv)*lambda(jwv)
          
              END DO ! jtm
            END DO ! jrh

          END DO ! jwv
 
        END IF ! nrad
      END DO ! jclass

    END IF  

    !--- 2) calculate optical properties for LW bands:

    IF (ANY(nrad(:)==2) .OR. ANY(nrad(:)==3)) THEN

      DO jclass = 1,nclass
        IF (nrad(jclass)==2 .OR. nrad(jclass)==3) THEN

          DO jwv = 1,Nwv_lw

            jlwv = Nwv_sw+Nwv_sw_opt+jwv ! total SW + LW wavelengths  

            ! loop over relative humidity and temperature 
            DO jrh = 1,N_rh
              DO jtm = 1,N_T

                ! get dry and wet radii [m]
                rdry = sizeclass(jclass)%lite_radi   
                rwet = lite_rwet(jtm,jrh,jclass)  

                !--- 2.1) calculate volume averaged refractive index nr and ni:
                CALL ham_rad_refrac( jclass, jlwv, rdry, rwet, znr,  zni )
  
                !--- 2.2) calculate size parameter:
                zxx = 2.0_dp * pi * rwet / lambda(jlwv)

                !--- 2.3) Call look up table for optical properties:

                IF (ABS(sigma_mode(jclass) - sigma_fine) < zeps) THEN
                  itable=3
                ELSE IF ((ABS(sigma_mode(jclass) - sigma_coarse) < zeps)) THEN
                  itable=4
                ELSE 
                  CALL finish('ham_rad','incompatible standard deviation in modal setup')
                END IF
       
                IF (itable == 3) THEN
                  CALL ham_rad_fitplus( zxx, znr, zni,              &
                                        itable, lut3_sigma, zsigma  )
                ELSE
                  CALL ham_rad_fitplus( zxx, znr, zni,              &
                                        itable, lut4_sigma, zsigma  )
                END IF
  
                lite_sigma(jtm,jrh,jlwv,jclass) = zsigma
                lite_nr(jtm,jrh,jlwv,jclass)    = znr
                lite_ni(jtm,jrh,jlwv,jclass)    = zni
        
                lite_sigma(jtm,jrh,jlwv,jclass) = lite_sigma(jtm,jrh,jlwv,jclass)*lambda(jlwv)*lambda(jlwv)

              END DO ! jtm
            END DO ! jrh
              
          END DO !jlwv
 
        END IF !nrad
      END DO !nclass

    END IF

  END SUBROUTINE ham_lite_rad_calc

!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_rad_refrac( kmod, kwv, rdry, rwet, pnr, pni )
 
    USE mo_ham_lite_main,   ONLY: cmr2ram 

    IMPLICIT NONE
 
    INTEGER,  INTENT(IN)  :: kmod ! current mode
    INTEGER,  INTENT(IN)  :: kwv  ! current wavelength 
    REAL(dp), INTENT(IN)  :: rdry, rwet  ! dry and wet radius [m]
    REAL(dp), INTENT(OUT) :: pnr, pni ! real and imaginary parts of refractive index
 
    INTEGER :: jspec, ispec, nspec, ikey 
    REAL(dp) :: vdry, vwet, zv, zvsum, znrsum, znisum 

    zvsum = 0._dp 
    znrsum = 0._dp
    znisum = 0._dp

    vdry = z4piover3 * (cmr2ram(kmod) * rdry)**3.0_dp
    vwet = z4piover3 * (cmr2ram(kmod) * rwet)**3.0_dp

    IF (nrad(kmod) > 0) THEN

      ! add aerosol compounds
      nspec = sizeclass(kmod)%lite_nspe
      DO jspec = 1,nspec

          zv = sizeclass(kmod)%lite_vspe(jspec) * vdry
          
          ispec = sizeclass(kmod)%lite_ispe(jspec)
          ikey = speclist(ispec)%iaerorad

          znrsum = znrsum + cnr(kwv,ikey) * zv
          znisum = znisum + cni(kwv,ikey) * zv
          zvsum = zvsum + zv

      END DO

      ! add aerosol water
      IF (sizeclass(kmod)%lsoluble) THEN
              
        zv = vwet - vdry

        ikey = speclist(id_wat)%iaerorad

        znrsum = znrsum + cnr(kwv,ikey) * zv
        znisum = znisum + cni(kwv,ikey) * zv
        zvsum = zvsum + zv
    
      END IF

    END IF
 
    ! compute volume weighted average
    pnr = znrsum / zvsum 
    pni = znisum / zvsum 

  END SUBROUTINE ham_rad_refrac

!---------------------------------------------------------------------------------------------------
  
  SUBROUTINE ham_rad_fitplus( pxx, pnr, pni, ktable, plut1, pfit1, plut2, pfit2, plut3, pfit3  )
    
    IMPLICIT NONE
 
    INTEGER,  INTENT(IN)  :: ktable 
    REAL(dp), INTENT(IN)  :: pnr, pni, pxx
    REAL(dp), INTENT(IN)  :: plut1(0:Nnrmax(ktable), 0:Nnimax(ktable), 0:Ndismax(ktable))
    REAL(dp), INTENT(IN), OPTIONAL  :: plut2(0:Nnrmax(ktable), 0:Nnimax(ktable), 0:Ndismax(ktable))
    REAL(dp), INTENT(IN), OPTIONAL  :: plut3(0:Nnrmax(ktable), 0:Nnimax(ktable), 0:Ndismax(ktable))
    REAL(dp), INTENT(OUT) :: pfit1
    REAL(dp), INTENT(OUT), OPTIONAL :: pfit2
    REAL(dp), INTENT(OUT), OPTIONAL :: pfit3
  
    INTEGER :: Ndis, Nnr, Nni   
 
    IF (  (pxx >= x0_min(ktable)) .AND. (pxx <= x0_max(ktable)) .AND. &
          (pnr >= nr_min(ktable)) .AND. (pnr <= nr_max(ktable)) .AND. &
          (pni >= ni_min(ktable)) .AND. (pni <= ni_max(ktable))       ) THEN

      Ndis = NINT((LOG(pxx) - log_x0_min(ktable)) / (log_x0_max(ktable) - log_x0_min(ktable)) * REAL(Ndismax(ktable),dp))
      Ndis = MIN(Ndismax(ktable) - 1, MAX(0,Ndis))

      Nnr = NINT((pnr - nr_min(ktable)) / inc_nr(ktable))
      Nnr = MIN(Nnrmax(ktable) - 1, MAX(0,Nnr))

      Nni = NINT((LOG(pni) - log_ni_min(ktable)) / inc_ni(ktable))
      Nni = MIN(Nnimax(ktable) - 1, MAX(0,Nni))

      pfit1 = plut1(Nnr,Nni,Ndis)
                
      IF (PRESENT(plut2)) pfit2 = plut2(Nnr,Nni,Ndis)
      IF (PRESENT(plut3)) pfit3 = plut3(Nnr,Nni,Ndis)

    ELSE
                
      pfit1 = 0._dp

      IF (PRESENT(plut2)) pfit2 = 0._dp
      IF (PRESENT(plut3)) pfit3 = 0._dp

    END IF
 
  END SUBROUTINE ham_rad_fitplus

!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_rad ( jg, jb, jcs, jce, kproma, klev, ktrac, kbndlw, kbndsw,      &
                            pxm_trc, tk_fl, pp_fl, sh_fl,                               &
                            aer_tau_sw_vr, aer_ssa_sw_vr, aer_asy_sw_vr, aer_tau_lw_vr  )
  
    USE mo_ham_lite_main,     ONLY: nrad   
    USE mo_ham_lite_misc,     ONLY: lcheck_rad, ham_lite_relhum, ham_lite_rindex, &
                                    lite_rwet, ham_lite_check_i2d, ham_lite_check_r2d 
    !USE mo_ham_lite_streams,  ONLY: ham_field

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: jg, jb, jcs, jce, kproma, klev, ktrac, kbndlw, kbndsw
    REAL(dp), INTENT(IN)  ::  pxm_trc(kproma,klev,ktrac), & ! number [#/m2]
                              tk_fl(kproma,klev),         & ! temperature at full levels [K]
                              pp_fl(kproma,klev),         & ! pressure at full levels [Pa]
                              sh_fl(kproma,klev)            ! specific humidity at full levels [kg/kg]
    REAL(dp), INTENT(INOUT) ::  aer_tau_sw_vr(kproma,klev,kbndsw),  & ! SW aerosol optical depth
                                aer_ssa_sw_vr(kproma,klev,kbndsw),  & ! SW aerosol single scattering albedo
                                aer_asy_sw_vr(kproma,klev,kbndsw),  & ! SW aerosol asymmetry factor
                                aer_tau_lw_vr(kproma,klev,kbndlw)     ! LW aerosol optical depth 

    INTEGER  :: jclass, jl, jk, jwv, jlwv, itrac, ikl 
    REAL(dp) :: rh_fl(kproma,klev) ! relative humidity at full levels [kg/kg]
    REAL(dp) :: zaer_tau_sw_vr(kproma,klev,Nwv_sw_tot,nclass),  & ! SW optical depth for each band and mode
                zaer_tau_lw_vr(kproma,klev,Nwv_lw,nclass)         ! LW optical depth for each band and mode
    
    !--- 0) initialize:
 
    DO jclass = 1,nclass
      DO jwv = 1,Nwv_tot
        DO jk = 1,klev
          DO jl = jcs,jce  
            sigma(jl,jk,jwv,jclass) = 0._dp 
            nr(jl,jk,jwv,jclass)    = 0._dp
            ni(jl,jk,jwv,jclass)    = 0._dp 
          END DO 
        END DO 
      END DO 
    END DO

    DO jclass = 1,nclass
      DO jwv = 1,Nwv_sw_tot
        DO jk = 1,klev
          DO jl = jcs,jce   
            omega(jl,jk,jwv,jclass) = 0._dp
            asym(jl,jk,jwv,jclass)  = 0._dp 
            zaer_tau_sw_vr(jl,jk,jwv,jclass) = 0.0_dp
          END DO 
        END DO 
      END DO 
    END DO

    DO jclass = 1,nclass
      DO jwv = 1,Nwv_lw
        DO jk = 1,klev
          DO jl = jcs,jce    
            zaer_tau_lw_vr(jl,jk,jwv,jclass) = 0.0_dp
          END DO 
        END DO 
      END DO 
    END DO
 
    DO jk = 1,klev
      DO jl = jcs,jce
        ix_tp(jl,jk) = 0
        ix_rh(jl,jk) = 0
        rh_fl(jl,jk) = 0.0_dp
      END DO 
    END DO
 
    CALL ham_lite_relhum  ( jcs, jce, kproma, klev, sh_fl, tk_fl, pp_fl, rh_fl )
    CALL ham_lite_rindex  ( jcs, jce, kproma, klev, tk_fl, rh_fl, ix_tp, ix_rh )
 
    ! DO jl = jcs,jce
    !   ham_field(jg)%temp_2d(jl,jb) = tk_fl(jl,klev)
    !   ham_field(jg)%rhum_2d(jl,jb) = rh_fl(jl,klev)
    ! END DO 

    ! DO jclass = 1,nclass 
    !   DO jl = jcs,jce
    !     ham_field(jg)%rwet_2d(jclass)%p_2d(jl,jb) = lite_rwet(ix_tp(jl,klev),ix_rh(jl,klev),jclass)
    !   END DO 
    ! END DO 

    IF (lcheck_rad) THEN
      CALL ham_lite_check_r2d( jb, jcs, jce, 1, klev, kproma, sh_fl, 'ham_lite_rad', 'sh_fl' )
      CALL ham_lite_check_r2d( jb, jcs, jce, 1, klev, kproma, tk_fl, 'ham_lite_rad', 'tk_fl' )
      CALL ham_lite_check_r2d( jb, jcs, jce, 1, klev, kproma, pp_fl, 'ham_lite_rad', 'pp_fl' )
      CALL ham_lite_check_r2d( jb, jcs, jce, 1, klev, kproma, rh_fl, 'ham_lite_rad', 'rh_fl' )
      CALL ham_lite_check_i2d( jb, jcs, jce, 1, klev, kproma, ix_tp, 'ham_lite_rad', 'ix_tp' )
      CALL ham_lite_check_i2d( jb, jcs, jce, 1, klev, kproma, ix_rh, 'ham_lite_rad', 'ix_rh' )
    END IF
      
    DO jclass = 1, nclass
      itrac = sizeclass(jclass)%idt_no
      DO jk = 1,klev
        DO jl = jcs,jce
          znum(jl,jk,jclass) = pxm_trc(jl,jk,itrac)  
        END DO 
      END DO
    END DO

    !--- 1) calculate optical properties for SW bands: 
    IF (ANY(nrad(:)==1) .OR. ANY(nrad(:)==3)) THEN

      DO jclass=1, nclass
        IF (nrad(jclass)==1 .OR. nrad(jclass)==3) THEN 

          DO jwv=1,Nwv_sw+Nwv_sw_opt
              
            DO jk = 1,klev
              DO jl = jcs,jce 
                sigma (jl,jk,jwv,jclass)  = lite_sigma  (ix_tp(jl,jk),ix_rh(jl,jk),jwv,jclass)
                omega (jl,jk,jwv,jclass)  = lite_omega  (ix_tp(jl,jk),ix_rh(jl,jk),jwv,jclass)
                asym  (jl,jk,jwv,jclass)  = lite_asym   (ix_tp(jl,jk),ix_rh(jl,jk),jwv,jclass)
                nr    (jl,jk,jwv,jclass)  = lite_nr     (ix_tp(jl,jk),ix_rh(jl,jk),jwv,jclass)
                ni    (jl,jk,jwv,jclass)  = lite_ni     (ix_tp(jl,jk),ix_rh(jl,jk),jwv,jclass) 
              END DO
            END DO

          END DO  

        END IF  
      END DO  
      

      DO jclass=1, nclass
        IF (nrad(jclass)>0) THEN

          DO jwv = 1,Nwv_sw+Nwv_sw_opt

            DO jk = 1,klev
              DO jl = jcs,jce 
                zaer_tau_sw_vr(jl,jk,jwv,jclass) = znum(jl,jk,jclass) * sigma(jl,jk,jwv,jclass)
              END DO
            END DO

          END DO
        END IF
      END DO
  
      !--- calculate weighted properties and reorder into vertical levels of RRTM structure:

      DO jwv = 1,Nwv_sw

        DO jclass = 1,nclass
          DO jk = 1,klev
            ikl = jk! klev+1-jk
            DO jl = jcs,jce
              aer_tau_sw_vr(jl,jk,jwv) = aer_tau_sw_vr(jl,jk,jwv) + &
                zaer_tau_sw_vr(jl,ikl,jwv,jclass)
              aer_ssa_sw_vr(jl,jk,jwv) = aer_ssa_sw_vr(jl,jk,jwv) + &
                zaer_tau_sw_vr(jl,ikl,jwv,jclass) * omega(jl,ikl,jwv,jclass)
              aer_asy_sw_vr(jl,jk,jwv) = aer_asy_sw_vr(jl,jk,jwv) + &
                zaer_tau_sw_vr(jl,ikl,jwv,jclass) * omega(jl,ikl,jwv,jclass) * asym(jl,ikl,jwv,jclass)
            END DO
          END DO
        END DO

        DO jk = 1,klev
          DO jl = jcs,jce
            IF (aer_ssa_sw_vr(jl,jk,jwv) > zeps) THEN 
              aer_asy_sw_vr(jl,jk,jwv) = aer_asy_sw_vr(jl,jk,jwv) / aer_ssa_sw_vr(jl,jk,jwv)
            ELSE 
              aer_asy_sw_vr(jl,jk,jwv) = 0.0_dp
            END IF 
            IF (aer_tau_sw_vr(jl,jk,jwv) > zeps) THEN 
              aer_ssa_sw_vr(jl,jk,jwv) = aer_ssa_sw_vr(jl,jk,jwv) / aer_tau_sw_vr(jl,jk,jwv)
            ELSE
              aer_ssa_sw_vr(jl,jk,jwv) = 1.0_dp
            END IF  
          END DO
        END DO

      END DO 
      
    END IF

    !--- 2) calculate optical properties for LW bands: 
    IF (ANY(nrad(:)==2) .OR. ANY(nrad(:)==3)) THEN

      DO jclass=1, nclass
        IF (nrad(jclass)==2 .OR. nrad(jclass)==3) THEN
 
          DO jwv=1, Nwv_lw
            jlwv = Nwv_sw+Nwv_sw_opt+jwv ! total SW + LW wavelengths

            DO jk = 1,klev
              DO jl = jcs,jce
                sigma (jl,jk,jlwv,jclass) = lite_sigma  (ix_tp(jl,jk),ix_rh(jl,jk),jlwv,jclass)
                nr    (jl,jk,jlwv,jclass) = lite_nr     (ix_tp(jl,jk),ix_rh(jl,jk),jlwv,jclass)
                ni    (jl,jk,jlwv,jclass) = lite_ni     (ix_tp(jl,jk),ix_rh(jl,jk),jlwv,jclass)
              END DO
            END DO

          END DO  

        END IF  
      END DO  
          
      DO jclass=1, nclass
        IF(nrad(jclass)>0) THEN

          DO jwv=1, Nwv_lw
            jlwv = Nwv_sw+Nwv_sw_opt+jwv ! total SW + LW wavelengths

            !--- calculate weighted properties and reorder into vertical levels of RRTM structure:

            DO jk = 1,klev 
              DO jl = jcs,jce
                zaer_tau_lw_vr(jl,jk,jwv,jclass) = znum(jl,jk,jclass) * sigma(jl,jk,jlwv,jclass) 
              END DO
            END DO

            DO jk = 1,klev
              ikl =jk! klev+1-jk
              DO jl = jcs,jce
                aer_tau_lw_vr(jl,jk,jwv) = aer_tau_lw_vr(jl,jk,jwv) + zaer_tau_lw_vr(jl,ikl,jwv,jclass)
              END DO
            END DO

          END DO

        END IF
      END DO

    END IF

    !--- 3) remove negative values in optical properties:
    DO jwv = 1,kbndlw
      DO jk = 1,klev 
        DO jl = jcs,jce
          aer_tau_lw_vr(jl,jk,jwv) = MAX( aer_tau_lw_vr(jl,jk,jwv), 0.0_dp )
        END DO
      END DO 
    END DO
    DO jwv = 1,kbndsw
      DO jk = 1,klev 
        DO jl = jcs,jce
          aer_tau_sw_vr(jl,jk,jwv)  = MAX( aer_tau_sw_vr(jl,jk,jwv), 0.0_dp )
          aer_ssa_sw_vr(jl,jk,jwv)  = MAX( aer_ssa_sw_vr(jl,jk,jwv), 0.0_dp )
          aer_asy_sw_vr(jl,jk,jwv)  = MAX( aer_asy_sw_vr(jl,jk,jwv), 0.0_dp )
        END DO
      END DO 
    END DO 
    
  END SUBROUTINE ham_lite_rad

!---------------------------------------------------------------------------------------------------

  ! SUBROUTINE ham_lite_rad_diag( jg, jb, jcs, jce, kproma, klev, kbndlw, kbndsw,                 &
  !                               pdz, aer_tau_sw_vr, aer_ssa_sw_vr, aer_asy_sw_vr, aer_tau_lw_vr )
  
  !   USE mo_ham_lite_main,       ONLY: nrad, cmr2ram  
  !   !USE mo_ham_lite_streams,    ONLY: ham_rad_field   
  !   USE mo_ham_lite_rad_data,   ONLY: nraddiagwv, nradang

  !   IMPLICIT NONE

  !   INTEGER, INTENT(IN) ::  jg, jb, jcs, jce, kproma, klev, kbndlw, kbndsw 
  !   REAL(dp), INTENT(IN) :: pdz           (kproma,klev),        & ! geometric height thickness of layer [m] 
  !                           aer_tau_sw_vr (kproma,klev,kbndsw), & ! SW aerosol optical depth
  !                           aer_ssa_sw_vr (kproma,klev,kbndsw), & ! SW aerosol single scattering albedo
  !                           aer_asy_sw_vr (kproma,klev,kbndsw), & ! SW aerosol asymmetry factor
  !                           aer_tau_lw_vr (kproma,klev,kbndlw)    ! LW aerosol optical depth

  !   INTEGER ::  jk, jl, jclass, jspec, jwv   
  !   REAL(dp)  ::  zomega(kproma), zsigma(kproma), zasym(kproma),  &
  !                 znr(kproma), zni(kproma), ztau(kproma),         &
  !                 znr_2d(kproma), zni_2d(kproma),                 &
  !                 ztmp1(kproma), ztmp2(kproma), zint(kproma) 
  !   REAL(dp), POINTER ::  tau_p(:,:,:), tau_tot_p(:,:,:), tau_tot_2d_p(:,:),  &
  !                         abs_p(:,:,:), abs_tot_p(:,:,:), abs_tot_2d_p(:,:)
  !   LOGICAL ::  ltest  

  !   DO jwv = 1,Nwv_tot

  !     !--- 1) diagnose optical depth:

  !     IF (nraddiagwv(jwv) > 0) THEN

  !       tau_tot_p     => ham_rad_field(jg)%tau_tot(jwv)%p_3d
  !       tau_tot_2d_p  => ham_rad_field(jg)%tau_2d_tot(jwv)%p_2d

  !       DO jk = 1,klev
  !         DO jl = jcs,jce
  !           tau_tot_p(jl,jk,jb) = 0.0_dp
  !         END DO
  !       END DO

  !       DO jl = jcs,jce
  !         tau_tot_2d_p(jl,jb) = 0.0_dp
  !       END DO
        
  !       DO jclass = 1,nclass
  !         IF (nrad(jclass) > 0) THEN

  !           tau_p => ham_rad_field(jg)%tau_mode(jclass,jwv)%p_3d

  !           !--- per mode:
  !           DO jk = 1,klev
  !             DO jl = jcs,jce
  !               tau_p(jl,jk,jb) = znum(jl,jk,jclass) * sigma(jl,jk,jwv,jclass)
  !             END DO
  !           END DO

  !           !--- summed over all modes:
  !           DO jk = 1,klev
  !             DO jl = jcs,jce
  !               tau_tot_p(jl,jk,jb) = tau_tot_p(jl,jk,jb) + tau_p(jl,jk,jb)
  !             END DO
  !           END DO

  !           !--- summed over all modes and levels:
  !           DO jk = 1,klev
  !             DO jl = jcs,jce
  !               tau_tot_2d_p(jl,jb) = tau_tot_2d_p(jl,jb) + tau_p(jl,jk,jb)
  !             END DO
  !           END DO

  !         END IF
  !       END DO

  !     END IF  

  !     IF (nraddiagwv(jwv) > 1) THEN

  !       !--- 2) 2D extended diagnostics of mode radiative parameters:

  !       IF (nraddiag > 0) THEN

  !         DO jl = jcs,jce
  !           znr_2d(jl) = 0.0_dp
  !           zni_2d(jl) = 0.0_dp
  !         END DO 

  !         DO jclass = 1,nclass
  !           IF (nrad(jclass) > 0) THEN

  !             tau_p => ham_rad_field(jg)%tau_mode(jclass,jwv)%p_3d

  !             DO jl = jcs,jce
  !               ztau(jl)    = 0.0_dp
  !               zomega(jl)  = 0.0_dp
  !               zsigma(jl)  = 0.0_dp
  !               zasym(jl)   = 0.0_dp
  !               znr(jl)     = 0.0_dp
  !               zni(jl)     = 0.0_dp
  !             END DO

  !             DO jk = 1,klev
  !               DO jl = jcs,jce
  !                 ztau(jl)   = ztau(jl)   + tau_p(jl,jk,jb)
  !                 zomega(jl) = zomega(jl) + omega(jl,jk,jwv,jclass) * tau_p(jl,jk,jb)
  !                 zsigma(jl) = zsigma(jl) + sigma(jl,jk,jwv,jclass) * tau_p(jl,jk,jb)
  !                 zasym(jl)  = zasym(jl)  + asym(jl,jk,jwv,jclass)  * tau_p(jl,jk,jb)
  !                 znr(jl)    = znr(jl)    + nr(jl,jk,jwv,jclass)    * tau_p(jl,jk,jb)
  !                 zni(jl)    = zni(jl)    + ni(jl,jk,jwv,jclass)    * tau_p(jl,jk,jb)
  !                 znr_2d(jl) = znr_2d(jl) + nr(jl,jk,jwv,jclass)    * tau_p(jl,jk,jb)
  !                 zni_2d(jl) = zni_2d(jl) + ni(jl,jk,jwv,jclass)    * tau_p(jl,jk,jb)
  !               END DO
  !             END DO

  !             DO jl = jcs,jce
  !               ltest = (ztau(jl) > zeps)

  !               ztmp1(jl) = MERGE(ztau(jl), 1.0_dp, ltest) 
  !               ztmp1(jl) = 1._dp / ztmp1(jl)

  !               ham_rad_field(jg)%tau_2d_mode(jclass,jwv)%p_2d(jl,jb)   = MERGE(ztau(jl), 0._dp, ltest)
  !               ham_rad_field(jg)%omega_2d_mode(jclass,jwv)%p_2d(jl,jb) = MERGE(zomega(jl) * ztmp1(jl), 0._dp, ltest)
  !               ham_rad_field(jg)%sigma_2d_mode(jclass,jwv)%p_2d(jl,jb) = MERGE(zsigma(jl) * ztmp1(jl), 0._dp, ltest)
  !               ham_rad_field(jg)%asym_2d_mode(jclass,jwv)%p_2d(jl,jb)  = MERGE(zasym(jl) * ztmp1(jl), 0._dp, ltest)
  !               ham_rad_field(jg)%nr_2d_mode(jclass,jwv)%p_2d(jl,jb)    = MERGE(znr(jl) * ztmp1(jl), 0._dp, ltest)
  !               ham_rad_field(jg)%ni_2d_mode(jclass,jwv)%p_2d(jl,jb)    = MERGE(zni(jl) * ztmp1(jl), 0._dp, ltest)
  !             END DO

  !           END IF
  !         END DO

  !       END IF 

  !       !--- 3) 3D extended diagnostics of mode radiative parameters:

  !       IF (nraddiag == 2) THEN

  !         DO jclass=1, nclass
  !           IF( nrad(jclass) > 0 )THEN

  !             DO jk = 1,klev
  !               DO jl = jcs,jce
  !                 ham_rad_field(jg)%omega_mode(jclass,jwv)%p_3d(jl,jk,jb) = omega(jl,jk,jwv,jclass)
  !                 ham_rad_field(jg)%sigma_mode(jclass,jwv)%p_3d(jl,jk,jb) = sigma(jl,jk,jwv,jclass)
  !                 ham_rad_field(jg)%asym_mode(jclass,jwv)%p_3d(jl,jk,jb)  = asym(jl,jk,jwv,jclass)
  !                 ham_rad_field(jg)%nr_mode(jclass,jwv)%p_3d(jl,jk,jb)    = nr(jl,jk,jwv,jclass)
  !                 ham_rad_field(jg)%ni_mode(jclass,jwv)%p_3d(jl,jk,jb)    = ni(jl,jk,jwv,jclass)
  !               END DO
  !             END DO

  !           END IF
  !         END DO

  !       END IF

  !       !--- 4) diagnose absorption optical depth:

  !       abs_tot_p     => ham_rad_field(jg)%abs_tot(jwv)%p_3d
  !       abs_tot_2d_p  => ham_rad_field(jg)%abs_2d_tot(jwv)%p_2d

  !       DO jk = 1,klev
  !         DO jl = jcs,jce
  !           abs_tot_p(jl,jk,jb) = 0.0_dp
  !         END DO
  !       END DO

  !       DO jl = jcs,jce
  !         abs_tot_2d_p(jl,jb) = 0.0_dp
  !       END DO

  !       DO jclass = 1,nclass
  !         IF (nrad(jclass) > 0) THEN

  !           DO jl = jcs,jce
  !             ham_rad_field(jg)%abs_2d_mode(jclass,jwv)%p_2d(jl,jb) = 0.0_dp
  !           END DO

  !           abs_p => ham_rad_field(jg)%abs_mode(jclass,jwv)%p_3d
  !           tau_p => ham_rad_field(jg)%tau_mode(jclass,jwv)%p_3d

  !           !--- per each mode:
  !           DO jk = 1,klev
  !             DO jl = jcs,jce
  !               abs_p(jl,jk,jb) = (1.0_dp - omega(jl,jk,jwv,jclass)) * tau_p(jl,jk,jb)
  !             END DO
  !           END DO

  !           !--- summed over all modes:
  !           DO jk = 1,klev
  !             DO jl = jcs,jce
  !               abs_tot_p(jl,jk,jb) = abs_tot_p(jl,jk,jb) + abs_p(jl,jk,jb) 
  !             END DO
  !           END DO

  !           !--- summed over all modes and levels:
  !           DO jk = 1,klev
  !             DO jl = jcs,jce
  !               abs_tot_2d_p(jl,jb) = abs_tot_2d_p(jl,jb) + abs_p(jl,jk,jb) 
  !             END DO
  !           END DO

  !           DO jk = 1,klev
  !             DO jl = jcs,jce
  !               ham_rad_field(jg)%abs_2d_mode(jclass,jwv)%p_2d(jl,jb) = &
  !                 ham_rad_field(jg)%abs_2d_mode(jclass,jwv)%p_2d(jl,jb) + abs_p(jl,jk,jb) 
  !             END DO
  !           END DO

  !         END IF
  !       END DO
 
  !     END IF  

  !   END DO  

  !   !--- 5) diagnose angstroem parameter between two wavelengths:

  !   IF (nradang(1) /= 0 .AND. nradang(2) /= 0) THEN 

  !     DO jl = jcs,jce
  !       ltest = ( ham_rad_field(jg)%tau_2d_tot(nradang(1))%p_2d(jl,jb) > zeps)  &
  !         .AND. ( ham_rad_field(jg)%tau_2d_tot(nradang(2))%p_2d(jl,jb) > zeps)

  !       ztmp1(jl) = MERGE( ham_rad_field(jg)%tau_2d_tot(nradang(1))%p_2d(jl,jb), 1._dp, ltest) 
  !       ztmp2(jl) = MERGE( ham_rad_field(jg)%tau_2d_tot(nradang(2))%p_2d(jl,jb), 1._dp, ltest) 

  !       ham_rad_field(jg)%ang(jl,jb) = MERGE( &
  !         LOG(ztmp2(jl)/ztmp1(jl)) / LOG(lambda(nradang(1))/lambda(nradang(2))), &
  !         ham_rad_field(jg)%ang(jl,jb), ltest)
  !     END DO

  !   END IF

  !   !--- 6) diagnose aerosol optical depth, single scattering albedo, and asymmetry factor:

  !   DO jk = 1,klev
  !     DO jl = jcs,jce
  !       zint(jl) = 0._dp
  !       ham_rad_field(jg)%aod_533nm(jl,jb)  = 0._dp
  !       ham_rad_field(jg)%aod_9732nm(jl,jb) = 0._dp
  !       ham_rad_field(jg)%ssa_533nm(jl,jb)  = 0._dp
  !       ham_rad_field(jg)%asy_533nm(jl,jb)  = 0._dp
  !     END DO
  !   END DO

  !   DO jk = 1,klev
  !     DO jl = jcs,jce
  !       zint(jl) = zint(jl) + pdz(jl,jk)
  !     END DO
  !   END DO

  !   DO jk = 1,klev
  !     DO jl = jcs,jce
  !       ham_rad_field(jg)%aod_533nm(jl,jb) =  & 
  !         ham_rad_field(jg)%aod_533nm(jl,jb) + aer_tau_sw_vr(jl,jk,idxdia_sw)

  !       ham_rad_field(jg)%aod_9732nm(jl,jb) = &
  !         ham_rad_field(jg)%aod_9732nm(jl,jb) + aer_tau_lw_vr(jl,jk,idxdia_lw)

  !       ham_rad_field(jg)%ssa_533nm(jl,jb) =  &
  !         ham_rad_field(jg)%ssa_533nm(jl,jb) + aer_ssa_sw_vr(jl,jk,idxdia_sw) * pdz(jl,jk) / zint(jl)

  !       ham_rad_field(jg)%asy_533nm(jl,jb) =  & 
  !         ham_rad_field(jg)%asy_533nm(jl,jb) + aer_asy_sw_vr(jl,jk,idxdia_sw) * pdz(jl,jk) / zint(jl)
  !     END DO
  !   END DO

  ! END SUBROUTINE ham_lite_rad_diag
 
!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_rad_cache( kproma, klev )
 
    IMPLICIT NONE

    INTEGER, INTENT(in) :: kproma, klev

    ALLOCATE  (sigma(kproma,klev,Nwv_tot,nclass))
    ALLOCATE  (omega(kproma,klev,Nwv_sw_tot,nclass))
    ALLOCATE  (asym(kproma,klev,Nwv_sw_tot,nclass))
    ALLOCATE  (nr(kproma,klev,Nwv_tot,nclass))
    ALLOCATE  (ni(kproma,klev,Nwv_tot,nclass))
    ALLOCATE  (znum(kproma,klev,nclass))
    ALLOCATE  (ix_tp(kproma,klev))
    ALLOCATE  (ix_rh(kproma,klev)) 

  END SUBROUTINE ham_lite_rad_cache

!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_rad_cache_cleanup
 
    IMPLICIT NONE

    DEALLOCATE (  sigma, omega, asym, nr, ni, znum, ix_tp, ix_rh  )

  END SUBROUTINE ham_lite_rad_cache_cleanup

!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_rad_gp2g( kbndlw, kbndsw, kband_lw, kband_sw, zband_lw, zband_sw )
 
    IMPLICIT NONE

    INTEGER, INTENT(IN) ::  kbndlw, & ! number of LW bands
                            kbndsw    ! number of SW bands  
    REAL(dp), INTENT(IN)::  kband_lw (2,kbndlw),  &   ! limits of longwave bands [1/cm] - RRTMGP 
                            kband_sw (2,kbndsw)       ! limits of shortwave bands [1/cm] - RRTMGP
    REAL(dp), INTENT(INOUT):: zband_lw (2,kbndlw),  &   ! limits of longwave bands [1/cm] - RRTMG
                              zband_sw (2,kbndsw)       ! limits of shortwave bands [1/cm] - RRTMG

    INTEGER :: jk, jl 

    DO jl = 1,2
      
      DO jk = 1,kbndlw-1
        zband_lw(jl,jk) = kband_lw(jl,jk+1) 
      END DO 
      zband_lw(jl,kbndlw) = kband_lw(jl,1)  

      DO jk = 1,kbndsw-1
        zband_sw(jl,jk) = kband_sw(jl,jk+1) 
      END DO 
      zband_sw(jl,kbndsw) = kband_sw(jl,1)  
      
    END DO

  END SUBROUTINE ham_lite_rad_gp2g

!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_rad_check(  jb, jcs, jce, kproma, klev, kbndlw, kbndsw,                 &
                                  zband_lw, zband_sw,                                         &
                                  aer_tau_sw_vr, aer_ssa_sw_vr, aer_asy_sw_vr, aer_tau_lw_vr  )

!    USE mo_mpi_lite,             ONLY: p_pe, p_io
    USE mo_ham_lite_misc,        ONLY: ham_lite_stat_r2d

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: jb, jcs, jce, kproma, klev, kbndlw, kbndsw
    REAL(dp), INTENT(IN)::  zband_lw(2,kbndlw), zband_sw(2,kbndsw)       
    REAL(dp), INTENT(IN) :: aer_tau_sw_vr(kproma,klev,kbndsw),   &  
                            aer_ssa_sw_vr(kproma,klev,kbndsw),   &  
                            aer_asy_sw_vr(kproma,klev,kbndsw),   &  
                            aer_tau_lw_vr(kproma,klev,kbndlw)       

    INTEGER :: jwv
    REAL(dp)  :: zmin, zmax, zmean
  
!    IF ((p_pe == p_io) .AND. (jb == 1)) THEN 

      write(*,*) 'shortwave band' 
      DO jwv = 1,kbndsw
        write(*,'(a,5x,i3,5x,e10.3,5x,e10.3)') 'sw', jwv, zband_sw(1,jwv), zband_sw(2,jwv) 
        CALL ham_lite_stat_r2d( jcs, jce, 1, klev, kproma, aer_tau_sw_vr(:,:,jwv), zmin, zmax, zmean ) 
        write(*,'(a,5x,e12.4,5x,e12.4,5x,e12.4)') 'aer_tau_sw_vr (min, max, mean)', zmin, zmax, zmean 
        CALL ham_lite_stat_r2d( jcs, jce, 1, klev, kproma, aer_ssa_sw_vr(:,:,jwv), zmin, zmax, zmean ) 
        write(*,'(a,5x,e12.4,5x,e12.4,5x,e12.4)') 'aer_ssa_sw_vr (min, max, mean)', zmin, zmax, zmean    
        CALL ham_lite_stat_r2d( jcs, jce, 1, klev, kproma, aer_asy_sw_vr(:,:,jwv), zmin, zmax, zmean ) 
        write(*,'(a,5x,e12.4,5x,e12.4,5x,e12.4)') 'aer_asy_sw_vr (min, max, mean)', zmin, zmax, zmean
      END DO 

      write(*,*) 'longwave band' 
      DO jwv = 1,kbndlw
        write(*,'(a,5x,i3,5x,e10.3,5x,e10.3)') 'lw', jwv, zband_lw(1,jwv), zband_lw(2,jwv) 
        CALL ham_lite_stat_r2d( jcs, jce, 1, klev, kproma, aer_tau_lw_vr(:,:,jwv), zmin, zmax, zmean ) 
        write(*,'(a,5x,e12.4,5x,e12.4,5x,e12.4)') 'aer_tau_lw_vr (min, max, mean)', zmin, zmax, zmean
      END DO 

!    END IF 

  END SUBROUTINE ham_lite_rad_check
 
END MODULE mo_ham_lite_rad
