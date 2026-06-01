! 
! based on mo_ham_init.f90 and mo_cloud_micro_2m.f90
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

MODULE mo_ham_lite_misc

  USE mo_kind_lite,            ONLY: dp
  USE mo_math_constants_lite,  ONLY: pi
  USE mo_exception_lite,       ONLY: finish
  USE mo_ham_lite_main,        ONLY: sizeclass, nclass  

  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: ham_lite_relhum,    &  
!            ham_lite_satpre,    &
            ham_lite_rindex,    &  
            ham_lite_mprops,    &
            ham_lite_dyvisc,    &
            ham_lite_mfpath,    &
            ham_lite_check_i1d, &
            ham_lite_check_i2d, &
            ham_lite_check_r1d, &
            ham_lite_check_r2d, &
            ham_lite_check_wci, &
            ham_lite_check_wcc, & 
            ham_lite_stat_r2d,  &
            ham_lite_stat_cld,  &
            ham_lite_tail 
         
  PUBLIC :: lite_rwet,          &
            pbase,              &
            pnsum,              &
            pnact,              &
            pfrac

  PUBLIC :: pm6rp,prhop,prelhum !eehol: rwet and aerosol density
            
  PUBLIC :: lemimod,            &
            lcheck_lite,        & 
            lcheck_init,        & 
            lcheck_wdep,        &
            lcheck_ddep,        &
            lcheck_dust,        &
            lcheck_emi,         &
            lcheck_sed,         &
            lcheck_rad,         &
            lcheck_bdn 

  !--------------------
  ! module parameters
  !--------------------
   
  REAL(dp), PARAMETER :: z4piover3 = 4._dp*pi/3._dp

  LOGICAL, PARAMETER  ::      &
    lcheck_lite = .TRUE.,    &
    lcheck_init = .TRUE.,    &
    lcheck_wdep = .FALSE.,     &
    lcheck_ddep = .FALSE.,    & 
    lcheck_dust = .FALSE.,    & 
    lcheck_emi = .FALSE.,     &
    lcheck_sed = .FALSE.,     &
    lcheck_rad = .FALSE.,     &
    lcheck_bdn = .FALSE.

  LOGICAL, PARAMETER  :: lemimod = .TRUE.

  !--------------------
  ! module variables
  !--------------------

  INTEGER, DIMENSION(:,:), ALLOCATABLE :: pbase         ! index of cloud base

  REAL(dp), DIMENSION(:,:,:), ALLOCATABLE :: lite_rwet  ! wet radius [m]
  REAL(dp), DIMENSION(:,:),   ALLOCATABLE :: pnsum      ! number of newly activated cloud droplets [m-3]
  REAL(dp), DIMENSION(:,:,:), ALLOCATABLE :: pnact      ! number of newly activated cloud droplets per mode [m-3]
  REAL(dp), DIMENSION(:,:,:), ALLOCATABLE :: pfrac      ! fraction of activated particles per mode 
  REAL(dp), DIMENSION(:,:,:), ALLOCATABLE :: pm6rp      ! aerosol wet radius per mode [m]
  REAL(dp), DIMENSION(:,:,:), ALLOCATABLE :: prhop      ! mean mode particle density [kg m-3]
  REAL(dp), DIMENSION(:,:),   ALLOCATABLE :: prelhum    ! relative humidity
  
  
  !$OMP THREADPRIVATE (pbase)
  !$OMP THREADPRIVATE (pnsum)
  !$OMP THREADPRIVATE (pnact)
  !$OMP THREADPRIVATE (pfrac) 

CONTAINS

!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE ham_lite_relhum( jcs, jce, kproma, klev, zq_tr, tk_fl, pp_fl, rh_fl )
 
    USE mo_physical_constants_lite,  ONLY: vtmpc1
    USE mo_aes_convect_tables_lite,  ONLY: tlucuaw, jptlucu1, jptlucu2

    INTEGER, INTENT(IN)     ::  jcs, jce, kproma, klev 
    REAL(dp), INTENT(IN)    ::  zq_tr(kproma,klev),   & 
                                tk_fl(kproma,klev),   &
                                pp_fl(kproma,klev)  
    REAL(dp), INTENT(INOUT) ::  rh_fl(kproma,klev)

    INTEGER :: jk, jl, it
    REAL(dp) :: zqs, zqa, zac

    DO jk = 1,klev
      DO jl = jcs,jce
        it  = NINT(tk_fl(jl,jk) * 1000._dp)
        it  = MAX(MIN(it, jptlucu2), jptlucu1)
        
        zqs = tlucuaw(it) / pp_fl(jl,jk)
        zqs = MIN(zqs, 0.5_dp)
        zqs = zqs/(1._dp - vtmpc1*zqs)
         
        rh_fl(jl,jk) = zq_tr(jl,jk) / zqs
        rh_fl(jl,jk) = MAX(0.0_dp, MIN(rh_fl(jl,jk), 1.0_dp))
      ENDDO
    ENDDO

  END SUBROUTINE ham_lite_relhum
 
!---------------------------------------------------------------------------------------------------
 
  ! SUBROUTINE ham_lite_satpre( jb, jcs, jce, jks, klev, kproma, ptm1, pesw )
  
  !   USE mo_satad,             ONLY: sat_pres_water  

  !   INTEGER, INTENT(IN)     ::  jb, jcs, jce, jks, klev, kproma
  !   REAL(dp), INTENT(IN)    ::  ptm1  (kproma,klev) 
  !   REAL(dp), INTENT(INOUT) ::  pesw  (kproma,klev) ! saturation vapor pressure of water [Pa]
 
  !   INTEGER :: jk, jl  

  !   DO jk = 1,klev
  !     DO jl = jcs,jce
  !       pesw(jl,jk) = sat_pres_water(ptm1(jl,jk))
  !     END DO
  !   END DO  

  !   ! print checks 
  !   IF (lcheck_wdep) THEN
  !     CALL ham_lite_check_r2d( jb, jcs, jce, jks, klev, kproma, pesw, 'ham_lite_satpre', 'pesw' )
  !   END IF

  ! END SUBROUTINE ham_lite_satpre

!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE ham_lite_rindex( jcs, jce, kproma, klev, tk_fl, rh_fl, ix_tp, ix_rh )
   
    !USE mo_ham_lite_streams,    ONLY: ham_field  
    USE mo_ham_lite_kappa,      ONLY: T_min, T_max, rh_min, rh_max, N_T, N_rh, &
                                      rh_step_compress, log_rh_step_compress, inv_rh_init_step

    IMPLICIT NONE
 
    INTEGER, INTENT(IN)     ::  jcs, jce, kproma, klev
    REAL(dp), INTENT(IN)    ::  tk_fl(kproma,klev),   &   ! temperature at full levels [K]
                                rh_fl(kproma,klev)        ! relative humidity at full levels [Pa]
    INTEGER, INTENT(INOUT)  ::  ix_tp(kproma,klev),   &   ! index to temperature in look up table
                                ix_rh(kproma,klev)        ! index to relative humidity in look up table

    INTEGER   :: jk, jl  
    REAL(dp)  :: tmp, rhm  

    DO jk = 1,klev
      DO jl = jcs,jce
        tmp = MAX(tk_fl(jl,jk), T_min)
        tmp = MIN(tmp, T_max)
        ix_tp(jl,jk) = 1 + NINT(REAL((N_T - 1),dp) * (tmp - T_min) / (T_max - T_min))  
        ix_tp(jl,jk) = MAX(ix_tp(jl,jk), 1)
        ix_tp(jl,jk) = MIN(ix_tp(jl,jk), N_T)

        rhm = LOG(1._dp - (1._dp - rh_step_compress) * (rh_fl(jl,jk) - rh_min) * inv_rh_init_step)
        ix_rh(jl,jk) = CEILING(rhm / log_rh_step_compress)
        ix_rh(jl,jk) = MAX(ix_rh(jl,jk), 1)
        ix_rh(jl,jk) = MIN(ix_rh(jl,jk), N_rh)
      END DO
     END DO    

  END SUBROUTINE ham_lite_rindex

!---------------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_mprops( jg, jb, jcs, jce, kproma, klev, ptm1, papm1, pqm1 )
    
    !USE mo_model_domain,      ONLY: t_patch   
    !USE mo_exception,         ONLY: finish, message, message_text    
    USE mo_subm_spec_lite,         ONLY: speclist 
    !USE mo_ham_lite_streams,  ONLY: ham_field  
    USE mo_ham_lite_main,          ONLY: cmr2ram    
    USE mo_ham_lite_spec,          ONLY: id_wat     
    
    IMPLICIT NONE 
     
    INTEGER, INTENT(IN) ::  jg,       & ! grid index 
                            jb,       & ! index of block loop
                            jcs, jce, & ! start and end indices of columns loop  
                            kproma,   & ! maximum number of columns 
                            klev        ! number of levels  
    REAL(dp), INTENT(IN) :: ptm1  (kproma,klev), & ! temperature [K]    
                            papm1 (kproma,klev), & ! pressure at full levels [Pa] 
                            pqm1  (kproma,klev)    ! specific humidity [kg/kg]     

    INTEGER ::  jk, jl, jmode
    INTEGER ::  ix_tp(kproma,klev), &  ! index to temperature in look up table
                ix_rh(kproma,klev)     ! index to relative humidity in look up table
    REAL(dp) :: zrdry, zrwet, zvdry, zvwet, zmwat, zmdry 
    REAL(dp) :: zrh(kproma,klev)       ! relative humidity [0,1]   
  
    CALL ham_lite_relhum  ( jcs, jce, kproma, klev, pqm1, ptm1, papm1, zrh )
    CALL ham_lite_rindex  ( jcs, jce, kproma, klev, ptm1, zrh, ix_tp, ix_rh )

    ! DO jk = 1,klev
    !   DO jl = jcs,jce 
    !     ham_field(jg)%rhum(jl,jk,jb) = zrh(jl,jk)
    !   END DO
    ! END DO

    DO jmode = 1,nclass
      DO jk = 1,klev
        DO jl = jcs,jce
       
          zrdry = sizeclass(jmode)%lite_radi
          zrwet = lite_rwet(ix_tp(jl,jk),ix_rh(jl,jk),jmode)

          zvdry = z4piover3 * (cmr2ram(jmode) * zrdry)**3
          zvwet = z4piover3 * (cmr2ram(jmode) * zrwet)**3
          
          zmwat = speclist(id_wat)%density * (zvwet - zvdry)
          zmdry = sizeclass(jmode)%lite_dens * zvdry

          pm6rp(jl,jk,jmode) = zrwet !eehol: add rwet to public
          prhop(jl,jk,jmode) = (zmdry + zmwat) / zvwet !eehol: add densaer to public
          prelhum(jl,jk) = zrh(jl,jk) !eehol: add relhum to public

          ! wet count median radius [m]   
          !ham_field(jg)%rwet(jmode)%p_3d(jl,jk,jb) = zrwet

          ! mean mode density [kg/m3]  
          !ham_field(jg)%daer(jmode)%p_3d(jl,jk,jb) = (zmdry + zmwat) / zvwet 

        END DO
      END DO
    END DO

    ! print checks 
    IF (lcheck_lite) THEN    

      CALL ham_lite_check_r2d( jb, jcs, jce, 1, klev, kproma, pqm1, 'ham_lite_mprops', 'pqm1' )
      CALL ham_lite_check_r2d( jb, jcs, jce, 1, klev, kproma, ptm1, 'ham_lite_mprops', 'ptm1' )
      CALL ham_lite_check_r2d( jb, jcs, jce, 1, klev, kproma, papm1, 'ham_lite_mprops', 'papm1' )
      CALL ham_lite_check_r2d( jb, jcs, jce, 1, klev, kproma, zrh, 'ham_lite_mprops', 'zrh' ) 

      DO jmode = 1,nclass
        CALL ham_lite_check_wcc( jb, sizeclass(jmode)%shortname, 'mode' )
        !CALL ham_lite_check_r2d( jb, jcs, jce, 1, klev, kproma,             & 
        !  ham_field(jg)%daer(jmode)%p_3d(:,:,jb), 'ham_lite_mprops', 'daer' ) 
      END DO     

    END IF
 
  END SUBROUTINE ham_lite_mprops

!---------------------------------------------------------------------------------------------------

  ELEMENTAL REAL(dp) FUNCTION ham_lite_dyvisc(ta)
    !
    ! calculate dynamic viscosity of air [kg m-1 s-1]
    ! Sutherland's formula for ideal gas with reference temperature of 291.15 K
    ! 
    !$ACC ROUTINE SEQ
    USE mo_kind_lite,                ONLY: dp
    USE mo_physical_constants_lite,  ONLY: tmelt ! melting temperature of ice/snow

    IMPLICIT NONE

    REAL(dp), INTENT(in) :: ta   ! ambient temperature [K]
    REAL(dp), PARAMETER :: &
         c = 120.d0      , &     ! Sutherland's constant (for air) [K]
         t0 = 291.15d0   , &     ! reference temperature [K]
         eta0 = 1.827d-5         ! reference dynamic viscosity [kg m-1 s-1]
    REAL(dp) :: a, b
    REAL(dp) :: ztmpc, ztmp1 
    
    a = t0 + c
    b = ta + c
    ham_lite_dyvisc = eta0 * a/b * (ta/t0)**(3.d0/2.d0)
 
    RETURN
  END FUNCTION ham_lite_dyvisc

!---------------------------------------------------------------------------------------------------

  ELEMENTAL REAL(dp) FUNCTION ham_lite_mfpath(ta, pa)
    !
    ! calculate mean free path of air [m]
    ! Seinfeld and Pandis, John Wiley & Sons, 2006
    ! 
    !$ACC ROUTINE SEQ
    USE mo_kind_lite,  ONLY: dp 

    IMPLICIT NONE

    REAL(dp), INTENT(in) :: ta   ! ambient temperature [K]
    REAL(dp), INTENT(in) :: pa   ! ambient pressure [Pa]    

    REAL(dp), PARAMETER :: &
      pref = 101325.0_dp, &  ! reference pressure [Pa]
      tref = 293.15_dp       ! reference temperature [K]
      
    ham_lite_mfpath = 0.066_dp * (pref / pa) * (ta / tref) * 1.E-06_dp
 
    RETURN
  END FUNCTION ham_lite_mfpath

!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE ham_lite_check_i1d( jb, jcs, jce, kproma, pcheck, sname, tname )

    USE mo_kind_lite,      ONLY: dp 
    USE mo_mpi_lite,       ONLY: p_pe, p_io

    INTEGER, INTENT(IN)           ::  jb, jcs, jce, kproma, pcheck(kproma)
    CHARACTER(LEN=*), INTENT(IN)  ::  sname, tname 
    
    INTEGER :: inum, jl, tmin, tmax, tsum, tmean  
  
    IF ((p_pe == p_io) .AND. (jb == 1)) THEN

      tmin = +8888888  
      DO jl = jcs,jce
        tmin = MIN(tmin,pcheck(jl))
      END DO

      tmax = -8888888 
      DO jl = jcs,jce
        tmax = MAX(tmax,pcheck(jl))
      END DO 

      inum = 0
      tsum = 0 
      DO jl = jcs,jce
        inum = inum + 1
        tsum = tsum + pcheck(jl)  
      END DO 
      tmean = tsum/inum 

      write(*,*) sname, '   ', tname 
      write(*,'(a,5x,i10,i10,i10)') 'min, max, mean', tmin, tmax, tmean

    END IF

  END SUBROUTINE ham_lite_check_i1d

!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE ham_lite_check_i2d( jb, jcs, jce, jks, klev, kproma, pcheck, sname, tname )

    USE mo_kind_lite,      ONLY: dp 
    USE mo_mpi_lite,       ONLY: p_pe, p_io

    INTEGER, INTENT(IN)           ::  jb, jcs, jce, jks, klev, kproma, pcheck(kproma,klev)
    CHARACTER(LEN=*), INTENT(IN)  ::  sname, tname 
    
    INTEGER :: inum, jk, jl, tmin, tmax, tsum, tmean 

    IF ((p_pe == p_io) .AND. (jb == 1)) THEN

      tmin = +8888888  
      DO jk = jks,klev
        DO jl = jcs,jce
          tmin = MIN(tmin,pcheck(jl,jk))
        END DO
      END DO

      tmax = -8888888 
      DO jk = jks,klev
        DO jl = jcs,jce
          tmax = MAX(tmax,pcheck(jl,jk))
        END DO
      END DO 

      inum = 0
      tsum = 0 
      DO jk = jks,klev
        DO jl = jcs,jce
          inum = inum + 1
          tsum = tsum + pcheck(jl,jk) 
        END DO
      END DO 
      tmean = tsum/REAL(inum) 

      write(*,*) sname, '   ', tname 
      write(*,'(a,5x,i10,i10,i10)') 'min, max, mean', tmin, tmax, tmean

    END IF

  END SUBROUTINE ham_lite_check_i2d

!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE ham_lite_check_r1d( jb, jcs, jce, kproma, pcheck, sname, tname )

    USE mo_kind_lite,      ONLY: dp 
    USE mo_mpi_lite,       ONLY: p_pe, p_io

    INTEGER, INTENT(IN)           ::  jb, jcs, jce, kproma
    REAL(dp), INTENT(IN)          ::  pcheck(kproma)
    CHARACTER(LEN=*), INTENT(IN)  ::  sname, tname 
    
    INTEGER :: inum, jl
    REAL(dp) :: tmin, tmax, tsum, tmean 
  
    IF ((p_pe == p_io) .AND. (jb == 1)) THEN

      tmin = +99.0E+20_dp 
      DO jl = jcs,jce
        tmin = MIN(tmin,pcheck(jl))
      END DO

      tmax = -99.0E+20_dp
      DO jl = jcs,jce
        tmax = MAX(tmax,pcheck(jl))
      END DO 

      inum = 0
      tsum = 0 
      DO jl = jcs,jce
        inum = inum + 1
        tsum = tsum + pcheck(jl)  
      END DO 
      tmean = tsum/REAL(inum) 

      write(*,*) sname, '   ', tname 
      write(*,'(a,5x,e12.4,e12.4,e12.4)') 'min, max, mean', tmin, tmax, tmean

    END IF

  END SUBROUTINE ham_lite_check_r1d
 
!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE ham_lite_check_r2d( jb, jcs, jce, jks, klev, kproma, pcheck, sname, tname )

    USE mo_kind_lite,      ONLY: dp 
    USE mo_mpi_lite,       ONLY: p_pe, p_io

    INTEGER, INTENT(IN)           ::  jb, jcs, jce, jks, klev, kproma    
    REAL(dp), INTENT(IN)          ::  pcheck(kproma,klev)
    CHARACTER(LEN=*), INTENT(IN)  ::  sname, tname 
    
    INTEGER :: inum, jk, jl
    REAL(dp) :: tmin, tmax, tsum, tmean 

    IF ((p_pe == p_io) .AND. (jb == 1)) THEN

      tmin = +99.0E+20_dp
      DO jk = jks,klev
        DO jl = jcs,jce
          tmin = MIN(tmin,pcheck(jl,jk))
        END DO
      END DO

      tmax = -99.0E+20_dp
      DO jk = jks,klev
        DO jl = jcs,jce
          tmax = MAX(tmax,pcheck(jl,jk))
        END DO
      END DO 

      inum = 0
      tsum = 0.0_dp
      DO jk = jks,klev
        DO jl = jcs,jce
          inum = inum + 1
          tsum = tsum + pcheck(jl,jk) 
        END DO
      END DO 
      tmean = tsum/REAL(inum) 

      write(*,*) sname, '   ', tname 
      write(*,'(a,5x,e12.4,e12.4,e12.4)') 'min, max, mean', tmin, tmax, tmean

    END IF

  END SUBROUTINE ham_lite_check_r2d

!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE ham_lite_check_wci( jb, inumb, sname )
 
    USE mo_mpi_lite,       ONLY: p_pe, p_io

    INTEGER, INTENT(IN)           ::  jb, inumb
    CHARACTER(LEN=*), INTENT(IN)  ::  sname 

    IF ((p_pe == p_io) .AND. (jb == 1)) write(*,*) sname, '   ', inumb
 
  END SUBROUTINE ham_lite_check_wci

!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE ham_lite_check_wcc( jb, iname, sname )
 
    USE mo_mpi_lite,       ONLY: p_pe, p_io

    INTEGER, INTENT(IN)           ::  jb 
    CHARACTER(LEN=*), INTENT(IN)  ::  sname, iname 

    IF ((p_pe == p_io) .AND. (jb == 1)) write(*,*) sname, '   ', iname
 
  END SUBROUTINE ham_lite_check_wcc

!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE ham_lite_stat_r2d( jcs, jce, jks, klev, kproma, pcheck, pmin, pmax, pmean )

    USE mo_kind_lite,      ONLY: dp 
    USE mo_mpi_lite,       ONLY: p_pe, p_io

    INTEGER, INTENT(IN)       ::  jcs, jce, jks, klev, kproma    
    REAL(dp), INTENT(IN)      ::  pcheck(kproma,klev)
    REAL(dp), INTENT(INOUT)   ::  pmin, pmax, pmean 
    
    INTEGER :: inum, jk, jl
    REAL(dp) :: zsum 

    pmin = +99.0E+20_dp
    DO jk = jks,klev
      DO jl = jcs,jce
        pmin = MIN(pmin,pcheck(jl,jk))
      END DO
    END DO

    pmax = -99.0E+20_dp
    DO jk = jks,klev
      DO jl = jcs,jce
        pmax = MAX(pmax,pcheck(jl,jk))
      END DO
    END DO 

    inum = 0
    zsum = 0.0_dp
    DO jk = jks,klev
      DO jl = jcs,jce
        inum = inum + 1
        zsum = zsum + pcheck(jl,jk) 
      END DO
    END DO 
    pmean = zsum/REAL(inum) 
  
  END SUBROUTINE ham_lite_stat_r2d

!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE ham_lite_stat_cld( jks, klev, qcmin, pback,                    &
                                pdz, pcm1, pim1, pcheck, pmin, pmax, pmean  )

    USE mo_kind_lite,              ONLY: dp 
    USE mo_mpi_lite,               ONLY: p_pe, p_io 

    INTEGER, INTENT(IN)       ::  jks, klev 
    REAL(dp), INTENT(IN)      ::  qcmin, pback,     &
                                  pdz     (klev),   &
                                  pcheck  (klev),   &
                                  pcm1    (klev),   &   ! cloud water  
                                  pim1    (klev)        ! cloud ice 
    REAL(dp), INTENT(INOUT)   ::  pmin, pmax, pmean 
    
    INTEGER :: jk, isum
    REAL(dp) :: zmin, zmax, zlev, zsum
    LOGICAL :: ltest

    isum = 0
    zmin = +99.0E+20_dp
    DO jk = jks,klev
      ltest = ((pcm1(jk) + pim1(jk)) > qcmin) 
      isum  = MERGE(isum + 1, isum, ltest)
      zmin  = MERGE(MIN(zmin,pcheck(jk)), zmin, ltest)
    END DO
    ltest = (isum > 0)
    pmin = MERGE(zmin, pback, ltest) 

    isum = 0
    zmax = -99.0E+20_dp
    DO jk = jks,klev
      ltest = ((pcm1(jk) + pim1(jk)) > qcmin)
      isum  = MERGE(isum + 1, isum, ltest)
      zmax  = MERGE(MAX(zmax,pcheck(jk)), zmax, ltest) 
    END DO 
    ltest = (isum > 0)
    pmax = MERGE(zmax, pback, ltest) 

    isum = 0
    zlev = 0.0_dp
    zsum = 0.0_dp
    DO jk = jks,klev
      ltest = ((pcm1(jk) + pim1(jk)) > qcmin)
      isum  = MERGE(isum + 1, isum, ltest)
      zlev  = MERGE(zlev + pdz(jk), zlev, ltest)
      zsum  = MERGE(zsum + pcheck(jk) * pdz(jk), zsum, ltest)
    END DO 
    ltest = (isum > 0)
    pmean = MERGE(zsum / zlev, pback, ltest) 
  
  END SUBROUTINE ham_lite_stat_cld
 
!---------------------------------------------------------------------------------------------

  SUBROUTINE ham_lite_tail( jcs, jce, jks, jclass, kproma, klev, pcmr, prad, pfrac )

    ! calculate number-fraction larger than radius pr for a given log-normal mode  
    !
    !                        / x                              _
    !                 N      |       1           1   ln(R)-ln(R)  2
    !    N(0,x) = ---------  |   --------  exp(- - ( ----------- )   ) d ln(R) 
    !             ln(sigma)  |   sqrt(2PI)       2    ln(sigma)
    !                        / 0 
    !                         
    !                         /tx                   2
    !                        |        1            t
    !           =     N      |     --------  exp(- - ) d t 
    !                        |     sqrt(2PI)       2 
    !                        /-inf
    ! 
    !    where:                   
    !
    !                        _
    !               ln(R)-ln(R)
    !    t      =   -----------
    !                ln(sigma)
    !
    !    and:
    !                        _
    !               ln(x)-ln(R)
    !    tx     =   -----------
    !                ln(sigma)
 
    USE mo_ham_lite_main,  ONLY: silog_mode   

    IMPLICIT NONE
 
    INTEGER, INTENT(IN) :: jcs, jce, jks, jclass, kproma, klev 
    REAL(dp), INTENT(IN) :: pcmr, &             ! count mean radius
                            prad(kproma,klev)   ! lower bound radius
    REAL(dp), INTENT(INOUT) :: pfrac(kproma,klev)

    INTEGER :: jl, jk
    REAL(dp) :: zarg, zvar, zeps

    zeps = EPSILON(1.0_dp)
 
    DO jk = jks,klev
      DO jl = jcs,jce

        IF (prad(jl,jk) > zeps) THEN

          !--- transform number distribution to error function 
          zarg = (LOG(prad(jl,jk)) - LOG(pcmr))/silog_mode(jclass) 

          !--- calculate the cumulative of the log-normal distribution  
          CALL cumulative_normal(zarg, zvar, pfrac(jl,jk))

        ELSE IF (prad(jl,jk) < zeps) THEN

          pfrac(jl,jk) = 1.0_dp

        ELSE

          pfrac(jl,jk) = 0.0_dp

        END IF

      END DO  
    END DO  

  END SUBROUTINE ham_lite_tail
 
!---------------------------------------------------------------------------------------------------

  SUBROUTINE cumulative_normal ( arg, presult, ccum )
 
    ! evaluate normal distribution function 
    !
    !                              / x
    !                     1       |       -t*t/2
    !          P(x) = ----------- |      e       dt
    !                 sqrt(2 pi)  |
    !                             /-oo
    !
    !    W. J. Cody    Mathematics and Computer Science Division
    !                  Argonne National Laboratory
    !                  Argonne, IL 60439
    !
    !    W. Cody, ALGORITHM 715: SPECFUN - 
    !    A Portable FORTRAN Package of Special Function Routines and Test Drivers,
    !    ACM Transactions on Mathematical Software, volume 19, pages 22-32, 1993 
    !
    !   Parameters:
    !
    !     ARG     = Upper limit of integration. X is double precision
    !
    !     RESULT  = Cumulative normal distribution. RESULT is double precision
    !
    !     CCUM    = Complement of Cumulative normal distribution. CCUM is double precision
    !
    !   The main computation evaluates near-minimax approximations
    !   derived from those in "Rational Chebyshev approximations for
    !   the error function" by W. J. Cody, Math. Comp., 1969, 631-637.
    !   This transportable program uses rational functions that
    !   theoretically approximate the normal distribution function to
    !   at least 18 significant decimal digits.  The accuracy achieved
    !   depends on the arithmetic system, the compiler, the intrinsic
    !   functions, and proper selection of the machine-dependent constants.
    !
    !   Machine-dependent constants:
    !
    !     MIN     = smallest machine representable number
    !
    !     EPS     = argument below which anorm(x) may be represented by
    !               0.5  and above which  x*x  will not underflow.
    !               A conservative value is the largest machine number X
    !               such that   1.0 + X = 1.0   to machine precision.
    !

    USE mo_kind_lite, ONLY: dp

    IMPLICIT NONE

    REAL(dp), PARAMETER, DIMENSION ( 5 ) :: a = (/ &
        2.2352520354606839287e00_dp, &
        1.6102823106855587881e02_dp, &
        1.0676894854603709582e03_dp, &
        1.8154981253343561249e04_dp, &
        6.5682337918207449113e-2_dp /)
    REAL(dp) :: arg
    REAL(dp), PARAMETER, DIMENSION ( 4 ) :: b = (/ &
        4.7202581904688241870e01_dp, &
        9.7609855173777669322e02_dp, &
        1.0260932208618978205e04_dp, &
        4.5507789335026729956e04_dp /)
    REAL(dp), PARAMETER, DIMENSION ( 9 ) :: c = (/ &
        3.9894151208813466764e-1_dp, &
        8.8831497943883759412e00_dp, &
        9.3506656132177855979e01_dp, &
        5.9727027639480026226e02_dp, &
        2.4945375852903726711e03_dp, &
        6.8481904505362823326e03_dp, &
        1.1602651437647350124e04_dp, &
        9.8427148383839780218e03_dp, &
        1.0765576773720192317e-8_dp /)
    REAL(dp) :: ccum
    REAL(dp), PARAMETER, DIMENSION ( 8 ) :: d = (/ &
        2.2266688044328115691e01_dp, &
        2.3538790178262499861e02_dp, &
        1.5193775994075548050e03_dp, &
        6.4855582982667607550e03_dp, &
        1.8615571640885098091e04_dp, &
        3.4900952721145977266e04_dp, &
        3.8912003286093271411e04_dp, &
        1.9685429676859990727e04_dp /)
    REAL(dp) :: del
    REAL(dp) :: eps
    INTEGER :: i
    REAL(dp) :: zmin
    REAL(dp), PARAMETER, DIMENSION ( 6 ) :: p = (/ &
        2.1589853405795699e-1_dp, &
        1.274011611602473639e-1_dp, &
        2.2235277870649807e-2_dp, &
        1.421619193227893466e-3_dp, &
        2.9112874951168792e-5_dp, &
        2.307344176494017303e-2_dp /)
    REAL(dp), PARAMETER, DIMENSION ( 5 ) :: q = (/ &
        1.28426009614491121e00_dp, &
        4.68238212480865118e-1_dp, &
        6.59881378689285515e-2_dp, &
        3.78239633202758244e-3_dp, &
        7.29751555083966205e-5_dp /)
    REAL(dp) :: presult
    REAL(dp), PARAMETER :: root32 = 5.656854248_dp
    REAL(dp), PARAMETER :: sixten = 16.0_dp
    REAL(dp) :: temp
    REAL(dp), PARAMETER :: sqrpi = 3.9894228040143267794e-1_dp
    REAL(dp), PARAMETER :: thrsh = 0.66291_dp
    REAL(dp) :: x
    REAL(dp) :: xden
    REAL(dp) :: xnum
    REAL(dp) :: y
    REAL(dp) :: xsq

    !  machine dependent constants
    eps = EPSILON ( 1.0_dp ) * 0.5_dp

    ! simplified calculation of the smallest machine representable number
    zmin = EPSILON ( 1.0_dp )

    x = arg
    y = ABS ( x )

    IF ( y <= thrsh ) THEN
      !
      !  Evaluate  anorm  for  |X| <= 0.66291
      !
      IF ( y > eps ) THEN
          xsq = x * x
      ELSE
          xsq = 0.0_dp
      END IF

      xnum = a(5) * xsq
      xden = xsq
      DO i = 1, 3
          xnum = ( xnum + a(i) ) * xsq
          xden = ( xden + b(i) ) * xsq
      END DO
      presult = x * ( xnum + a(4) ) / ( xden + b(4) )
      temp = presult
      presult = 0.5_dp + temp
      ccum = 0.5_dp - temp
      !
      !  Evaluate ANORM for 0.66291 <= |X| <= sqrt(32)
      !
    ELSE IF ( y <= root32 ) THEN

      xnum = c(9) * y
      xden = y
      DO i = 1, 7
          xnum = ( xnum + c(i) ) * y
          xden = ( xden + d(i) ) * y
      END DO
      presult = ( xnum + c(8) ) / ( xden + d(8) )
      xsq = AINT ( y * sixten ) / sixten
      del = ( y - xsq ) * ( y + xsq )
      presult = EXP(-xsq*xsq*0.5_dp) * EXP(-del*0.5_dp) * presult
      ccum = 1.0_dp - presult

      IF ( x > 0.0_dp ) THEN
          temp = presult
          presult = ccum
          ccum = temp
      END IF
      !
      !  Evaluate  anorm  for |X| > sqrt(32).
      !
    ELSE

      presult = 0.0_dp
      xsq = 1.0_dp / ( x * x )
      xnum = p(6) * xsq
      xden = xsq
      DO i = 1, 4
          xnum = ( xnum + p(i) ) * xsq
          xden = ( xden + q(i) ) * xsq
      END DO

      presult = xsq * ( xnum + p(5) ) / ( xden + q(5) )
      presult = ( sqrpi - presult ) / y
      xsq = AINT ( x * sixten ) / sixten
      del = ( x - xsq ) * ( x + xsq )
      presult = EXP ( - xsq * xsq * 0.5_dp ) * EXP ( - del * 0.5_dp ) * presult
      ccum = 1.0_dp - presult  

      IF ( x > 0.0_dp ) THEN
          temp = presult
          presult = ccum
          ccum = temp
      END IF

    END IF

    IF ( presult < zmin ) THEN
      presult = 0.0_dp
    END IF

    IF ( ccum < zmin ) THEN
      ccum = 0.0_dp
    END IF

  END SUBROUTINE cumulative_normal

END MODULE mo_ham_lite_misc
