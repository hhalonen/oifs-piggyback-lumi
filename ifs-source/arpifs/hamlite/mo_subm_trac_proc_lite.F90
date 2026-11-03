! 
! based on mo_tracer_processes.f90 
! in https://git.iac.ethz.ch/hammoz/icon-hammoz.git (hammoz/gpu/master)
!
! Philipp Weiss, University of Oxford  
!
! Rewritten for OIFS: all ICON variable-list / CDI / GRIB2 infrastructure
! replaced with plain Fortran ALLOCATE.  Callers are currently commented out
! in mo_subm_interface_lite, so everything is effectively a no-op.
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

MODULE mo_subm_trac_proc_lite

  USE mo_kind_lite,                ONLY: wp
  USE mo_exception_lite,           ONLY: message, finish
  USE mo_subm_trac_lite,           ONLY: diag_burden
  USE mo_subm_trac_para_lite,      ONLY: nmaxtrac
  USE mo_impl_constants_lite,      ONLY: SUCCESS, MAX_CHAR_LENGTH

  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: xt_burden_init_mem  ! set up burden diagnostics
  PUBLIC :: xt_burden           ! calculate tracer burdens  

  !--------------------
  ! module parameters
  !--------------------

  CHARACTER(len=*), PARAMETER :: thismodule = 'mo_subm_trac_proc'

  !--------------------
  ! module variables
  !--------------------

  TYPE t_diag_burden_field
    TYPE ( diag_burden ) d_burden(nmaxtrac)
  END TYPE t_diag_burden_field

  TYPE ( t_diag_burden_field ), ALLOCATABLE, TARGET :: diag_burden_field(:)

  CONTAINS

!------------------------------------------------------------------------------
  SUBROUTINE xt_burden_init_mem ( p_patch )

    USE mo_model_domain_lite,  ONLY: t_patch
    USE mo_subm_trac_lite,     ONLY: nburden, d_burden
    USE mo_parallel_config_lite, ONLY: nproma

    IMPLICIT NONE

    TYPE(t_patch),INTENT(IN), DIMENSION(:)  :: p_patch

    INTEGER :: ndomain, jg, jn, ist, nblks, itype

    CALL message(TRIM(thismodule),'construction of tracer processes started')

    ndomain = SIZE(p_patch)

    ALLOCATE( diag_burden_field(ndomain), STAT=ist)
    IF (ist/=SUCCESS) CALL finish(TRIM(thismodule), 'allocation of diag_burden_field failed')

    ! Manually allocate burden pointer arrays (replaces ICON add_var)
    DO jg = 1, ndomain
      nblks = p_patch(jg)%nblks_c
      DO jn = 1, nburden
        itype = d_burden(jn)%itype
        IF (IAND(itype, 1) /= 0) THEN
          ALLOCATE(diag_burden_field(jg)%d_burden(jn)%ptr1(nproma, nblks))
          diag_burden_field(jg)%d_burden(jn)%ptr1 = 0._wp
        END IF
        IF (IAND(itype, 2) /= 0) THEN
          ALLOCATE(diag_burden_field(jg)%d_burden(jn)%ptr2(nproma, nblks))
          diag_burden_field(jg)%d_burden(jn)%ptr2 = 0._wp
        END IF
        IF (IAND(itype, 4) /= 0) THEN
          ALLOCATE(diag_burden_field(jg)%d_burden(jn)%ptr4(nproma, nblks))
          diag_burden_field(jg)%d_burden(jn)%ptr4 = 0._wp
        END IF
        IF (IAND(itype, 8) /= 0) THEN
          ALLOCATE(diag_burden_field(jg)%d_burden(jn)%ptr8(nproma, nblks))
          diag_burden_field(jg)%d_burden(jn)%ptr8 = 0._wp
        END IF
        IF (IAND(itype, 16) /= 0) THEN
          ALLOCATE(diag_burden_field(jg)%d_burden(jn)%ptr16(nproma, nblks))
          diag_burden_field(jg)%d_burden(jn)%ptr16 = 0._wp
        END IF
        IF (IAND(itype, 32) /= 0) THEN
          ALLOCATE(diag_burden_field(jg)%d_burden(jn)%ptr32(nproma, nblks))
          diag_burden_field(jg)%d_burden(jn)%ptr32 = 0._wp
        END IF
      END DO
    END DO

    CALL message(TRIM(thismodule),'construction of tracer processes finished')

  END SUBROUTINE xt_burden_init_mem

!------------------------------------------------------------------------------
  SUBROUTINE xt_burden( jg, jb, jcs, jce, kproma, klev, klevp1, &
                        pdtime, pmair, pxtm1, pxtte             )

    USE mo_kind_lite,              ONLY: wp
    USE mo_exception_lite,         ONLY: finish, message
    USE mo_subm_main_lite,         ONLY: lburden
    USE mo_subm_trac_lite,         ONLY: ntrac, trlist, nburden, d_burden
    USE mo_ham_lite_main,          ONLY: sizeclass
    USE mo_ham_lite_misc,          ONLY: lcheck_bdn, ham_lite_check_wcc, ham_lite_check_r1d

    IMPLICIT NONE

    INTEGER,  INTENT(IN) :: jg, jb, jcs, jce, kproma, klev, klevp1
    REAL(wp), INTENT(IN) :: pdtime
    REAL(wp), INTENT(IN) :: pmair(:,:)
    REAL(wp), INTENT(IN) :: pxtm1(:,:,:), pxtte(:,:,:)

    INTEGER :: jk, jl, jt, iburden, itype
    LOGICAL, SAVE :: lfirst=.TRUE.

    IF (ntrac == 0 .OR. .NOT. lburden) RETURN

    IF (lfirst) THEN
      DO jt = 1, ntrac
        IF (trlist%ti(jt)%burdenid > nburden) &
          CALL finish('xt_burden', 'invalid burden id for tracer ' // trlist%ti(jt)%fullname)
      END DO
    END IF

    !-- initialize
    DO jt = 1,ntrac
      iburden = trlist%ti(jt)%burdenid
      IF (iburden <= 0) CYCLE
      itype = d_burden(iburden)%itype
      IF (IAND(itype, 1) /= 0) THEN
        DO jl = jcs,jce
          diag_burden_field(jg)%d_burden(iburden)%ptr1(jl,jb) = 0._wp
        END DO
      END IF
      IF (IAND(itype, 8) /= 0) THEN
        DO jl = jcs,jce
          diag_burden_field(jg)%d_burden(iburden)%ptr8(jl,jb) = 0._wp
        END DO
      END IF
    END DO

    !--- calculate and store
    DO jt = 1,ntrac
      iburden = trlist%ti(jt)%burdenid
      IF (iburden <= 0) CYCLE
      itype = d_burden(iburden)%itype

      ! total column mass
      IF (IAND(itype, 1) /= 0) THEN
        DO jk = 1,klev
          DO jl = jcs,jce
            diag_burden_field(jg)%d_burden(iburden)%ptr1(jl,jb) = &
              diag_burden_field(jg)%d_burden(iburden)%ptr1(jl,jb) + &
              (pxtm1(jl,jk,jt) + pxtte(jl,jk,jt)*pdtime)* pmair(jl,jk)
          END DO
        END DO
      END IF
      IF (IAND(itype, 2) /= 0) THEN
        IF (lfirst) CALL message('xt_burden', 'burdentype 2 not implemented')
      END IF
      IF (IAND(itype, 4) /= 0) THEN
        IF (lfirst) CALL message('xt_burden', 'burdentype 4 not implemented')
      END IF
      IF (IAND(itype, 8) /= 0) THEN
        IF (lfirst) CALL message('xt_burden', 'burdentype 8 not implemented')
      END IF
      IF (IAND(itype, 16) /= 0) THEN
        IF (lfirst) CALL message('xt_burden', 'burdentype 16 not properly implemented yet!')
      END IF
      IF (IAND(itype, 32) /= 0) THEN
        IF (lfirst) CALL message('xt_burden', 'burdentype 32 not properly implemented yet!')
      END IF

      IF (lcheck_bdn) THEN
        CALL ham_lite_check_wcc( jb, sizeclass(jt)%shortname, 'mode' )
        CALL ham_lite_check_r1d( jb, jcs, jce, kproma, &
          diag_burden_field(jg)%d_burden(iburden)%ptr1(:,:), 'xt_burden', 'd_burden' )
      END IF

    END DO

    lfirst=.FALSE.

  END SUBROUTINE xt_burden

END MODULE mo_subm_trac_proc_lite
