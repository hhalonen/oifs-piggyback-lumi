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

#if defined __xlC__ && !defined NOXLFPROCESS
@PROCESS HOT
#endif
#ifndef FSEL_INC_INCLUDED
#define FSEL_INC_INCLUDED
#if !(defined __xlC__ && defined _ARCH_PWR6)
#define SWDIV_NOCHK(a,b) ((a)/(b))
#define FSEL(a,b,c) MERGE(b,c,(a).GE.0._wp)
#endif
#endif

MODULE mo_aes_convect_tables_lite

  ! Lookup tables for convective adjustment code
  !
  ! D. Salmond, CRAY (UK), August 1991, original code
  ! L. Kornblueh, MPI, April 2003, cleanup and move of table setup code
  !                            from setphys.f90 in module
  ! M. Puetz, IBM, April 2009, added spline option
  ! L. Kornblueh, MPI, July 2010, corrected inconsistent use of saturation
  !                            vapour formulae
  ! L. Kornblueh, August 2010, corrected index bug causing inconsistencies
  !                            in table lookup for splines

  USE mo_kind_lite,      ONLY: wp
  USE mo_exception_lite, ONLY: message_text, message, finish
  USE mo_physical_constants_lite, ONLY: alv, als, rd, rv, tmelt, cpd
  !USE mo_aes_cop_config,     ONLY: aes_cop_config
  !USE mo_fortran_tools,      ONLY: assert_acc_device_only

  USE mo_model_domain_lite,       ONLY: p_patch  ! for debugging only
  USE mo_math_constants_lite,     ONLY: rad2deg  ! for debugging only

  IMPLICIT NONE

  SAVE

  PRIVATE

  ! Reference: Sonntag D., 1990: Important new values of the physical
  ! constants of 1986, vapour pressure formulations based on ITS-90,
  ! and psychrometer formulae. Z. Meteor. 70, pp 340-344.

  REAL(wp), PARAMETER :: cavl1 = -6096.9385_wp
  REAL(wp), PARAMETER :: cavl2 =    21.2409642_wp
  REAL(wp), PARAMETER :: cavl3 =    -2.711193_wp
  REAL(wp), PARAMETER :: cavl4 =     1.673952_wp
  REAL(wp), PARAMETER :: cavl5 =     2.433502_wp

  REAL(wp), PARAMETER :: cavi1 = -6024.5282_wp
  REAL(wp), PARAMETER :: cavi2 =    29.32707_wp
  REAL(wp), PARAMETER :: cavi3 =     1.0613868_wp
  REAL(wp), PARAMETER :: cavi4 =    -1.3198825_wp
  REAL(wp), PARAMETER :: cavi5 =    -0.49382577_wp

  ! variables public

  PUBLIC :: jptlucu1          ! lookup table lower bound
  PUBLIC :: jptlucu2          ! lookup table upper bound

  PUBLIC :: fjptlucu1         ! lookup table lower bound (real)
  PUBLIC :: fjptlucu2         ! lookup table upper bound (real)

  PUBLIC :: tlucua            ! table -- e_s*Rd/Rv
  PUBLIC :: tlucub            ! table -- derivative: d es/ d t
  PUBLIC :: tlucuc            ! table -- lv/cp

  PUBLIC :: tlucuaw           ! table -- e_s*Rd/Rv - forcing saturation wrt water
  Public :: tlucubw           ! table -- derivative: d es/ d t - forcing saturation wrt water
  PUBLIC :: tlucucw           ! table -- lv/cp - forcing saturation wrt water

  PUBLIC :: tlucu             ! fused table
  PUBLIC :: tlucuw            ! fused table

  PUBLIC :: lookupoverflow
  PUBLIC :: flookupoverflow

  !PUBLIC :: prepare_ua_index_spline
  !PUBLIC :: prepare_ua_index_spline_batch

  ! PUBLIC :: lookup_ua_spline
  ! PUBLIC :: lookup_ua_spline_batch

  ! PUBLIC :: lookup_uaw_spline
  ! PUBLIC :: lookup_ua_eor_uaw_spline
  ! PUBLIC :: lookup_ua_eor_uaw_spline_batch

  ! PUBLIC :: lookup_ua_list_spline
  ! PUBLIC :: lookup_uaw_list_spline

  ! !PUBLIC :: prepare_ua_index

  ! PUBLIC :: lookup_ua
  ! PUBLIC :: lookup_uaw
  ! PUBLIC :: lookup_ua_eor_uaw

  ! PUBLIC :: lookup_ua_list
  ! PUBLIC :: lookup_uaw_list

  ! PUBLIC :: lookup_ubc
  ! PUBLIC :: lookup_ubc_list

  ! subroutines public

  PUBLIC :: init_convect_tables ! initialize LUTs
  !PUBLIC :: lookuperror         ! error handling routine

  !---------------------------------------------------------------------------
  !
  ! Configuration section:
  !
  !-- temperature evaluation bounds:
  !
  REAL(wp), PARAMETER :: tlbound =  50.0_wp  ! lower bound [K]
  REAL(wp), PARAMETER :: tubound = 400.0_wp  ! upper bound [K]

  !> Maximum temperature when the upper bound has been extended [K].
  REAL(wp), PARAMETER :: tmax_extended = 3000._wp

  !
  !-- full table:
  !
  REAL(wp), PARAMETER :: fdeltat  =   0.001_wp  ! distance of table knots [K]
  ! division is not sufficiently precise, have to replace 1.0_wp/fdeltat
  REAL(wp), PARAMETER :: rfdeltat = 1000.0_wp
  !
  !-- spline interpolation:
  !
  REAL(wp), PARAMETER :: sdeltat  =   0.025_wp  ! distance of table knots [K]
  ! division is not sufficiently precise, have to replace 1.0_wp/sdeltat
  REAL(wp), PARAMETER :: rsdeltat =  40.0_wp
  !
  !---------------------------------------------------------------------------
  !
  ! Derived bounds and deltas:
  !
  !-- full table:
  !
  INTEGER,  PARAMETER :: jptlucu0 = NINT(rfdeltat*tmelt)   ! lookup table physical center
  INTEGER,  PARAMETER :: jptlucu1 = NINT(rfdeltat*tlbound) ! lookup table lower bound
  INTEGER,  PARAMETER :: jptlucu2 = NINT(rfdeltat*tubound) ! lookup table upper bound
  !
  !-- spline interpolation:
  !
  INTEGER,  PARAMETER :: lucupctr = NINT(rsdeltat*tmelt)   ! lookup table physical center
  INTEGER,  PARAMETER :: lucupmin = NINT(rsdeltat*tlbound) ! lookup table lower bound
  INTEGER,  PARAMETER :: lucupmax = NINT(rsdeltat*tubound) ! lookup table upper bound
  !
  !   For the spline interpolation the melting point tmelt = 273.15
  !   is resembled twice at index = lucubctr and index = lucubctr-1
  !   to be able to interpolate values close to the melting point
  !   correctly.
  !
  INTEGER, PARAMETER :: idxctr   = lucupctr
  INTEGER, PARAMETER :: idxctrm1 = idxctr-1
  !
  !---------------------------------------------------------------------------

  ! add float point constants for accelerated boundary guard code
  ! original big table
  REAL(wp), PARAMETER :: fjptlucu1 = REAL(jptlucu1,wp)
  REAL(wp), PARAMETER :: fjptlucu2 = REAL(jptlucu2,wp)
  ! new basic table bounds
  REAL(wp), PARAMETER :: flucupmin = REAL(lucupmin,wp)
  REAL(wp), PARAMETER :: flucupmax = REAL(lucupmax,wp)

  ! logical is fine and logical, but fp faster
  LOGICAL  :: lookupoverflow = .FALSE.          ! preset with false
  REAL(wp) :: flookupoverflow = 0.0_wp          ! preset with false

  REAL(wp) :: tlucua(jptlucu1-1:jptlucu2+1)     ! table - Es*Rd/Rv, mixed phases
  REAL(wp) :: tlucuaw(jptlucu1-1:jptlucu2+1)    ! table - Es*Rd/Rv, water phase only
  REAL(wp) :: tlucuad(jptlucu1-1:jptlucu2+1)    ! table - dEs/dT*Rd/Rv, mixed phases
  REAL(wp) :: tlucuawd(jptlucu1-1:jptlucu2+1)   ! table - dEs/dT*Rd/Rv, water phase only
  REAL(wp) :: tlucub(jptlucu1-1:jptlucu2+1)     ! table - inner dEs/dT*L/cp, mixed phases
  REAL(wp) :: tlucubw(jptlucu1-1:jptlucu2+1)    ! table - inner dEs/dT*L/cp, water phase only
  REAL(wp) :: tlucuc(jptlucu1-1:jptlucu2+1)     ! table - L/cp, mixed phases
  REAL(wp) :: tlucucw(jptlucu1-1:jptlucu2+1)    ! table - L/cp, water phase only
  !$ACC DECLARE CREATE(tlucua)

  ! fused tables for splines
  REAL(wp) :: tlucu(1:2,lucupmin-2:lucupmax+1)     ! fused table
  REAL(wp) :: tlucuw(1:2,lucupmin-2:lucupmax+1)    ! fused table

  !----------------------------------------------------------------------------
CONTAINS
  !----------------------------------------------------------------------------
  SUBROUTINE init_convect_tables
    REAL(wp) :: zalvdcp, zalsdcp
    REAL(wp) :: ztt, zldcp
    REAL(wp) :: zavm1, zavm2, zavm3, zavm4, zavm5
    REAL(wp) :: zminner,zdminner,zlinner,zdlinner
    INTEGER :: it

    ! check if tmelt is hit by one of the table entries (tricky to make right!):
    IF (ABS(REAL(lucupctr,wp)-rsdeltat*tmelt) > EPSILON(1.0_wp)) THEN
      CALL finish('init_convect_tables','distance of spline knots incorrect.')
    ENDIF

    WRITE (message_text,'(a,f8.2,a)')  'Melting temperature  : ', tmelt, ' [K]'
    CALL message('',message_text)
    WRITE (message_text,'(a,f8.2,a)')  'Lower table bound    : ', tlbound, ' [K]'
    CALL message('',message_text)
    WRITE (message_text,'(a,f8.2,a)')  'Upper table bound    : ', tubound, ' [K]'
    CALL message('',message_text)
    WRITE (message_text,'(a)')         'Full table settings  : '
    CALL message('',message_text)
    WRITE (message_text,'(a,f12.3,a)') '  Temperature delta  : ', fdeltat, ' [K]'
    CALL message('',message_text)
    WRITE (message_text,'(a,f12.3,a)') '  Inverse delta      : ', rfdeltat, ' [1/K]'
    CALL message('',message_text)
    WRITE (message_text,'(a,3i8)')     '  Indices [lb,ctr,ub]: ', jptlucu1, jptlucu0, jptlucu2
    CALL message('',message_text)
    WRITE (message_text,'(a)')         'Spline table settings: '
    CALL message('',message_text)
    WRITE (message_text,'(a,f12.3,a)') '  Temperature delta  : ', sdeltat, ' [K]'
    CALL message('',message_text)
    WRITE (message_text,'(a,f12.3,a)') '  Inverse delta      : ', rsdeltat, ' [1/K]'
    CALL message('',message_text)
    WRITE (message_text,'(a,3i8)')     '  Indices [lb,ctr,ub]: ', lucupmin, lucupctr, lucupmax
    CALL message('',message_text)
    WRITE (message_text,'(a,2i8)')     '  Table split indices: ', idxctr, idxctrm1
    CALL message('',message_text)

    zalvdcp = alv/cpd
    zalsdcp = als/cpd

    ! extend the table by one to safe guard code for accessing it+1, when
    ! it is already checked

    ! This are the old tables, extending over 350000 values: kills performance

!IBM* NOVECTOR
    DO it = jptlucu1-1, jptlucu2+1

      ztt = fdeltat*it
      IF ((ztt-tmelt) > 0.0_wp) THEN
        zldcp = zalvdcp
        zavm1 = cavl1
        zavm2 = cavl2
        zavm3 = cavl3
        zavm4 = cavl4
        zavm5 = cavl5
      ELSE
        zldcp = zalsdcp
        zavm1 = cavi1
        zavm2 = cavi2
        zavm3 = cavi3
        zavm4 = cavi4
        zavm5 = cavi5
      END IF

      zminner  = (zavm1/ztt+zavm2+zavm3*0.01_wp*ztt+zavm4*ztt*ztt*1.e-5_wp+zavm5*LOG(ztt))
      zdminner = (-zavm1/(ztt*ztt) + zavm3*0.01_wp + zavm4*ztt*2.e-5_wp + zavm5/ztt)

      zlinner  = (cavl1/ztt+cavl2+cavl3*0.01_wp*ztt+cavl4*ztt*ztt*1.e-5_wp+cavl5*LOG(ztt))
      zdlinner = (-cavl1/(ztt*ztt) + cavl3*0.01_wp + cavl4*ztt*2.e-5_wp + cavl5/ztt)

      tlucua(it)  = EXP(zminner)*rd/rv
      tlucuaw(it) = EXP(zlinner)*rd/rv

      tlucuad(it)  = zdminner*EXP(zminner)*rd/rv
      tlucuawd(it) = zdlinner*EXP(zlinner)*rd/rv

      tlucub(it)  = zldcp*zdminner
      tlucubw(it) = zalvdcp*zdlinner

      tlucuc(it)  = zldcp
      tlucucw(it) = zalvdcp

    END DO

    ! This are the new tables, base for the spline interpolation

!IBM* NOVECTOR
    DO it = lucupmin-2, idxctrm1
      ztt = sdeltat*(it+1)
      zldcp = zalsdcp
      zavm1 = cavi1
      zavm2 = cavi2
      zavm3 = cavi3
      zavm4 = cavi4
      zavm5 = cavi5

      zminner  = (zavm1/ztt+zavm2+zavm3*0.01_wp*ztt+zavm4*ztt*ztt*1.e-5_wp+zavm5*LOG(ztt))
      zdminner = (-zavm1/(ztt*ztt) + zavm3*0.01_wp + zavm4*ztt*2.e-5_wp + zavm5/ztt)

      zlinner  = (cavl1/ztt+cavl2+cavl3*0.01_wp*ztt+cavl4*ztt*ztt*1.e-5_wp+cavl5*LOG(ztt))
      zdlinner = (-cavl1/(ztt*ztt) + cavl3*0.01_wp + cavl4*ztt*2.e-5_wp + cavl5/ztt)

      tlucu(1,it)  = EXP(zminner)*rd/rv
      tlucu(2,it)  = sdeltat*zdminner*EXP(zminner)*rd/rv

      tlucuw(1,it) = EXP(zlinner)*rd/rv
      tlucuw(2,it) = sdeltat*zdlinner*EXP(zlinner)*rd/rv

    END DO

!IBM* NOVECTOR
    DO it = idxctr, lucupmax+1
      ztt = sdeltat*it
      zldcp = zalvdcp
      zavm1 = cavl1
      zavm2 = cavl2
      zavm3 = cavl3
      zavm4 = cavl4
      zavm5 = cavl5

      zminner  = (zavm1/ztt+zavm2+zavm3*0.01_wp*ztt+zavm4*ztt*ztt*1.e-5_wp+zavm5*LOG(ztt))
      zdminner = (-zavm1/(ztt*ztt) + zavm3*0.01_wp + zavm4*ztt*2.e-5_wp + zavm5/ztt)

      zlinner  = (cavl1/ztt+cavl2+cavl3*0.01_wp*ztt+cavl4*ztt*ztt*1.e-5_wp+cavl5*LOG(ztt))
      zdlinner = (-cavl1/(ztt*ztt) + cavl3*0.01_wp + cavl4*ztt*2.e-5_wp + cavl5/ztt)

      tlucu(1,it)  = EXP(zminner)*rd/rv
      tlucu(2,it)  = sdeltat*zdminner*EXP(zminner)*rd/rv

      tlucuw(1,it) = EXP(zlinner)*rd/rv
      tlucuw(2,it) = sdeltat*zdlinner*EXP(zlinner)*rd/rv
    END DO

    !$ACC ENTER DATA COPYIN(tlucu, tlucuw)
    !$ACC UPDATE DEVICE(tlucua) ASYNC(1)

  END SUBROUTINE init_convect_tables

END MODULE mo_aes_convect_tables_lite
