! 
! based on mo_hammoz_emi_volcano.f90 
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

MODULE mo_hammoz_lite_emi_vol

  USE mo_kind_lite, ONLY: dp 

  IMPLICIT NONE
  PRIVATE

  !--------------------
  ! module interface
  !--------------------

  PUBLIC :: init_emi_volcano!, read_emi_volcano

  !--------------------
  ! module variables
  !--------------------

  INTEGER :: ibc_volcc, ibc_volce ! index to boundary conditions

  !--------------------
  ! module parameters
  !--------------------

  INTEGER, PARAMETER :: nlmax = 2000  ! maximum number of lines in file
  INTEGER, PARAMETER :: ncmax = 32    ! maximum number of columns
  INTEGER, PARAMETER :: ncols = 5     ! number of columns in file 
  REAL(dp), PARAMETER :: pi_180 = ATAN(1._dp)/45._dp
  REAL(dp), PARAMETER :: missv = -999.e7_dp 

  CHARACTER(LEN=*), PARAMETER ::thismodule = 'mo_hammoz_emi_vol'

  CONTAINS

!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE init_emi_volcano( jg, ktype )

    USE mo_exception_lite,       ONLY: finish
    USE mo_subm_bcs_lite,        ONLY: bc_find, bc_modify, BC_EVERYWHERE

    IMPLICIT NONE

    INTEGER, INTENT(IN) ::  jg,     & ! grid index
                            ktype     ! type of emissions to initialize:
                                      ! 1 = continuous degassing
                                      ! 2 = explosive

    INTEGER             :: ibc        ! index to boundary condition
    CHARACTER(LEN=64)   :: bc_name

    IF (ktype < 1 .OR. ktype > 2) CALL finish('init_emi_volcano', 'invalid value of ktype')

    !-- locate corresponding boundary condition
    IF (ktype == 1) THEN
      bc_name = 'VOLCC emissions of SO2'
    ELSE
      bc_name = 'VOLCE emissions of SO2'
    END IF
    CALL bc_find(jg, bc_name, ibc)
    IF (ibc < 1) CALL finish('init_emi_volcano', 'cannot find boundary condition for '//TRIM(bc_name))
    IF (ktype == 1) THEN
      ibc_volcc = ibc
    ELSE
      ibc_volce = ibc
    END IF

    !-- modify boundary condition to fit the specific needs of this module
    CALL bc_modify(jg, ibc, bc_domain=BC_EVERYWHERE, bc_ndims=3, ef_actual_unit='kg m-2 s-1')

  END SUBROUTINE init_emi_volcano

!---------------------------------------------------------------------------------------------------

  ! SUBROUTINE read_emi_volcano( jg, p_patch, pareacella, pclon, pclat, pzh, ktype, lprint_info )
 
  !   ! read volcanic SO2 emissions from ascii file and remap values to ICON grid
 
  !   USE mo_exception,           ONLY: finish, message, message_text
  !   USE mo_parallel_config,     ONLY: nproma, p_test_run
  !   USE mo_run_config,          ONLY: nlev
  !   USE mo_mpi,                 ONLY: p_pe, p_io, p_bcast, p_comm_work
  !   USE mo_model_domain,        ONLY: t_patch
  !   USE mo_gnat_gridsearch,     ONLY: t_gnat_tree, gnat_init_grid, gnat_query_containing_triangles, &
  !                                     gnat_merge_distributed_queries, gk
  !   USE mo_grid_config,         ONLY: grid_sphere_radius
  !   USE mo_impl_constants,      ONLY: min_rlcell_int, grf_bdywidth_c
  !   USE mo_loopindices,         ONLY: get_indices_c
  !   USE mo_sync,                ONLY: global_sum_array 
  !   USE mo_subm_bcs,            ONLY: bc_set

  !   IMPLICIT NONE

  !   INTEGER, INTENT(IN)   ::  jg      ! grid index
  !   INTEGER, INTENT(IN)   ::  ktype   ! type of emissions to initialize:
  !                                     ! 1 = continuous degassing, 2 = explosive
  !   REAL(dp), INTENT(IN)  ::  pareacella(:,:),  & ! grid box area [m2] (nproma, nblk)  
  !                             pzh(:,:,:),       & ! geometric height of half levels [m] (nproma, klevp1, nblk) 
  !                             pclon(:,:),       & ! grid cell center longtude (nproma, nblk)
  !                             pclat(:,:)          ! grid cell center latitide (nproma, nblk)
  !   LOGICAL, INTENT(IN), OPTIONAL :: lprint_info
  !   TYPE(t_patch), TARGET, INTENT(IN) :: p_patch 

  !   INTEGER ::  ib, ik, il, ibc,                  &
  !               jb, jk, jl, jbs, jbe, jcs, jce,   &
  !               jrow, jvol,                       &  
  !               ilower, iupper,                   &  
  !               nrows,                            &  
  !               npromz_lonlat, nblks_lonlat,      &
  !               nblks_c, ncd, rls, rle,           &  
  !               npts_icon, nthis_local_pts 
  !   INTEGER, ALLOCATABLE :: tri_idx(:,:,:), global_idx(:)
  !   LOGICAL :: lprint_info_l 
  !   REAL(dp) :: zsemi, zvfac  
  !   REAL(gk), ALLOCATABLE ::  in_points(:,:,:), min_dist(:,:),  &
  !                             zvemi(:,:,:), zread(:,:),         &
  !                             zlats(:), zlons(:), zfemi(:),     &   ! fields for reading
  !                             zhlow(:), zhhei(:)
  !   CHARACTER(LEN=256)  :: cfilename, clabel
  !   CHARACTER(LEN=22)   :: formstr = '(2(1x,i4),3(1x,e10.4))'
  !   CHARACTER(LEN=30)   :: units
    
  !   TYPE (t_gnat_tree)  :: gnat
  
  !   IF ( PRESENT ( lprint_info ) ) THEN
  !     lprint_info_l = lprint_info
  !   ELSE
  !     lprint_info_l = .FALSE.
  !   END IF
 
  !   ncd = MAX(1,p_patch%n_childdom)
  !   nblks_c = p_patch%nblks_c
  !   rls = grf_bdywidth_c+1
  !   rle = min_rlcell_int
  !   jbs = p_patch%cells%start_blk(rls,1)
  !   jbe = p_patch%cells%end_blk(rle,ncd)

  !   IF (ktype < 1 .OR. ktype > 2) CALL finish('init_emi_volcano', 'invalid value of ktype')

  !   !-- initialize

  !   ALLOCATE (zvemi(nproma,nlev,nblks_c))

  !   DO jb = 1,nblks_c
  !     DO jk = 1,nlev
  !       DO jl = 1,nproma
  !         zvemi(jl,jk,jb) = 0._dp
  !       END DO
  !     END DO
  !   END DO

  !   !-- read in emissions

  !   IF (ktype == 1) THEN
  !     ibc = ibc_volcc
  !     cfilename = 'continuous_volcanos.dat'
  !     clabel = 'SO2 emissions from continuous volcanic degassing'
  !   ELSE
  !     ibc = ibc_volce
  !     cfilename = 'explosive_volcanos.dat'
  !     clabel = 'SO2 emissions from explosive volcanic eruptions'
  !   END IF
  !   cfilename = TRIM(cfilename)

  !   CALL message('init_emi_volcano', 'reading volcanic so2 emissions from '//TRIM(cfilename))
    
  !   ALLOCATE (zread(nlmax,ncols)) 
  !   IF ( p_pe == p_io ) THEN
  !     CALL read_ascii_file(cfilename, ncols, nlmax, nrows, zread, formstr, 'init_emi_volcano', units)
  !     IF (TRIM(ADJUSTL(units)) .ne. 'kg y-1') THEN
  !       WRITE(message_text,'(a,a)') 'units of input file should be [kg y-1], units found are: ', TRIM(units)
  !       CALL finish('read_emi_volcano', message_text)
  !     END IF
  !   END IF 
  !   CALL p_bcast(zread, p_io, p_comm_work)
  !   CALL p_bcast(nrows, p_io, p_comm_work)
 
  !   ALLOCATE( zlons(nrows), zlats(nrows), zfemi(nrows), zhlow(nrows), zhhei(nrows) )
    
  !   DO jrow = 1,nrows
  !     zlons(jrow) = zread(jrow,1)
  !     zlats(jrow) = zread(jrow,2)
  !     zfemi(jrow) = zread(jrow,3)
  !     zhlow(jrow) = zread(jrow,4)
  !     zhhei(jrow) = zread(jrow,5)
  !   END DO
 
  !   zsemi = 0._dp
  !   DO jrow = 1,nrows
  !     zsemi = zsemi + zfemi(jrow)
  !   END DO

  !   WRITE(message_text,'(a,e25.15,a)') 'global total '//TRIM(clabel)//' = ',zsemi,' kg(SO2) year-1'
  !   CALL message('read_emi_volcano', message_text)

  !   !-- remap horizontally to ICON grid

  !   nblks_lonlat  = (nrows - 1)/nproma + 1          ! number of blocks
  !   npromz_lonlat = nrows - (nblks_lonlat-1)*nproma ! end point in last block

  !   ALLOCATE( in_points(nproma,nblks_lonlat,2), &
  !             tri_idx(2,nproma,nblks_lonlat),   &
  !             min_dist(nproma,nblks_lonlat),    &
  !             global_idx(nrows)                 )

  !   DO jrow = 1,nrows
  !     global_idx(jrow) = -1
  !   END DO

  !   DO ib = 1,nblks_lonlat
  !     DO il = 1,nproma
  !       DO ik = 1,2
  !         tri_idx(ik,il,ib) = -1
  !       END DO
  !     END DO
  !   END DO

  !   DO ib = 1,nblks_lonlat
  !     DO il = 1,nproma 
  !       min_dist(il,ib) = 90._gk 
  !     END DO
  !   END DO
 
  !   il = 0
  !   ib = 1
  !   DO jrow = 1,nrows
  !     il = il + 1
  !     IF (il > nproma) THEN
  !       il = 1
  !       ib = ib + 1
  !     ENDIF
  !     in_points(il,ib,1) = REAL(pi_180 * zlons(jrow), gk)  
  !     in_points(il,ib,2) = REAL(pi_180 * zlats(jrow), gk)   
  !   END DO

  !   CALL gnat_init_grid( gnat, p_patch )

  !   CALL gnat_query_containing_triangles( gnat, p_patch, in_points(:,:,:),            &
  !                                         nproma, nblks_lonlat, npromz_lonlat,        &
  !                                         grid_sphere_radius,                         &
  !                                         p_test_run, tri_idx(:,:,:), min_dist(:,:)   )
 
  !   CALL gnat_merge_distributed_queries(  p_patch, nrows, nproma, nblks_lonlat,       &
  !                                         min_dist, tri_idx(:,:,:), in_points(:,:,:), &
  !                                         global_idx(:), nthis_local_pts              )

  !   ! print information
  !   IF ( lprint_info_l ) THEN
  !     jrow = 1
  !     CALL message(thismodule,'placing volcanoes')
  !     DO ib = 1,nblks_lonlat
  !       DO il = 1,nproma
  !         jl = tri_idx(1,il,ib)
  !         jb = tri_idx(2,il,ib)
  !         jvol = global_idx(jrow)
  !         IF ( jl > 0 .AND. jrow <= nthis_local_pts ) THEN 
  !           WRITE(message_text,FMT='(i5.5,2(1X,F7.2),A5,2(1X,F7.2))') p_pe, &
  !             zlons(jvol), zlats(jvol), ' --- ', pclon(jl,jb)/pi_180, pclat(jl,jb)/pi_180         
  !           CALL message(thismodule, message_text, all_print=.TRUE.)
  !           jrow = jrow + 1 
  !         END IF
  !       END DO
  !     END DO            
  !   END IF

  !   npts_icon=global_sum_array(nthis_local_pts)
  !   IF ( npts_icon > nrows ) THEN
  !     WRITE(message_text,*) 'too many volcanoes placed on grid', nrows, '<', npts_icon 
  !     CALL finish(thismodule, message_text)
  !   END IF
  !   IF ( npts_icon < nrows ) THEN
  !     WRITE(message_text,*) 'not all volcanoes placed on grid', nrows, '>', npts_icon 
  !     CALL finish(thismodule, message_text)
  !   END IF

  !   !-- remap vertically to ICON grid

  !   jrow = 1
  !   DO ib = 1,nblks_lonlat
  !     DO il = 1,nproma
  !       IF ( tri_idx(1,il,ib) > 0  .AND. jrow <= nthis_local_pts ) THEN 

  !         jl = tri_idx(1,il,ib)
  !         jb = tri_idx(2,il,ib)
  !         jvol = global_idx(jrow)

  !         ! find model levels corresponding to min and max altitude of emissions  
  !         ilower = -1
  !         DO jk = 1,nlev
  !           IF (zhlow(jvol) <= pzh(jl,jk,jb)) THEN
  !             ilower = jk + 1
  !           END IF
  !         END DO
  !         IF (ilower > nlev) ilower = nlev
          
  !         iupper = nlev
  !         DO jk = nlev,1,-1
  !           IF (zhhei(jvol) >= pzh(jl,jk,jb)) THEN
  !             iupper = jk
  !           END IF
  !         END DO 

  !         IF (ilower == -1) THEN
  !           WRITE(message_text,'(a,i0,e25.15,a,i0,i0,e25.15)')  & 
  !             'ilower=-1 for jvol, zhlow = ',jvol, zhlow(jvol), 'jl, jb, zh=', jl, jb, pzh(jl,:,jb)
  !           CALL message('read_emi_volcano', message_text)
  !         END IF

  !         IF (ilower < iupper) THEN
  !           WRITE(message_text,'(a,i0)') 'error in emission levels. file '//TRIM(cfilename)//', line ',jvol
  !           CALL message('read_emi_volcano', message_text)
  !         END IF

  !         ! forward emissions to grid cells
  !         ! convert units from [kg(so2) year-1] to [kg(so2) m-2 s-1]
  !         zvfac = 1._dp / REAL(ilower-iupper+1, dp)
  !         DO jk = iupper,ilower
  !           zvemi(jl,jk,jb) = zvemi(jl,jk,jb) + zvfac * zfemi(jvol) / (pareacella(jl,jb) *  86400._dp * 365._dp)
  !         END DO

  !         jrow = jrow + 1

  !       END IF 
  !     END DO
  !   END DO
 
  !   DO jb = jbs,jbe 
      
  !     CALL get_indices_c(p_patch, jb, jbs, jbe, jcs, jce, rls, rle)
  !     IF (jcs > jce) CYCLE

  !     CALL bc_set(jg, jb, jcs, jce, ibc, zvemi(:,:,jb))

  !   END DO

  !   DEALLOCATE( zvemi, zread, zhlow, zhhei, zfemi, zlats, zlons,  &
  !               in_points, tri_idx, min_dist, global_idx          )


  ! END SUBROUTINE read_emi_volcano

!---------------------------------------------------------------------------------------------------
 
  SUBROUTINE read_ascii_file( filename, ncols, maxcount, ncount,        &
                              buffer, formatstr, calling_routine, units )
    
    ! open and read from an ascii file
    ! file may contain comment lines (starting with '#')  
  
    USE mo_exception_lite,     ONLY: finish
    USE mo_io_units_lite,      ONLY: find_next_free_unit
    USE mo_mpi_lite,           ONLY: p_pe, p_io

    IMPLICIT NONE
 
    CHARACTER(LEN=*), INTENT(IN)    :: filename, formatstr, calling_routine
    INTEGER, INTENT(IN)             :: ncols, maxcount
    INTEGER, INTENT(OUT)            :: ncount
    REAL(dp), INTENT(INOUT)         :: buffer(maxcount,ncols)
    CHARACTER(len=*), INTENT(INOUT) :: units
 
    INTEGER             :: iunit, ierr, ilines, jl, jc
    LOGICAL             :: lex
    REAL(dp)            :: zvalbuf(ncmax)
    CHARACTER(LEN=255)  :: linebuf

    ncount = 0           ! number of lines infile
    ilines = 0           ! number of lines in file (including comments)
    linebuf = '                                                                                      '

    DO jc = 1,ncols
      DO jl = 1,maxcount
        buffer(jl,jc) = 0._dp
      END DO
    END DO

    DO jc = 1,ncmax
      zvalbuf = missv 
    END DO

    IF (p_pe == p_io ) THEN

      iunit = find_next_free_unit (20, 99)
      INQUIRE (file=filename,exist=lex)
      IF (.NOT. lex) THEN
        CALL finish( calling_routine, 'failed to open file '//TRIM(filename)//'. file does not exist.' )
      END IF
      ! open file
      OPEN (iunit,file=TRIM(filename),form='FORMATTED',iostat=ierr)
      IF (ierr /= 0) CALL finish('read_ascii_file', 'could not open file '//TRIM(filename))
      REWIND(iunit)
      DO WHILE (ierr == 0)
        READ(iunit,FMT='(a)',IOSTAT=ierr) linebuf
        ilines = ilines + 1
        IF (ierr < 0) EXIT     
        IF (ierr > 0) THEN
          write(linebuf,'(i0)') ilines
          CALL finish( calling_routine, 'failed to read line '//TRIM(linebuf)//' in file '//TRIM(filename) )
        END IF
        ! analyze line
        IF (linebuf(1:1) == '#') CYCLE   
        IF (linebuf(1:6) == 'units:') THEN
          units=TRIM(linebuf(7:))
          CYCLE
        ENDIF 
        READ(linebuf,FMT=*,IOSTAT=ierr) zvalbuf(1:ncols)
        IF (ierr /= 0) THEN
          write(linebuf,'(i0)') ilines
          CALL finish( calling_routine, 'failed to read data in line '//TRIM(linebuf)//' in file '//TRIM(filename) )
        END IF
        ncount = ncount + 1
        IF (ncount > maxcount) THEN
          write(linebuf,'(i0)') maxcount
          CALL finish( calling_routine, 'error reading file '//TRIM(filename)//': number of lines exceeds '//linebuf )
        END IF
        DO jc = 1,ncols
          buffer(ncount,jc) = zvalbuf(jc)
        END DO
      END DO

      CLOSE(iunit)

    END IF

  END SUBROUTINE read_ascii_file

END MODULE mo_hammoz_lite_emi_vol

