SUBROUTINE HAMLITE_INIT(NFLEVG, NPROMA)
	! ╭───────────────────────────────────────────────────────────╮
	! │ Purpose :   Routine for initializing HAM-Lite variables   │
	! │ -------                                                   │
	! │ Author :    Hermanni Halonen (UEF)                        │
	! ╰───────────────────────────────────────────────────────────╯
	
	USE hamlite_variables
	USE mtime_lite
	USE mo_subm_trac_lite,             ONLY: ntrac
	USE mo_kind_lite,                  ONLY: dp
	USE mo_alloc_patches_lite,         ONLY: allocate_basic_patch
	USE mo_lite_main_list_config,      ONLY: init_defaults_lite_main_list !eehol: namelist defaults
    USE mo_subm_interface_lite,        ONLY: init_hamlite_submodel
    USE mo_subm_main_list_config_lite, ONLY: init_defaults_subm_main_list !eehol: namelist defaults
    USE mo_parallel_config_lite,       ONLY: set_nproma!,set_nproma_nblocks
    USE mo_lite_dust_list_config,      ONLY: init_defaults_lite_dust_list 
                                       !eehol: namelist defaults (for dust emissions if needed)
    USE PARKIND1,                      ONLY: JPIM, JPRB
    USE mo_ham_lite_misc,              ONLY: pm6rp,prhop, prelhum
    USE mo_ham_lite_main,              ONLY: nclass
    USE hamlite_driver_input,          ONLY: jb
    USE mo_kind_lite,                  ONLY: wp
    USE mo_aes_convect_tables_lite,    ONLY: init_convect_tables
    
    INTEGER :: NFLEVG, NPROMA
    
    !----------------------------------------------------------------------------
    !-------Starting initializations------------------------
    ! Allocate HAM-Lite variables
    !---Allocating patch for HAM-lite model (these should go to host model init routines)
    ALLOCATE(wrk_p_patch(1))
    ! Please note: The following variables in the patch MUST already be set:
    wrk_p_patch(1)%alloc_cell_blocks = 1
    wrk_p_patch(1)%nblks_e = 1
    wrk_p_patch(1)%nblks_v = 1
    wrk_p_patch(1)%n_patch_cells = 1
    wrk_p_patch(1)%n_patch_edges = 1
    wrk_p_patch(1)%n_patch_verts = 1
    wrk_p_patch(1)%n_patch_cells_g = 1
    wrk_p_patch(1)%n_patch_edges_g = 1
    wrk_p_patch(1)%n_patch_verts_g = 1
    wrk_p_patch(1)%max_childdom = 1
    wrk_p_patch(1)%nblks_c = NPROMA !eehol: how many blocks in cell
    wrk_p_patch(1)%id = 1 !eehol: patch id
    wrk_p_patch(1)%n_childdom = 1
    wrk_p_patch(1)%nlev = NFLEVG !eehol: number of full levels
    !wrk_p_patch(1)%cells%start_blk = 1
    !wrk_p_patch(1)%cells%end_blk = 1
    CALL allocate_basic_patch(wrk_p_patch(1)) !eehol: this should go to host model init routines (if needed in the end)
	!---End allocating patch for HAM-lite model

	!---Initialize HAM-lite submodel (these should go to host model init routines)
	!Read into datetime
	this_datetime => some_datetime
	!Date: 2010-01-01 00:00:00
	this_datetime%date%year = 2010
	this_datetime%date%month = 1
	this_datetime%date%day = 1
	this_datetime%time%hour = 0
	this_datetime%time%minute = 0
	this_datetime%time%second = 0
	this_datetime%time%ms = 0
	CALL set_nproma(NPROMA)
	!emi_basepath="./emissions_inventories"
	CALL init_defaults_subm_main_list !eehol: initialize default values for submodel main list (these should come from namelist)
	CALL init_defaults_lite_main_list !eehol: initialize default values for HAM lite main list (these should come from namelist)
	CALL init_defaults_lite_dust_list !eehol: initialize default values for dust main list (these should come from namelist)
	CALL init_hamlite_submodel(wrk_p_patch,this_datetime) !eehol: init hamlite submodel
	CALL init_convect_tables
	!---End initialize HAM-lite submodel
	
	kbndsw=14            ! number of SW bands 
	kbndlw=16            ! number of LW bands
	nsurf = 4
	
	!---Allocate and calculate wet radius and density
    IF (.NOT. ALLOCATED(pm6rp)) ALLOCATE(pm6rp(NPROMA,NFLEVG,nclass))
    IF (.NOT. ALLOCATED(prhop)) ALLOCATE(prhop(NPROMA,NFLEVG,nclass))
    IF (.NOT. ALLOCATED(prelhum)) ALLOCATE(prelhum(NPROMA,NFLEVG))
	IF (.NOT. ALLOCATED(zrwet)) ALLOCATE(zrwet(NPROMA,NFLEVG,jb,nclass))
	IF (.NOT. ALLOCATED(zrhop_lite)) ALLOCATE(zrhop_lite(NPROMA,NFLEVG,jb,nclass))
	!---Allocate and calculate wet radius and density
	
	zrwet(:,:,:,:) = 0.0_wp
    zrhop_lite(:,:,:,:) = 0.0_wp
    pm6rp(:,:,:) = 0.0_JPRB
    prhop(:,:,:) = 0.0_JPRB
    prelhum(:,:) = 0.0_JPRB

END SUBROUTINE HAMLITE_INIT