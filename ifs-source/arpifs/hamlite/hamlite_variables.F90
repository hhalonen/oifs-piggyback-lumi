MODULE hamlite_variables
	! ╭───────────────────────────────────────────────────────────╮
	! │ Purpose :   Module for storing HAM-Lite variables         │
	! │ -------                                                   │
	! │ Author :    Hermanni Halonen (UEF)                        │
	! ╰───────────────────────────────────────────────────────────╯

    USE mtime_lite,             ONLY: t_datetime => datetime
    USE mo_model_domain_lite,   ONLY: t_patch
    USE mo_kind_lite,           ONLY: wp

    IMPLICIT NONE

	!<--eehol: Type variables for allocating patch and initializations
	TYPE(t_patch), POINTER :: wrk_p_patch(:)
	TYPE(t_datetime), TARGET :: some_datetime
	TYPE(t_datetime), POINTER :: this_datetime
	!-->eehol

    INTEGER :: kbndsw, &            ! number of SW bands
               kbndlw, &            ! number of LW bands
               nsurf                ! number of surface types (land/water/ice/vegetation)
               
    REAL(wp), ALLOCATABLE :: zrwet(:,:,:,:), &   ! aerosol wet radius per mode [m]
                             zrhop_lite(:,:,:,:) ! mean mode particle density [kg m-3]
    
END MODULE hamlite_variables