# Compiler
FC = gfortran

# Compiler flags
FFLAGS = -cpp -O2 -w -D__STANDALONE -DNOMPI -D__NO_ICON_COMIN__ -fcheck=all -I./include -I/usr/include/ -I./namelists
#FFLAGS = -cpp -g -O0 -Wall -fcheck=all -fbacktrace -fno-omit-frame-pointer -D__STANDALONE -DNOMPI -D__NO_ICON_COMIN__ -I./include -I/usr/include/ -I./namelists

# Source files
SRC = mtime_constants_lite.F90 mtime_lite.F90 mo_kind_lite.F90 hamlite_driver_input.F90 mo_io_units_lite.F90 mo_util_backtrace_lite.F90 mo_exception_lite.F90 \
	  mo_math_constants_lite.F90 mo_turb_vdiff_params_lite.F90 \
	  mo_util_netcdf77_lite.F90 mo_util_string_lite.F90 mo_util_str12_lite.F90 mo_util_filename_lite.F90 \
	  mo_fortran_tools_lite.F90 mo_physical_constants_lite.F90 mo_impl_constants_grf_lite.F90 mo_impl_constants_lite.F90 mo_mpi_lite.F90 mo_parallel_config_lite.F90 \
	  mo_netcdf_lite.F90 mo_netcdf_parallel_lite.F90 \
	  mo_model_domain_lite.F90 mo_alloc_patches_lite.F90 mo_subm_trac_para_lite.F90 \
	  mo_aes_convect_tables_lite.F90 \
	  mo_run_config_lite.F90 mo_util_timeconv_lite.F90 \
	  mo_hammoz_lite_config.F90 mo_util_config_lite.F90 namelists/mo_subm_main_list_config_lite.F90 \
	  mo_subm_main_lite.F90 mo_util_extfield_lite.F90 mo_jsb_interface_lite.F90 mo_subm_bcs_lite.F90 mo_subm_jsbach_lite.F90 \
	  mo_subm_spec_lite.F90 mo_subm_trac_lite.F90 mo_subm_diag_lite.F90 \
	  namelists/mo_lite_main_list_config.F90 mo_ham_lite_main.F90 \
	  mo_ham_lite_rad_data.F90 mo_ham_lite_kappa.F90 mo_ham_lite_spec.F90 mo_ham_lite_misc.F90 \
	  namelists/mo_lite_dust_list_config.F90 mo_ham_lite_dust.F90 mo_ham_lite_seasalt.F90 \
	  mo_hammoz_lite_emi_mat.F90 mo_hammoz_lite_emi_vol.F90 mo_ham_lite_emi.F90 mo_hammoz_lite_emi.F90 \
	  mo_ham_lite_rad.F90 mo_ham_lite_init.F90 \
	  mo_ham_lite_drydep.F90 mo_hammoz_lite_drydep.F90 \
	  mo_hammoz_lite_act.F90 mo_ham_lite_wetdep.F90 mo_hammoz_lite_wetdep.F90 mo_ham_lite_act.F90 \
	  mo_ham_lite_sed.F90 mo_hammoz_lite_sed.F90 \
	  mo_subm_interface_lite.F90 mo_ham_lite_convert_concentrations.F90 hamlite_driver.F90
	  
#SRC = $(wildcard *.F90)

LIBS = -lnetcdf -lnetcdff

# Object files
OBJ = $(SRC:.F90=.o)

# Executable name
EXE = hamlite_box.exe

# Default target
all: $(EXE)

# Link executable
$(EXE): $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ) $(LIBS)

# Compile .F90 to .o
$(OBJ): %.o: %.F90
	$(FC) $(FFLAGS) -c $< -o $@

# Clean up
clean:
	rm -f $(OBJ) *.mod $(EXE)
