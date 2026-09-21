# Source me to get the correct configure/build/run environment
#
# LUMI-C (EuroHPC, CSC) -- GNU programming environment
#   LUMI/23.09 + cpeGNU/23.09  =>  gcc/gfortran 12.2.0
#
# Rationale: the ECMWF C++ dependencies pinned in bundle.yml (eckit 1.18.8,
# atlas 0.31.1, fdb 5.10.6) are 2022-era code that does not compile cleanly
# with gcc >= 13, and the HAMlite sources were developed with gfortran 11.3
# on Puhti. LUMI/23.09 is the newest LUMI stack that still provides gcc 12.

# Store tracing and disable (module is *way* too verbose)
{ tracing_=${-//[^x]/}; set +x; } 2>/dev/null

module_load() {
  echo "+ module load $*"
  module load $*
}
module_unload() {
  echo "+ module unload $*"
  module unload $*
}
module_purge() {
  echo "+ module purge"
  module --force purge
  # Reload the basic LUMI system settings module
  module load init-lumi
}

# Unload all modules to be certain (skipped when only setting up a run env)
[[ ${IFS_RUNTIME_ENV:-unset} == "unset" ]] && module_purge

# --- Programming environment -------------------------------------------------
module_load LUMI/23.09
module_load partition/C
module_load cpeGNU/23.09
module_load craype-x86-milan
module_load craype-network-ofi
module_load cray-mpich/8.1.27

# --- Build-time-only tools ---------------------------------------------------
if [[ ${IFS_RUNTIME_ENV:-unset} == "unset" ]]; then
  module_load buildtools/23.09     # cmake, ninja
  module_load cray-python          # eccodes MEMFS + ecbundle need python3
fi

# --- Numerical / IO libraries ------------------------------------------------
module_load cray-libsci
module_load cray-fftw
module_load cray-hdf5
module_load cray-netcdf
module_load libaec/1.0.6-cpeGNU-23.09

# Variables that CMake / ecbuild introspect. The Cray modules use their own
# names (CRAY_*_PREFIX), so map them onto what the Find modules expect.
export CRAY_LIBSCI=${CRAY_LIBSCI_PREFIX_DIR}/lib/libsci_gnu_mp.so
export FFTW_ROOT=${FFTW_ROOT}
export HDF5_ROOT=${CRAY_HDF5_PREFIX}
export NETCDF_ROOT=${CRAY_NETCDF_PREFIX}
export NetCDF_ROOT=${CRAY_NETCDF_PREFIX}
export AEC_ROOT=${EBROOTLIBAEC}
export MPI_HOME=${MPICH_DIR}

# --- Compilers ---------------------------------------------------------------
# Always use the Cray compiler wrappers, they know about MPI and the network
export CC=cc
export CXX=CC
export FC=ftn
export CRAY_ADD_RPATH=yes

export CMAKE_TOOLCHAIN_FILE=$PWD/toolchain.cmake

# Record the RPATH in the executable
export LD_RUN_PATH=$LD_LIBRARY_PATH

# IFS is a stack hog
ulimit -S -s unlimited

### Print some exported variables
echo "+ export FFTW_ROOT=${FFTW_ROOT}"
echo "+ export HDF5_ROOT=${HDF5_ROOT}"
echo "+ export NETCDF_ROOT=${NETCDF_ROOT}"
echo "+ export AEC_ROOT=${AEC_ROOT}"
echo "+ export MPI_HOME=${MPI_HOME}"
echo "+ export CMAKE_TOOLCHAIN_FILE=${CMAKE_TOOLCHAIN_FILE}"
echo "+ export CC=${CC}  CXX=${CXX}  FC=${FC}"

module list 2>&1

# Restore tracing to stored setting
{ if [[ -n "$tracing_" ]]; then set -x; else set +x; fi } 2>/dev/null
