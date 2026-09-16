# Toolchain for LUMI-C with the GNU programming environment (cpeGNU/24.03, gcc 13.3)

set( CMAKE_POSITION_INDEPENDENT_CODE ON CACHE STRING "" )

####################################################################
# OpenMP
####################################################################

set( OpenMP_C_FLAGS           "-fopenmp" )
set( OpenMP_CXX_FLAGS         "-fopenmp" )
set( OpenMP_Fortran_FLAGS     "-fopenmp" )

####################################################################
# General flags (added to the defaults)
####################################################################

set( CMAKE_C_FLAGS   "${CMAKE_C_FLAGS} -O2 -DNDEBUG" )

# gcc >= 13 no longer includes <cstdint> transitively from other standard
# headers. The eckit/atlas/fdb versions pinned in bundle.yml use the fixed-width
# integer types without including it, so force-include it for all C++ sources.
set( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O2 -DNDEBUG -include cstdint" )

# --no-relax is required for the large IFS binaries on Cray systems
set( CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,--no-relax" )

####################################################################
# Libraries
####################################################################

# Cray LibSci provides BLAS/LAPACK. Point CMake straight at it so that
# FindBLAS/FindLAPACK do not go hunting for a system reference build.
set( BLAS_LIBRARIES   "$ENV{CRAY_LIBSCI}" CACHE STRING "BLAS_LIBRARIES"   FORCE )
set( LAPACK_LIBRARIES "$ENV{CRAY_LIBSCI}" CACHE STRING "LAPACK_LIBRARIES" FORCE )

# MPI is provided by the cc/CC/ftn compiler wrappers. Do NOT set
# MPI_<LANG>_COMPILER here: FindMPI recognises the craype wrappers on its
# own and resolves them to their full path. Setting the bare wrapper names
# makes FindMPI fail with "missing: MPI_C_LIB_NAMES MPI_C_HEADER_DIR",
# which silently disables eckit's MPI feature.
