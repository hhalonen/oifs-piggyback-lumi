ecbuild_add_library(
    TARGET  dwarf_mpdata.${PREC}
    SOURCES
        advection_MPDATA_auxiliary_module.F90
        advection_MPDATA_geometry_module.F90
        advection_MPDATA_limiter_module.F90
        advection_MPDATA_mappings_module.F90
        advection_MPDATA_MPDATA_module.F90
        advection_MPDATA_nabla_module.F90
        advection_MPDATA_topology_module.F90
        dwarf_mpdata_module.F90
    PUBLIC_LIBS
        fckit
        atlas_f
        dwarf_util
        ${IFS_OMP_Fortran_LIBRARIES}
)

if( HAVE_SINGLE_PRECISION )
  target_compile_definitions( dwarf_mpdata.${PREC} PRIVATE "-DHAVE_SINGLE_PRECISION" )
endif()
