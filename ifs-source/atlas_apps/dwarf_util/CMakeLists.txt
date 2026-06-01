if( NOT TARGET dwarf_util )
    ecbuild_add_library(
        TARGET dwarf_util
        SOURCES
            dwarf_module.F90
    )
    # Make sure that the module directory is precision indendent
    include(target_fortran_module_directory)
    target_fortran_module_directory( TARGET dwarf_util MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/module/atlas_apps )
endif()
