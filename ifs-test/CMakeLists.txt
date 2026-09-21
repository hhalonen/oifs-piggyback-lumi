# (C) Copyright 1996-2015 ECMWF.

############################################################################################
# ifs_test project

cmake_minimum_required( VERSION 3.6 FATAL_ERROR )

find_package( ecbuild 3.1 REQUIRED )

project( ifs_test )

find_package( ifs REQUIRED )
find_package( eccodes )

ecbuild_info( "IFS_EXECUTABLE: ${IFS_EXECUTABLE}" )
ecbuild_info( "IFS_CYCLE: ${IFS_CYCLE}" )
ecbuild_info( "IFS_BLACKLIST_LIBRARY_PATH: ${IFS_BLACKLIST_LIBRARY_PATH}")
ecbuild_info( "IFS_PRECISION: ${IFS_PRECISION}")
ecbuild_info( "ECCODES_DEFINITION_PATH: ${ECCODES_DEFINITION_PATH}" )
ecbuild_info( "ECCODES_IFS_SAMPLES_PATH: ${ECCODES_IFS_SAMPLES_PATH}" )

#################################################################
# Configuration

set( SHARE_BINARY_DIR ${CMAKE_BINARY_DIR}/share/ifs_test )
set( SHARE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/share/ifs_test )
execute_process(COMMAND ${CMAKE_COMMAND} -E make_directory ${SHARE_BINARY_DIR} )

set( IFS_NAMELIST ${SHARE_SOURCE_DIR}/namelist )

if( IFS_CHECK_BOUNDS)
  set( IFS_NUNDEFLD "1")
else()
  set( IFS_NUNDEFLD "-99999")
endif()

if( CMAKE_SYSTEM_NAME MATCHES "Darwin")
  set( LD_LIBRARY_PATH_VAR "DYLD_LIBRARY_PATH" )
else()
  set( LD_LIBRARY_PATH_VAR "LD_LIBRARY_PATH" )
endif()

configure_file(${SHARE_SOURCE_DIR}/ifs_env.in ${SHARE_BINARY_DIR}/ifs_env )

#################################################################
# Tests

add_subdirectory(tests)

