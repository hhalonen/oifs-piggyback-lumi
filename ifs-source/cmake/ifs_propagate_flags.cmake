function( ifs_propagate_flags proj )
  # Propagate flags defined for IFS in cmake/compile_flags.cmake
  # to downstream project <proj>

  string( TOUPPER ${proj} PROJ )
  foreach( lang Fortran C CXX )
    foreach( flags "${lang}_FLAGS" "${lang}_FLAGS_DEBUG" "${lang}_FLAGS_BIT" )
       if( ${PNAME}_${flags} AND NOT ${PROJ}_${flags} )
         ecbuild_debug( "ifs_propagate_flags -- Setting ${PROJ}_${flags}: ${${PNAME}_${flags}}")
         set( ${PROJ}_${flags} ${${PNAME}_${flags}} PARENT_SCOPE )
       endif()
    endforeach()
  endforeach()

endfunction()

