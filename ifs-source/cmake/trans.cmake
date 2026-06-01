
### Add contributed ectrans if not already added via bundle

if( NOT TARGET trans_dp AND NOT TARGET trans_sp )
  ifs_propagate_flags( ectrans )
  set( ECTRANS_ENABLE_SINGLE_PRECISION ON )
  set( ECTRANS_ENABLE_DOUBLE_PRECISION ON )
  add_subdirectory( contrib/ectrans )
endif()

### Find ectrans

if( HAVE_SINGLE_PRECISION )
  set( ectrans_precision single )
else()
  set( ectrans_precision double )
endif()
ecbuild_find_package( ectrans REQUIRED COMPONENTS ${ectrans_precision} )

### create alias library

add_library(trans.${PREC} ALIAS trans_${prec} )


### Build programs that have not yet been dealt with

foreach( program IN ITEMS gpscalar_cos gpwind_cos rgrid )

  if( NOT TARGET ${program} )
    ecbuild_add_executable(TARGET ${program}
      SOURCES trans/programs/${program}.F90
      LIBS trans.${PREC} ifsaux.${PREC} eccodes_f90 
      LINKER_LANGUAGE Fortran)
  endif()

endforeach()
