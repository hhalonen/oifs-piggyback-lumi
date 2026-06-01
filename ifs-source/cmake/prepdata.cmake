ecbuild_info("[prepdata]")


if( NOT TARGET getres )
  ecbuild_add_executable(TARGET getres
    SOURCES prepdata/programs/getres.F
    LINKER_LANGUAGE Fortran
    LIBS ${IFSAUX_LIBRARIES} ${ECCODES_LIBRARIES})

  set_property(TARGET getres PROPERTY SERIAL TRUE)
endif()

if( NOT TARGET gptosp )
  ecbuild_add_executable(TARGET gptosp
    SOURCES prepdata/programs/gptosp.F90
    LINKER_LANGUAGE Fortran
    LIBS trans.${PREC} ${IFSAUX_LIBRARIES} ${ECCODES_LIBRARIES})

  set_property(TARGET gptosp PROPERTY SERIAL TRUE)
endif()

if( NOT TARGET grib_set_vtable.exe )
  ecbuild_add_executable(TARGET grib_set_vtable.exe
    SOURCES prepdata/programs/grib_set_vtable.F90
    LINKER_LANGUAGE Fortran
    LIBS ${IFSAUX_LIBRARIES} ${ECCODES_LIBRARIES})
endif()

if( NOT TARGET intsst )
  ecbuild_add_executable(TARGET intsst
    SOURCES prepdata/programs/intsst.F90
    LINKER_LANGUAGE Fortran
    LIBS trans.${PREC} ${IFSAUX_LIBRARIES} ${ECCODES_LIBRARIES})

  set_property(TARGET intsst PROPERTY SERIAL TRUE)
endif()

if( NOT TARGET sptogp )
  ecbuild_add_executable(TARGET sptogp
    SOURCES prepdata/programs/sptogp.F90
    LINKER_LANGUAGE Fortran
    LIBS trans.${PREC} ${IFSAUX_LIBRARIES} ${ECCODES_LIBRARIES})
endif()

if( NOT TARGET timeint )
  ecbuild_add_executable(TARGET timeint
    SOURCES prepdata/programs/timeint.F90
    LINKER_LANGUAGE Fortran
    LIBS ${IFSAUX_LIBRARIES} ${ECCODES_LIBRARIES})

  set_property(TARGET timeint PROPERTY SERIAL TRUE)
endif()

if( NOT TARGET uvtovod )
  ecbuild_add_executable(TARGET uvtovod
    SOURCES prepdata/programs/uvtovod.F90
    LINKER_LANGUAGE Fortran
    LIBS trans.${PREC} ${IFSAUX_LIBRARIES} ${ECCODES_LIBRARIES})
endif()

if( NOT TARGET vod2uv )
  ecbuild_add_executable(TARGET vod2uv
    SOURCES prepdata/programs/vod2uv.F90
    LINKER_LANGUAGE Fortran
    LIBS trans.${PREC} ${IFSAUX_LIBRARIES} ${ECCODES_LIBRARIES})
endif()








