ecbuild_info("[algor]")

ecbuild_add_library(TARGET algor.${PREC}
  LINKER_LANGUAGE Fortran
  DEFINITIONS ${IFS_DEFINITIONS}
  SOURCES_GLOB algor/*
  SOURCES_EXCLUDE_REGEX algor/external/minim/* algor/internal/minim/*
  PUBLIC_INCLUDES $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/algor/interface>
  PUBLIC_LIBS  ${IFSAUX_LIBRARIES}
  PRIVATE_LIBS ${LAPACK_LIBRARIES})
