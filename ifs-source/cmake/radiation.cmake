ecbuild_info("[radiation]")


ecbuild_add_library(TARGET radiation.${PREC}
  SOURCES_GLOB radiation/module/*
  PRIVATE_INCLUDES ${NETCDF_INCLUDE_DIRS}
  LIBS arpifs.${PREC} ${IFSAUX_LIBRARIES} NetCDF::NetCDF_Fortran
  CONDITION FALSE) # FIXME[IFS-HHH]: circular dependency between ifs & radiation
