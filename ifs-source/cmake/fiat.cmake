
### Add contributed fiat if not already added via bundle
if( NOT TARGET fiat )
  ifs_propagate_flags( fiat )
  add_subdirectory( contrib/fiat )
endif()

### Find fiat

ecbuild_find_package( fiat REQUIRED )
