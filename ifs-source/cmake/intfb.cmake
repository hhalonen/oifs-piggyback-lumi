if( NOT HAVE_OPENIFS_ONLY )
  list( APPEND aladin_include     
    aladin/adiab
    aladin/control
    aladin/dia
    aladin/coupling
    aladin/c9xx
    aladin/fullpos
    aladin/interpol
    aladin/nudging
    aladin/parallel
    aladin/setup
    aladin/sinvect
    aladin/transform
    aladin/utility
    aladin/var
    aladin/wavelet
    arpifs/io_serv
    arpifs/fp_serv
    arpifs/var)
endif()

if( NOT TARGET arpifs_intfb )
  ecbuild_generate_fortran_interfaces(

    TARGET arpifs_intfb

    DIRECTORIES
        
        ${aladin_include}

        arpifs/adiab
        arpifs/c9xx
        arpifs/canari
        arpifs/chem
        arpifs/climate
        arpifs/clradlid
        arpifs/cma2odb
        arpifs/control
        arpifs/dfi
        arpifs/dia
        arpifs/fullpos
        arpifs/gbrad
        arpifs/glomap_mode
        arpifs/interpol
        arpifs/kalman
        arpifs/m7
        arpifs/mwave
        arpifs/nemo
        arpifs/obs_error
        arpifs/obs_preproc
        arpifs/ocean
        arpifs/oops
        arpifs/op_obs
        arpifs/parallel
        arpifs/phys_dmn
        arpifs/phys_ec
        arpifs/phys_radi
        arpifs/pp_obs
        arpifs/raingg
        arpifs/sekf
        arpifs/setup
        arpifs/sinvect
        arpifs/smos
        arpifs/transform
        arpifs/utility
        arpifs/var

    SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}
    DESTINATION arpifs
    INCLUDE_DIRS arpifs_intfb_includes
    PARALLEL ${FCM_PARALLEL}
  )
endif()

if( HAVE_OPENIFS_ONLY )
  if( NOT TARGET openifs_intfb ) 
    ecbuild_generate_fortran_interfaces(
      TARGET openifs_intfb
      DIRECTORIES dummy
      SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/openifs
      DESTINATION openifs
      INCLUDE_DIRS openifs_intfb_includes
      PARALLEL ${FCM_PARALLEL}  
    )
  endif() 
endif()


if( NOT TARGET wam_intfb )
  ecbuild_generate_fortran_interfaces(
    TARGET wam_intfb
    DIRECTORIES Wam_oper
    SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/wam
    DESTINATION wam
    INCLUDE_DIRS wam_intfb_includes
    PARALLEL ${FCM_PARALLEL}
  )
endif()

if( NOT TARGET scmec_intfb )
  ecbuild_generate_fortran_interfaces(
    TARGET scmec_intfb
    DIRECTORIES source 
    SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/scmec
    DESTINATION scmec
    INCLUDE_DIRS scmec_intfb_includes
    PARALLEL ${FCM_PARALLEL}
  )
endif()
