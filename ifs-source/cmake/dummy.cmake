if( NOT TARGET ifsdummy )

    ecbuild_info("[dummy]")

    ecbuild_list_add_pattern(LIST ifsdummy_src
        GLOB  dummy/ifsaux/*
              dummy/wam/*
              dummy/arpifs/*
    )

    if( HAVE_FORECAST_ONLY )
      ecbuild_list_add_pattern(LIST ifsdummy_src GLOB dummy/forecast/*)
    endif()

    ecbuild_add_library(TARGET ifsdummy
        SOURCES ${ifsdummy_src}
    )

    include(target_ignore_missing_symbols)
    target_ignore_missing_symbols( TARGET ifsdummy SYMBOLS
      abor1_
    )
  
endif()
