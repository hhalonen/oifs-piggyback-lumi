! The IFS does not use the "PSRAD" capability of the radiation scheme;
! it is hidden behind #ifdef...#endif in radiation_interface.
! However, the interface generation script does not know about
! preprocessor directives, so believes that radiation_interface
! depends on a module radiation_psrad_rrtm.mod.  Therefore we need to
! create a dummy mod file that won't actually be used.

module radiation_psrad_rrtm
  integer :: this_will_never_be_used
end module radiation_psrad_rrtm
