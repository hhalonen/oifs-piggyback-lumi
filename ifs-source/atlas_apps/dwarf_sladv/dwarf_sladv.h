!
!
! MACRO: ABORT
! ^^^^^^^^^^^^
!    use fckit_module
!    call ABORT( "something went wrong" )
!
#ifdef ABORT
#undef ABORT
#endif
#define ABORT( what )\
fckit_exception%abort(what,__FILENAME__,__LINE__)

