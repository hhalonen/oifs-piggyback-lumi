module dwarf_module
implicit none
private
!------------------------------------------------------------------------------

type, public, abstract :: dwarf
  !! An abstract base type for a command type that holds only a method 
  !!    dwarf%execute( args ) with args a derived type of dwarf_args
contains
  procedure(dwarf_execute), deferred, public :: execute
  procedure(dwarf_final),   deferred, public :: final
end type

!------------------------------------------------------------------------------

type, public :: dwarf_args
  !! An abstract base type for argument to a dwarf%execute()
end type

!------------------------------------------------------------------------------

interface

  subroutine dwarf_execute( this, args )
    import dwarf
    import dwarf_args
    class(dwarf),      intent(inout) :: this
    class(dwarf_args), intent(inout) :: args
  end subroutine

  subroutine dwarf_final( this )
    import dwarf
    class(dwarf), intent(inout) :: this
  end subroutine
  
end interface

!------------------------------------------------------------------------------

end module

