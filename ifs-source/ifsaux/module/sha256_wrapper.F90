module sha256_wrapper

use, intrinsic :: iso_c_binding

implicit none

interface
  subroutine sha256_hash(message, n, sha, block, len_block, action) bind(c)
    use iso_c_binding
    type(c_ptr), value :: message
    integer(c_int64_t), value :: n
    integer(c_int32_t) :: sha(8)
    integer(c_int8_t) :: block(64)
    integer(c_int64_t) :: len_block
    integer(c_int32_t), value :: action
  end subroutine sha256_hash
end interface

end module sha256_wrapper

