MODULE mo_lib_loopindices_lite
  IMPLICIT NONE
  PUBLIC :: get_indices_c_lib, get_indices_e_lib, get_indices_v_lib
CONTAINS
  SUBROUTINE get_indices_c_lib(i_startidx_in, i_endidx_in, nproma, i_blk, &
       & i_startblk, i_endblk, i_startidx, i_endidx)
    INTEGER, INTENT(IN)  :: i_startidx_in, i_endidx_in, nproma, i_blk, i_startblk, i_endblk
    INTEGER, INTENT(OUT) :: i_startidx, i_endidx
    i_startidx = 1
    i_endidx = nproma
    IF (i_blk == i_startblk) i_startidx = i_startidx_in - (i_startblk-1)*nproma
    IF (i_blk == i_endblk)   i_endidx   = i_endidx_in   - (i_endblk-1)*nproma
  END SUBROUTINE get_indices_c_lib
  SUBROUTINE get_indices_e_lib(i_startidx_in, i_endidx_in, nproma, i_blk, &
       & i_startblk, i_endblk, i_startidx, i_endidx)
    INTEGER, INTENT(IN)  :: i_startidx_in, i_endidx_in, nproma, i_blk, i_startblk, i_endblk
    INTEGER, INTENT(OUT) :: i_startidx, i_endidx
    i_startidx = 1
    i_endidx = nproma
    IF (i_blk == i_startblk) i_startidx = i_startidx_in - (i_startblk-1)*nproma
    IF (i_blk == i_endblk)   i_endidx   = i_endidx_in   - (i_endblk-1)*nproma
  END SUBROUTINE get_indices_e_lib
  SUBROUTINE get_indices_v_lib(i_startidx_in, i_endidx_in, nproma, i_blk, &
       & i_startblk, i_endblk, i_startidx, i_endidx)
    INTEGER, INTENT(IN)  :: i_startidx_in, i_endidx_in, nproma, i_blk, i_startblk, i_endblk
    INTEGER, INTENT(OUT) :: i_startidx, i_endidx
    i_startidx = 1
    i_endidx = nproma
    IF (i_blk == i_startblk) i_startidx = i_startidx_in - (i_startblk-1)*nproma
    IF (i_blk == i_endblk)   i_endidx   = i_endidx_in   - (i_endblk-1)*nproma
  END SUBROUTINE get_indices_v_lib
END MODULE mo_lib_loopindices_lite
