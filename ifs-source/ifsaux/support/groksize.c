
/*
 * This routine is used to find out the size(in bytes) of a fortran entity.
 * Example : 
 * TYPE(TOTO) :: T(2)
 * CALL GROKSIZE (KSIZE, T(1), T(2))
 */
void groksize_ (int * ksize, const char * c1, const char * c2)
{
  *ksize = c1 - c2;
}
