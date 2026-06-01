#include <stdio.h>

/*
 * Used to get a string representation of an address from FORTRAN.
 * Useful to generate unique ids.
 */

void paddrs_ (char * s, void * p, int slen)
{
  int i, n = snprintf (s, slen, "%p", p);
  for (i = n; i < slen; i++) s[i] = ' ';
}
