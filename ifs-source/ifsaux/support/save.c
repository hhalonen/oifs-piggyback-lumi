#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>


void save_ (const char * p, const char * f, const int * p_len, const unsigned long int f_len)
{
  char _f[f_len+1];
  FILE * fp;

  memset (_f, 0, sizeof (_f));
  memcpy (_f, f, f_len);

  fp = fopen (_f, "w");
  fwrite (p, 1, *p_len, fp);
  fclose (fp);
}

void load_ (char * p, const char * f, int * p_len, const unsigned long int f_len)
{
  char _f[f_len+1];
  FILE * fp;
  struct stat st;
  int size;

  memset (_f, 0, sizeof (_f));
  memcpy (_f, f, f_len);

  stat (_f, &st);
  size = st.st_size;

  fp = fopen (_f, "r");
  fread (p, 1, size, fp);
  fclose (fp);

  *p_len = size;

}
