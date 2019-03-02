#include <stdio.h>

int main(void)
{
  char nmap[465*385], smap[355*345];
  FILE *n, *s;

  int i;

  for (i = 0; i < 465*385 ; i++)
  {
    nmap[i] = 0;
  }
  for ( i = 0; i < 355*345; i++)
  {
    smap[i] = 0;
  }


  n = fopen("nzero", "w");
  s = fopen("szero", "w");
  fwrite(nmap, sizeof(char), 465*385, n);
  fwrite(smap, sizeof(char), 355*345, s);

  return 0;

}
