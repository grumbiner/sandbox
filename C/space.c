#include <stdio.h>
#include <stdlib.h>

/* Test of space allocation */
/* Robert Grumbine 2 Oct 1995 */
/* Extended for systems over 128 Mb allocatable, raise to 16 Gb,
 * 13 July 2004 */

int main(void)
{
  int i;
  int b;
  float *a;

  for (i = 1; i < 1024 * 16; i++)
  {
    printf("i = %d\n",i);
    a = malloc(sizeof(b)*1024*1024*i);
    if (a == NULL) {
      printf("failed malloc at i = %d\n",i);
      return -1;
    }
    else {
     free(a);
    }
  }

  return 0;
}
