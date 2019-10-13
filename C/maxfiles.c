#include <stdio.h>
/* Brute force check on how many files can be opened on a system */
/* Robert Grumbine 1 May 2008 original */

int main(void) {
  FILE *fin[1000000];
  int i;
  char fname[900];

  for (i = 0; i < 1000000; i++) {
    printf("%d\n",i);
    sprintf(fname,"file.%d",i);
    fin[i] = fopen(fname,"w");
    if (fin[i] == (FILE*) NULL) {
      printf("failed on %d\n",i);
      return 1;
    }
  }
 
  return 0;
}
