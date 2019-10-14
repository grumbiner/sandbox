#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <malloc.h>
#include <ctype.h>

void cread_(char *x, int *nf, int *nx, int *ny, char *fname, int slen)
{
  FILE *fin;
  char *fn2;
  int i, j, npts;

  printf("%d\n", slen); 
  fn2=malloc(slen*sizeof(char));
  
  for (i = 0; i < slen; i++) {
    if ( isalpha(fname[i]) ) { fn2[i] = fname[i];}
    else {break;}
  }
  fn2[i+1] = "\0"; 

  printf("%d, %s\n", i, fn2);

  fin = fopen(fn2,  "r");
  if (fin == NULL) {
    printf("failed to open %s\n",fname);
    perror(fname);
    return;
  }

  printf("%d %d\n", *nx, *ny);
  npts = (*nx)*(*ny);

  *nf = 0;
  i = fread ((void *) x, sizeof(unsigned char), npts, fin);
  printf("read %d \n",i);
  if (i == npts ) {
    *nf += 1;
  }
  else {
    return;
  }

  
  return;
}

