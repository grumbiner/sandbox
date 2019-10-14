#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <ctype.h>

void cread_(float *x, int *nx, int *ny, char *fname, int slen)
{
  FILE *fin;
  char *fn2;
  int i, j, npts;
  char *cin;

  printf("%d\n", slen);
  fn2 = malloc(slen*sizeof(char));

  for (i = 0; i < slen; i++) {
    if (isalpha(fname[i]) ) { fn2[i] = fname[i] ; }
    else {break;}
  }
  fn2[i+1] = "\0";
  printf("%d, %s\n", i, fn2);

  fin = fopen(fn2, "r");
  if (fin == NULL) {
    printf("failed to open %s\n", fname);
    perror(fname);
    return;
  }

  printf("%d %d\n", *nx, *ny);
  npts = *nx * (*ny) ;
  cin = malloc(sizeof(unsigned char)*npts);
  i = fread( (unsigned char *) cin, sizeof(unsigned char), npts, fin);
  printf(" read %d\n", i);
  free (fn2);

  for (i = 0; i < npts; i++) {
    x[i] = (float) cin[i];
  }

  free(cin);
  return;
}

void cwrite_(float *x, int *nx, int *ny, char *fname, int slen)
{
  FILE *fout;
  char *fn2;
  int i, j, npts;
  char *cout;

  printf("%d\n", slen);
  fn2 = malloc(slen*sizeof(char));

  for (i = 0; i < slen; i++) {
    if (isalpha(fname[i]) ) { fn2[i] = fname[i] ; }
    else {break;}
  }
  fn2[i+1] = "\0";
  printf("%d, %s\n", i, fn2);

  fout = fopen(fn2, "w");
  if (fout == NULL) {
    printf("failed to open %s\n", fname);
    perror(fname);
    return;
  }

  printf("%d %d\n", *nx, *ny);
  npts = *nx * (*ny) ;
  cout = malloc ( sizeof(unsigned char) * npts);
  for (i = 0; i < npts; i++) {
    cout[i] = (unsigned char) (x[i]+0.5);
  }
  fwrite(cout, sizeof(unsigned char), npts, fout);

  free(cout);
  free(fn2);
  return;
}
