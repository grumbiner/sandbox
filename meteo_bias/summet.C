#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  gaussian<float> sum, sumsq, tmp;
  FILE *fin, *fout, *prev;
  int n;

  fin = fopen(argv[1],"r");
  prev = fopen(argv[2],"r");
  fout = fopen(argv[3],"w");

  if (prev != (FILE *) NULL && !feof(prev) ) {
    fread(&n,sizeof(int),1, prev);
  }
  else {
    n = 0;
  }
  if (n < 0) {
    n = 0;
    printf("had to manually reset\n");
  }


  n += 1;
  printf("n = %d\n",n);
  fwrite(&n,sizeof(int),1, fout);

  while (!feof(fin) ) {
    tmp.binin(fin);
    if (n != 1) {
      sum.binin(prev);
      sumsq.binin(prev);
    }
    else {
      sum.set((float) 0.);
      sumsq.set((float) 0.);
    }
    sum += tmp;
    sum.binout(fout);
    tmp *= tmp;
    sumsq += tmp;
    sumsq.binout(fout);
  }

  return 0;
}
