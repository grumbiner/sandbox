#include <stdio.h>
#include <stdlib.h>
#include "amsr.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  amsrept *x;
  double maxlat = -90, minlat = +90.0;
  int i;
  
  fin = fopen(argv[1],"r");
  
  x = (amsrept *) malloc(N*sizeof(amsrept));
  while (!feof(fin)) {
    fread(x, sizeof(amsrept), N, fin);
    for (i = 0; i < N; i++) {
      printf("satid = %d\n",x[i].satid);
      if (x[i].clat > maxlat) {
        maxlat = x[i].clat;
        printf("new maxlat = %f\n",(float) maxlat);
      }
      if (x[i].clat < minlat) {
        minlat = x[i].clat;
        printf("new minlat = %f\n",(float) minlat);
      }
    }
  }

  //printf("size of amsrept = %d\n",sizeof(amsrept) );
  //printf("N = %d\n",N);

  return 0;
}
