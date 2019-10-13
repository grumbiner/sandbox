#include "ncepgrids.h"

#define DAYS 182

int main(int argc, char *argv[]) {
  global_12th<unsigned char> ages[182];
  mvector<int> histogram(24);
  int i, j;
  FILE *fin;

  histogram = 0;

  for (i = 0; i < DAYS; i++) {
    fin = fopen(argv[i+1], "r");
    ages[i].binin(fin);
    fclose(fin);
  }

  for (i = 1; i < DAYS; i++) {
    for (j = 0; j < ages[0].xpoints()*ages[0].ypoints(); j++) {
      if (ages[i][j] == 0) {
        histogram[ages[i-1][j] ] += 1;
      }
    }
  }

  for (i = 0; i < histogram.xpoints(); i++) {
    if (histogram[i] != 0) {
      printf("%2d %d\n",i,histogram[i]);
    }
  }
    
  return 0;
}  
