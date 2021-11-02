#include "ncepgrids.h"
// 25 June 2014
// Construct histogram of flags for a posteriori filtering
// Robert Grumbine

void histo(grid2<unsigned char> &x, mvector<int> &histogram) ;

int main(int argc, char *argv[]) {
  FILE *fin1, *fin2;
  global_ice<unsigned char> low;
  global_12th<unsigned char> high;
  mvector<int> histogram(256);
  int i;

  fin1 = fopen(argv[1], "r");
  fin2 = fopen(argv[2], "r");
  low.binin(fin1);
  high.binin(fin2);
  fclose(fin1);
  fclose(fin2);

  histo(low, histogram);
  for (i = 0; i < histogram.xpoints(); i++ ) {
    if (histogram[i] != 0) {
      printf("low %3d  %6d\n",i,histogram[i]);
    }
  }
  
  histo(high, histogram);
  for (i = 0; i < histogram.xpoints(); i++ ) {
    if (histogram[i] != 0) {
      printf("high %3d  %7d\n",i,histogram[i]);
    }
  }

  return 0;
}


void histo(grid2<unsigned char> &x, mvector<int> &histogram) {
  ijpt loc;
  histogram = 0;

  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    histogram[ x[loc] ] += 1;
  }
  }

  return;
}
