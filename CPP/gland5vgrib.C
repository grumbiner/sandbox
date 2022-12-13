#include "ncepgrids.h"

template <class T>
void histo(grid2<T> &x, mvector<int> &histogram) ;

int main(int argc, char *argv[]) {
  FILE *fin1, *fin2;
  global_12th<float> low;
  global_12th<unsigned char> high;
  mvector<int> histogram(256);
  int i;

  fin1 = fopen(argv[1], "r");
  fin2 = fopen(argv[2], "r");
  low.binin(fin1);
  high.binin(fin2);
  fclose(fin1);
  fclose(fin2);
  if (low.gridmax() < 100) low *= 100.;

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

  ijpt loc;
  latpt ll;
  for (loc.j = 0; loc.j < low.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < low.xpoints(); loc.i++) {
    if (low[loc] != high[loc] ) {
      ll = low.locate(loc);
      printf("%4d %4d  %7.3f %7.3f  %3d %3d\n",loc.i, loc.j, ll.lat, ll.lon, (int) low[loc], high[loc]);
    }
  }
  }

  return 0;
}


template <class T>
void histo(grid2<T> &x, mvector<int> &histogram) {
  ijpt loc;
  histogram = 0;

  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    histogram[ x[loc] ] += 1;
  }
  }

  return;
}
