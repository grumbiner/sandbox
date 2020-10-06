#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  global_quarter<float> x;
  int i;
  ijpt loc1, loc2;
  latpt ll;

  ll.lat = 42.0; ll.lon = 160.0;
  loc1 = x.locate(ll);
  printf("%f %f  %d %d\n",ll.lat, ll.lon, loc1.i, loc1.j);

  ll.lat = 25.0; ll.lon = -130.0;
  loc2 = x.locate(ll);
  printf("%f %f  %d %d\n",ll.lat, ll.lon, loc2.i, loc2.j);

  for (i = 1; i < argc; i++) {
    fin = fopen(argv[i],"r");
    x.binin(fin);
    fclose(fin);
    printf("%d %f %f\n",i,x[loc1],x[loc2]);
  }

  return 0;
}
