#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  global_12th<float> sst;
  float limit;
  latpt ll;
  ijpt loc;
  int count = 0;

  limit = atof(argv[1]);
  fin = fopen(argv[2],"r");
  sst.binin(fin);

  if (sst.gridmax() > 200 && limit < 200) sst -= 273.15;
  if (sst.gridmax() < 200 && limit > 200) sst += 273.15;

  for (loc.j = 0; loc.j < sst.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < sst.xpoints(); loc.i++) {
     if (limit > 0 && sst[loc] > limit) {
       count += 1;
       ll = sst.locate(loc);
       printf("over limit at %f %f  with %f\n",ll.lat, ll.lon, sst[loc]);
     }
     if (limit < 0 && sst[loc] < limit) {
       count += 1;
       ll = sst.locate(loc);
       printf("under limit at %f %f  with %f\n",ll.lat, ll.lon, sst[loc]);
     }
  }
  }
  printf("%d points, limit %f\n",count, limit);

  return 0;
} 
