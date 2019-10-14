#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_12th<unsigned char> mask;
  latpt ll, ur;
  ijpt ill, iur, loc;
  FILE *fin;
  latpt fll, fur;

  fin = fopen(argv[1],"r");
  mask.binin(fin);
  fclose(fin);
 
//Note that the lower left latitude is not what you might expect.
//This is because in ij space, a high latitude is a low j for this
//  grid.
  ll.lat = 50.0;
  ll.lon = 267.0;
  ur.lat = 40.0;
  ur.lon = 285.0;
  ill = mask.locate(ll);
  iur = mask.locate(ur);
  printf("Corner points are: %d %d  %d %d\n",ill.i, ill.j, iur.i, iur.j);
  fll = mask.locate(ill);
  fur = mask.locate(iur);
  printf("Corner points are: %f %f  %f %f\n",fll.lat, fll.lon, fur.lat, fur.lon);


  for (loc.j = ill.j; loc.j <= iur.j; loc.j++) {
    for (loc.i = ill.i; loc.i <= iur.i; loc.i++) {
      if (mask[loc] > 0) mask[loc] = 1;
      printf("%d ",mask[loc]);
    }
    printf("\n");
  }

  return 0;
}
