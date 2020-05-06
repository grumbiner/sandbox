#include <stdlib.h>
#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  fijpt ll, ur;
  ijpt ill, iur;
  latpt loc1, loc2;
  northhigh<unsigned char> north;
  southhigh<unsigned char> south;
  psgrid<unsigned char> tmp;
  bool ok;
  palette<unsigned char> gg(19, 65);
  FILE *fin;

  loc1.lon = atof(argv[1]);
  loc1.lat = atof(argv[2]);
  loc2.lon = atof(argv[3]);
  loc2.lat = atof(argv[4]);
  fin = fopen("nland.new", "r");
  north.binin(fin);
  fclose(fin);
  fin = fopen("sland.new", "r");
  south.binin(fin);
  fclose(fin);

  if (loc1.lat > 0. && loc2.lat > 0.) {
    ll = north.locate(loc1);
    ur = north.locate(loc2);
    ok = north.in(ll) && north.in(ur);
  }
  else if (loc1.lat < 0. && loc2.lat < 0.) {
    ll = south.locate(loc1);
    ur = south.locate(loc2);
    ok = south.in(ll) && south.in(ur);
  }
  else {
    printf("Cannot deal with divided hemispheres on input points\n");
  }

  if (ok) {
    printf("Corners %7.2f %7.2f  %7.2f %7.2f\n", min(ll.i,ur.i), min(ll.j,ur.j), 
                                                 max(ur.i,ll.i), max(ur.j,ll.j)  );
    ill = ll;
    iur = ur;
    tmp.subset(north, ill, iur);
    tmp.xpm("a.xpm", 12, gg);
  }
  else {
    printf("Corner(s) falls outside region\n");
  }

  return 0;
}
