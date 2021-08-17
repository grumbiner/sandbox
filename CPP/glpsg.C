#include "ncepgrids.h"
#include "legacy.h"

int main(void) {
  psgrid<float> great(1500,1000,1000, 5000,45.0, -10.0, 1.0,1.0e3,1.0e3);
  great_lakes_1km<float> x;
  global_12th<unsigned char> mask;
  global_12th<float> fmask;
  palette<unsigned char> gg(19,65);
  FILE *fin;
  ijpt loc;
  latpt ll, ll2;

  fin = fopen("global_5min","r");
  mask.binin(fin);
  fclose(fin);

  conv(mask, fmask);
  x.fromall(fmask, fmask, 157, 157);
  x.xpm("gl.xpm",14,gg);

  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    ll = x.locate(loc);
    ll2 = great.locate(loc);
    //if (ll.lon != ll2.lon || ll.lat != ll2.lat) {
    if (ll.lat != ll2.lat) {
      printf("%4d %4d %f %f   %f\n",loc.i, loc.j, 
                ll.lat, ll2.lat, ll.lat - ll2.lat); 
    }
  }
  }

  return 0;
}
