#include "ncepgrids.h"

int main(void) {
  llgrid<short int> etopo2(10800, 5400, -1./30., 1./30., 90.0, -180.0);
  llgrid<float> diffs(10800, 5400, -1./30., 1./30., 90.0, -180.0);
  //llgrid<short int> dbdb2(10800, 5401, +1./30., 1./30., -90.0, 0.0);
  llgrid<short int> dbdb2(10800, 5400, -1./30., 1./30., 90.0, -180.0);
  FILE *fin;
  ijpt loc, oloc;
  int count = 0;
  latpt ll;

  fin = fopen("etopo2.raw","r");
  etopo2.binin(fin);
  fclose(fin);

  fin = fopen("dbdb2.raw","r");
  dbdb2.binin(fin);
  fclose(fin);

  int limit = 0;

  for (loc.j = 0; loc.j < etopo2.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < etopo2.xpoints(); loc.i++) {
    ll = etopo2.locate(loc);
    //oloc = dbdb2.locate(ll);
    oloc = loc;
    if (fabs((double) dbdb2[oloc] - etopo2[loc]) > limit) {
      printf("%5d %4d  %6d %6d %5d  %7.3f %8.3f\n",loc.i, loc.j, 
           etopo2[loc], dbdb2[oloc],
           dbdb2[oloc] - etopo2[loc], ll.lat, ll.lon);
      count += 1;
    }
    diffs[loc] = dbdb2[oloc] - etopo2[loc];
  }
  }
  printf("%d differences over %d\n",count, limit);
  palette<unsigned char> gg(19, 65);
  printf("max, min, avg rms differences %f %f %f %f\n",diffs.gridmax(), 
              diffs.gridmin(), diffs.average(), diffs.rms() );
  diffs.scale();
  diffs.xpm("dif.xpm", 7, gg);
 
  return 0;
}
