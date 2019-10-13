#include "ncepgrids.h"

int main(void) {
  llgrid<short int> etopo2(10800, 5400, -1./30., 1./30., 90.0, -180.0);
  llgrid<float> diffs(10800, 5400, -1./30., 1./30., 90.0, -180.0);
  llgrid<short int> dbdb2(10800, 5400, -1./30., 1./30., 90.0, -180.0);
  #define MINUTES 2
  llgrid<float> dest((360*60)/MINUTES+1, ((-60- -90)*60)/MINUTES+1,
                       MINUTES/60., MINUTES/60., -90, 0.0);

  FILE *fin;
  ijpt loc, oloc, l2;
  int count = 0;
  latpt ll;
  float flagval = -1, nonval = -99;


  fin = fopen("etopo2.raw","r");
  etopo2.binin(fin);
  fclose(fin);

  fin = fopen("dbdb2.raw","r");
  dbdb2.binin(fin);
  fclose(fin);

  fin = fopen("reformed.aa","r");
  dest.binin(fin);
  fclose(fin);

  int limit = 0;

  for (loc.j = 4000; loc.j < etopo2.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < etopo2.xpoints(); loc.i++) {
    ll = etopo2.locate(loc);
    oloc = loc;
    l2 = dest.locate(ll);
    if (fabs((double) dbdb2[oloc] - etopo2[loc]) > limit && ll.lat < -65.0) {
      if (dest[l2] > 0.) { 
      printf("%5d %4d  %6d %6d %6.1f  %7.1f %7.1f %1d  %7.3f %8.3f\n",loc.i, loc.j, 
           etopo2[loc], dbdb2[oloc], dest[l2],
           etopo2[loc] - dest[l2], dbdb2[oloc] - dest[l2],
           fabs(etopo2[loc] - dest[l2]) > fabs(dbdb2[oloc] - dest[l2]),
           ll.lat, ll.lon);
      }
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
  dest.scale();
  dest.xpm("aa.xpm",7,gg);
 
  return 0;
}
