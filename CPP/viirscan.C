#include "ncepgrids.h"

int main(void) {
  grid2<float> lat(3200, 768), lon(3200, 768), conc(3200, 768), temp(3200, 768);
  global_12th<float> rganaly, accum, t2, ccount, tcount;
  int i;
  FILE *fin;
  fin = fopen("fred","r");
  float flag = 9999.;

  lat.binin(fin);
  lon.binin(fin);
  conc.binin(fin);
  temp.binin(fin);
  fclose(fin);

  ijpt loc;
  latpt ll;
  fin = fopen("eta","r");
  rganaly.ftnin(fin);
  fclose(fin);
  accum.set((float) 0);
  t2.set((float) 0);
  ccount.set((float) 0);
  tcount.set((float) 0);


  for (i = 0; i < lat.xpoints()*lat.ypoints(); i++) {
    if (conc[i] != flag ) {
      ll.lat = lat[i]; ll.lon = lon[i];
      loc = rganaly.locate(ll);
//      printf("%f %f  %f %f  %f\n",lon[i], lat[i], conc[i], temp[i], rganaly[loc]*100);
      accum[loc] += conc[i];
      ccount[loc] += 1;
      if (temp[i] != flag) {
        t2[loc]    += temp[i];
        tcount[loc] += 1;
      }
    }
  }


  for (i = 0; i < ccount.xpoints()*ccount.ypoints(); i++) {
    if (ccount[i] != 0) {
      accum[i] /= ccount[i];
    }
    if (tcount[i] != 0) {
      t2[i] /= tcount[i];
    }
  }
  for (loc.j = 0; loc.j < ccount.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ccount.xpoints(); loc.i++) {
    if (ccount[loc] != 0) {
      ll = ccount.locate(loc);
      printf("%7.3f %7.3f  %6.2f  %5.1f %5.1f  %6.1f\n",ll.lon, ll.lat, t2[loc], accum[loc], rganaly[loc]*100, accum[loc] - 100.*rganaly[loc]);
    }
  }
  }


  return 0;
}
