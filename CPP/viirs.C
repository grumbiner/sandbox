#include "ncepgrids.h"

int main(void) {
  grid2<float> lat(3200, 768), lon(3200, 768), conc(3200, 768), temp(3200, 768);
  global_12th<float> rganaly, accum, count;
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
  count.set((float) 0);


  for (i = 0; i < lat.xpoints()*lat.ypoints(); i++) {
    if (conc[i] != flag ) {
      ll.lat = lat[i]; ll.lon = lon[i];
      loc = rganaly.locate(ll);
//      printf("%f %f  %f %f  %f\n",lon[i], lat[i], conc[i], temp[i], rganaly[loc]*100);
      accum[loc] += conc[i];
      count[loc] += 1;
    }
  }



  for (i = 0; i < count.xpoints()*count.ypoints(); i++) {
    if (count[i] != 0) {
      accum[i] /= count[i];
    }
  }
  for (loc.j = 0; loc.j < count.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < count.xpoints(); loc.i++) {
    if (count[loc] != 0) {
      ll = count.locate(loc);
      printf("%f %f  %f %f\n",ll.lon, ll.lat, accum[loc], rganaly[loc]*100);
    }
  }
  }


  return 0;
}
