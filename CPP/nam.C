#include "ncepgrids.h"

int main(void) {
  //lambert<float> mask(2345, 1597, 19.229, 233.723448, 265.0, 25.0, 2539.703000, 2539.703000, 1.0);
  lambert<float> mask(2345, 1597, 19.229, 233.723448-360., 265.0-360., 25.0, 2539.703000, 2539.703000, 1.0);
  FILE *fin;
  latpt ll;
  fijpt floc;
  ijpt loc;
  float latmin = 90.0, latmax = -90.0, lonmax = -180.0, lonmin = 900;

  fin = fopen("nam_mask", "r");
  mask.binin(fin);
  fclose(fin);

  for (loc.j = 0; loc.j < mask.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < mask.xpoints() ; loc.i++) {
    ll = mask.locate(loc);
    if (ll.lat > latmax) latmax = ll.lat;
    if (ll.lat < latmin) latmin = ll.lat;

    if (ll.lon > lonmax) lonmax = ll.lon;
    if (ll.lon < lonmin) lonmin = ll.lon;
  }
  }
  printf("ij lon min %f max %f  lat min %f max %f\n",lonmin, lonmax, latmin, latmax);


  loc.j = mask.ypoints() / 2;
  for (loc.i = 0; loc.i < mask.xpoints(); loc.i++) {
    ll = mask.locate(loc);
    if (ll.lat > latmax) latmax = ll.lat;
    if (ll.lon > lonmax) lonmax = ll.lon;
    if (ll.lat < lonmin) lonmin = ll.lon;
    printf("ij %f %f  %4d %4d  %f\n",ll.lat, ll.lon, loc.i, loc.j, mask[loc]);
  }
  fflush(stdout);

  ll.lat = 39.0;
  for (ll.lon = -180.0; ll.lon < -60.0; ll.lon += 0.01) {
    //printf("about to call locate\n"); fflush(stdout);
    floc = mask.locate(ll);
    printf("ll %f %f  %f %f  %f\n",ll.lat, ll.lon, floc.i, floc.j, mask[floc]);
    fflush(stdout);
  }

  return 0;
}

//1:0:vt=2017030200:surface:anl:LAND Land Cover (0=sea, 1=land) [Proportion]:
//    ndata=3744965:undef=0:mean=0.60188:min=0:max=1
//    grid_template=30:winds(N/S):
//	Lambert Conformal: (2345 x 1597) input WE:SN output WE:SN res 48
//	Lat1 19.229000 Lon1 233.723448 LoV 265.000000
//	LatD 25.000000 Latin1 25.000000 Latin2 25.000000
//	LatSP -90.000000 LonSP 0.000000
//	North Pole (2345 x 1597) Dx 2539.703000 m Dy 2539.703000 m mode 48

