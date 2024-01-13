#include "ncepgrids.h"

bool in(latpt &ll, latpt &l1, latpt &l2);
int main(void) {
  FILE *fin;
  global_12th<unsigned char> conc;
  ijpt loc;
  latpt ll;

  fin = fopen("seaice.t00z.fill5min","r");
  conc.binin(fin);
  fclose(fin);

  // lower left, upper right
  latpt l1, l2;

  for (loc.j = 0; loc.j < conc.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < conc.xpoints(); loc.i++) {
    ll = conc.locate(loc);

    l1.lon = 153.; l1.lat = 40.;
    l2.lon = 175.; l2.lat = 48.;
    if (in(ll, l1, l2)) {
      conc[loc] = 0;
    }

    l1.lon = 160.; l1.lat = 40.;
    l2.lon = 175.; l2.lat = 51.;
    if (in(ll, l1, l2)) {
      conc[loc] = 0;
    }

    l1.lon = 170.; l1.lat = 55.;
    l2.lon = 190.; l2.lat = 58.;
    if (in(ll, l1, l2)) {
      conc[loc] = 0;
    }

// W. NAtlantic
    l1.lon = 301.; l1.lat = 40.;
    l2.lon = 315.; l2.lat = 46.;
    if (in(ll, l1, l2)) {
      conc[loc] = 0;
    }
    l1.lon = 302.; l1.lat = 40.;
    l2.lon = 306.; l2.lat = 47.5;
    if (in(ll, l1, l2)) {
      conc[loc] = 0;
    }


    l1.lon = 309.; l1.lat = 40.;
    l2.lon = 315.; l2.lat = 47.;
    if (in(ll, l1, l2)) {
      conc[loc] = 0;
    }

// Antarctic:
    l1.lon = 0.; l1.lat = -60.;
    l2.lon = 360.; l2.lat = -30.;
    if (in(ll, l1, l2)) {
      conc[loc] = 0;
    }

    if (conc[loc] != 0) {
      printf("%4d %4d %f %f  %d\n",loc.i, loc.j, ll.lon, ll.lat, conc[loc]);
    }
  }
  }

  FILE *fout;
  fout = fopen("new", "w");
  conc.binout(fout);
  fclose(fout);

 

  return 0;
}
bool in(latpt &ll, latpt &l1, latpt &l2){
  return (ll.lat >= l1.lat && ll.lat <= l2.lat && ll.lon >= l1.lon && ll.lon <= l2.lon);
}
