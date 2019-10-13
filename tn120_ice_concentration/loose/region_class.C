#include "ncepgrids.h"
#include <stdlib.h>

class region {
  public:
    latpt ll, ur;
    region();
    region(latpt &, latpt &);
    bool in(latpt &);
};
region::region() {
  ll.lat = 0.;
  ll.lon = 0.;
  ur.lat = 0.;
  ur.lon = 0.;
}
region::region(latpt &x, latpt &y) {
  ll.lat = x.lat;
  ll.lon = x.lon;
  ur.lat = y.lat;
  ur.lon = y.lon;
  // Note that we're not verifying the propriety of the arguments
}
bool region::in(latpt &x) {
  return (x.lat >= ll.lat && x.lat <= ur.lat &&
          x.lon >= ll.lon && x.lon <= ur.lon ) ;
}

#define LAND 157
#define WEATHER 177
#define NO_DATA 224
#define MIN_CONC 15

int main(int argc, char *argv[]) {
  region hudson;
  northgrid<unsigned char> conc, land;
  global_ice<unsigned char> gland, gconc;
  global_sst<float> sst, count;
  ijpt ij, glob;
  fijpt fglob;
  latpt npt;
  FILE *fin;
  int lim;

  lim = atoi(argv[1]);
  hudson.ll.lat = 50.0;
  hudson.ll.lon = 0.;
  hudson.ur.lat = 70.0;
  hudson.ur.lon = 360.;
  fin = fopen("nland.map", "r");
  land.binin(fin);
  fclose(fin);
  fin = fopen("umasknorth.990913", "r");
  conc.binin(fin);
  fclose(fin);
  fin = fopen("halfdeg.map", "r");
  gland.binin(fin);
  fclose(fin);
  fin = fopen("sstout", "r");
  sst.binin(fin);
  fclose(fin);
  fin = fopen("globout", "r");
  gconc.binin(fin);
  fclose(fin);
  
  for (ij.j = 0; ij.j < land.ypoints() ; ij.j++) {
  for (ij.i = 0; ij.i < land.xpoints() ; ij.i++) {
    npt = conc.locate(ij);
    fglob = gland.locate(npt);
    glob.i = (int) (fglob.i + 0.5);
    glob.j = (int) (fglob.j + 0.5);
    if (npt.lon < 0.) npt.lon += 360.;
    //if (hudson.in(npt) && land.anyof(LAND, lim, ij) == 0 && 
    if (hudson.in(npt) && 
        conc[ij] != WEATHER && conc[ij] != NO_DATA && conc[ij] >= MIN_CONC &&
        sst[glob] < 293.15) {
      printf("%3d %3d  %5.2f %6.2f  %3d %3d %3.1f %3d  %3d %6.2f  %d %d\n",
         ij.i, ij.j, npt.lat, npt.lon, conc[ij], land[ij], count[glob], gconc[glob], gland[glob], sst[glob], glob.i, glob.j) ;
    }
  }
  }

  return 0;
} 
  
