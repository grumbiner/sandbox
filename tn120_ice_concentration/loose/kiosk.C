#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_12th<float> original;
  llgrid<float> kiosk(original.xpoints(), original.ypoints(), 
                      original.dlat, original.dlon, 
                      original.firstlat, -180. + original.dlon/2.);
  global_12th<unsigned char> oland;
  llgrid<float> kiosk_land(original.xpoints(), original.ypoints(),
                    original.dlat, original.dlon,
                    original.firstlat, -180. + original.dlon/2.);
  FILE *fin;
  palette<unsigned char> gg(19,65);

  ijpt loc, oloc;
  latpt ll;

  fin = fopen(argv[1],"r");
  original.binin(fin);
  fclose(fin);
  original *= 100;
  fin = fopen(argv[2],"r");
  oland.binin(fin);
  fclose(fin);

  for (loc.j = 0; loc.j < original.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < original.xpoints(); loc.i++) {
    ll = kiosk.locate(loc);
    oloc = original.locate(ll);
    kiosk[loc] = original[oloc];
    kiosk_land[loc] = (float) oland[oloc];
  }
  }

  kiosk.colorproc(kiosk_land, 7, 65, std_ice_coloring);
  kiosk.xpm(argv[3],1,gg);
  
  return 0;
}
