#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  //float minlat = 44, maxlat = 52, minlon = -70+360, maxlon = -52+360;
  global_12th<unsigned char> ice, mask;
  stlawrence<unsigned char> stl;
  ijpt loc, tloc;
  latpt ll;
  FILE *fin, *fout;
  palette<unsigned char> gg(19, 65);

  fin = fopen(argv[1],"r");
  ice.binin(fin);
  fclose(fin);
  fin = fopen("seaice_gland5min","r");
  mask.binin(fin);
  fclose(fin);

  for (loc.j = 0; loc.j < stl.ypoints(); loc.j++ ) {
  for (loc.i = 0; loc.i < stl.xpoints(); loc.i++ ) {
    ll = stl.locate(loc);
    tloc = ice.locate(ll);
    stl[loc] = ice[tloc]; 
    if (mask[tloc] > 100) stl[loc] = mask[tloc];
  }
  }

  fout = fopen(argv[2],"w");
  stl.binout(fout);
  fclose(fout);

  stl.xpm(argv[3],9,gg);

  return 0;
}
