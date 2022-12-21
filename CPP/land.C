#include "ncepgrids.h"


int main(int argc, char *argv[]) {
  global_12th<unsigned char> ice;
  stlawrence<float> stl;
  okhotsk<float> ok;
  ijpt loc, tloc;
  latpt ll;
  FILE *fin, *fout;
  palette<unsigned char> gg(19, 65);

  fin = fopen("seaice_gland5min","r");
  ice.binin(fin);
  fclose(fin);

  for (loc.j = 0; loc.j < ok.ypoints(); loc.j++ ) {
  for (loc.i = 0; loc.i < ok.xpoints(); loc.i++ ) {
    ll = ok.locate(loc);
    tloc = ice.locate(ll);
    ok[loc] = ice[tloc]; 
  }
  }
  for (loc.j = 0; loc.j < stl.ypoints(); loc.j++ ) {
  for (loc.i = 0; loc.i < stl.xpoints(); loc.i++ ) {
    ll = stl.locate(loc);
    tloc = ice.locate(ll);
    stl[loc] = ice[tloc]; 
  }
  }

//  fout = fopen("land","w");
//  stl.ftnout(fout);
//  fclose(fout);

  stl.xpm("stlland.xpm",9,gg);
  ok.xpm("okland.xpm",9,gg);

  return 0;
}
