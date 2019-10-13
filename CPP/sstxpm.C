#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_12th<float> sst;
  global_12th<unsigned char> land, out;
  palette<unsigned char> g(40);
  FILE *fin;
  int i, rate = 256/g.ncol;
  ijpt loc;

  fin = fopen(argv[1],"r");
  sst.binin(fin);
  fclose(fin);
  if (sst.gridmax() > 50) sst -= 273.15;
 
  fin = fopen(argv[2], "r");
  land.binin(fin);
  fclose(fin);

  for (i = 0; i < g.ncol; i++) {
    g.set_color(i, rate*i, rate*i, rate*i);
  }

  for (loc.j = 0; loc.j < sst.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < sst.xpoints(); loc.i++) {
    if (land[loc] > 99) {
      out[loc] = 0;
    }
    else {
      out[loc] = 1 + (int) (0.5 + ((g.ncol-1)/42.0)*(sst[loc] + 2.0));
    }
  }
  }

  out.xpm("out.xpm",1,g);

  return 0;
}  
