#include <math.h>
#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  ijpt loc;
  global_ice<unsigned char> orig, newer, land;
  global_ice<float> fland;
  global_sst<float> sst, sland;
  palette<unsigned char> gg(19,65);
  
  fin = fopen(argv[1], "r");
  orig.binin(fin);
  fclose(fin);
  fin = fopen(argv[2], "r");
  newer.binin(fin);
  fclose(fin);
  fin = fopen(argv[3], "r");
  land.binin(fin);
  fclose(fin);
  fin = fopen("sstout", "r");
  sst.binin(fin);
  fclose(fin);

  for (loc.j = 0; loc.j < fland.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < fland.xpoints(); loc.i++) {
     fland[loc] = (float) land[loc];
  }
  }
  newer.xpm("new.xpm",7,gg);
  sland.fromall(fland, fland, 255, 255);
  sst -= 271.0;
  for (loc.j = 0; loc.j < sland.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < sland.xpoints(); loc.i++) {
     if (sland[loc] > 50.) {
        sst[loc] = 2.*18.; 
     }
  } 
  }
  sst.xpm("sst.xpm", 2, gg);

  for (loc.j = 0; loc.j < orig.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < orig.xpoints(); loc.i++) {
    if (fabs(orig[loc] - newer[loc]) > 1 ) {
      printf("%3d %3d  %3d %3d  %3d\n",loc.i, loc.j, orig[loc], newer[loc], land[loc]);
    }
    if (fabs(orig[loc] - newer[loc]) > 10 ) {
      printf("big %3d %3d  %3d %3d  %3d\n",loc.i, loc.j, orig[loc], newer[loc], land[loc]);
    }
  }
  }
  return 0;
}
