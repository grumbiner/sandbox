#include "ncepgrids.h"

//Translate from grids which are flagged in RTOFS sense to 
//  sea ice concentration analysis flags

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  GRIDTYPE<unsigned char> rtofs, ssmi;
  int i;

//RTOFS flags
  unsigned char boundary = 1, land = 5, water = 17, ocean = 15, undef = 3;
// SSMI flags
  unsigned char iceland = 157, icewater = 0, coast = 195, no_data = 224;

  fin = fopen(argv[1], "r");
  rtofs.binin(fin);
  fclose(fin);

  for (i = 0; i < ssmi.xpoints() * ssmi.ypoints(); i++) {
    if (rtofs[i] == water || rtofs[i] == ocean ) {
      ssmi[i] = icewater;
    }
    else if (rtofs[i] == boundary) {
      ssmi[i] = coast;
    }
    else if (rtofs[i] == land) {
      ssmi[i] = iceland;
    }
    else if (rtofs[i] == undef) {
      ssmi[i] = no_data;
      printf("index point %d has no data\n",i);
    }
    else {
      printf("index point %d has unkown value %d\n",i, rtofs[i]);
    }
    
  }

  fout = fopen(argv[2],"w");
  ssmi.binout(fout);
  fclose(fout);

  return 0;
}
