#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  GRIDTYPE<float> bathy;
  GRIDTYPE<unsigned char> mask;
  unsigned char boundary = 1, land = 5, water = 17, ocean = 15, undef = 3;
  unsigned char coast = 0;
  ijpt loc;

  fin = fopen(argv[1],"r");
  bathy.binin(fin);
  mask.binin(fin);
  fclose(fin);

  fout = fopen(argv[2],"w");
  for (loc.j = 0; loc.j < mask.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < mask.xpoints(); loc.i++) {
    if (mask[loc] == boundary || mask[loc] == coast) {
      mask[loc] = COAST;
    }
    else if (mask[loc] == water || mask[loc] == ocean) {
      mask[loc] = OCEAN;
    }
    else if (mask[loc] == land) {
      mask[loc] = LAND;
    }
    else {
      printf("Error, illegal flag %d at %d %d\n",undef, loc.i, loc.j);
    }

  }
  }

  mask.binout(fout);

  return 0;
}
