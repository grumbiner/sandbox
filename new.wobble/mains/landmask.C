#include "ncepgrids.h"

// ensure the land mask is applied to the ice concentrations:

int main(int argc, char *argv[]) {
  global_ice<unsigned char> land, ice;
  FILE *fice, *fland, *fout;
  int i;

  fice = fopen(argv[1], "r");
  fland = fopen(argv[2], "r");
  fout  = fopen(argv[3], "w");
  ice.binin(fice);
  land.binin(fland);
  for (i = 0; i < land.xpoints() * land.ypoints(); i++) {
    if (land[i] == 157) ice[i] = land[i];
  }
  ice.binout(fout);

  return 0;
} 
