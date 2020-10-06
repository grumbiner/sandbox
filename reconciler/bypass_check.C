#include "ncepgrids.h"

// Compare land mask file with the 'bypass' type file -- see
//   whether any points marked 'bypass' are considered water
//   by the land masking, and if so, where
// Robert Grumbine 23 July 2007

int main(int argc, char *argv[]) {
  GRIDTYPE<unsigned char> mask, bypass;
  FILE *fin1, *fin2;
  ijpt loc;
  latpt ll;

  fin1 = fopen(argv[1], "r");
  fin2 = fopen(argv[2], "r");
  if (fin1 == (FILE*) NULL) {
    printf("failed to open land mask file %s\n", argv[1]);
    return 1;
  }
  if (fin2 == (FILE*) NULL) {
    printf("failed to open bypass lake file %s\n", argv[2]);
    return 2;
  }

  mask.binin(fin1);
  bypass.binin(fin2);
  fclose(fin1);
  fclose(fin2);

  for (loc.j = 0; loc.j < mask.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < mask.xpoints(); loc.i++) {
    if (bypass[loc] == 194 && mask[loc] != 5) {
      ll = mask.locate(loc);
      printf("%d %d lat %f %f bypass but mask is %d\n",loc.i, loc.j, 
                 ll.lat, ll.lon, mask[loc]);
    }
  }
  }

  return 0;
}
