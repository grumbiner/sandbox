#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  GRIDTYPE<unsigned char> mask;
  GRIDTYPE<int> count;
  int i, days ;
  palette<unsigned char> gg(19,65);
  ijpt loc;
  latpt ll;

  fin = fopen(argv[1],"r");
  if (fin == (FILE*) NULL) {
    printf("failed to open %s\n",argv[1]);
    return 1;
  }
  mask.binin(fin);
  fclose(fin);

  fin = fopen(argv[2],"r");
  if (fin == (FILE*) NULL) {
    printf("failed to open %s\n",argv[2]);
    return 1;
  }
  count.binin(fin);
  fclose(fin);

  days = count.gridmax();
//  Loop over all points and reconcile:
  for (loc.j = 0; loc.j < mask.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < mask.xpoints(); loc.i++) {
    ll = mask.locate(loc);
    if (mask[loc] != 157) {
      printf("%3d %3d  %6.2f %7.2f  %3d %4d %5.3f\n",loc.i, loc.j, 
          ll.lat, ll.lon, mask[loc], count[loc],
          (float)count[loc] / (float)days );
    }
  }
  } 

  return 0;
}
