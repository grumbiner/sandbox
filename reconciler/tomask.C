#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_ice<unsigned char> land, icemask, flip;
  int i, j, k, line = 80;
  char cr;
  FILE *fin, *fice, *fout;
  ijpt loc, locflip;
  latpt ll;

  fin  = fopen(argv[1],"r");
  fice = fopen(argv[2],"r");
  icemask.binin(fice);
  fclose(fice);

  for (k = 0; k < land.xpoints()*land.ypoints() / line ; k++) {
    for (i = 0; i < line; i++) {
      fscanf(fin, "%1d",&j);
      if (j == 3) {
        land[i + k*line] = 157; 
      }
      else if (j == 0) {
        land[i + k*line] = 0;
      }
      else {
        printf("Impossible value %d at i,k %d %d\n",j, i,k);
      }

      //if (j != 3) {
      //  printf("%d at k= %d i = %d\n",j, k, i);
      //}
    }
  }

  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
    locflip.j = land.ypoints() - 1 - loc.j;
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
    locflip.i = loc.i;
    flip[loc] = land[locflip];
    if ( land[locflip] != icemask[loc]) {
      ll = land.locate(loc);
      printf("%4d %4d  %6.2f %6.2f  sst %3d  ice %3d\n",
        loc.i, loc.j, ll.lat, ll.lon, land[locflip],  icemask[loc]);
    }
  }
  }

  fout = fopen(argv[3],"w");
  flip.binout(fout);
  fclose(fout);

  return 0;
}
