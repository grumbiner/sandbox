
#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  northgrid<float> icec;
  global_sst<float> sst;
  FILE *fin1, *fin2, *fout;
  ijpt loc, tloc;
  fijpt floc;
  latpt ll;

  fin1 = fopen(argv[1],"r");
  if (fin1 == (FILE *) NULL) {
    printf("Failed to open ice concentration file\n");
    return 1;
  }
  fin2 = fopen(argv[2],"r");
  if (fin2 == (FILE *) NULL) {
    printf("Failed to open sst file\n");
    return 2;
  }
  fout = fopen(argv[3],"w");
  if (fout == (FILE *) NULL) {
    printf("Failed to open output file\n");
    return 3;
  }

  icec.binin(fin1);
  sst.binin(fin2);
  fclose(fin1);
  fclose(fin2);

  for (loc.j = 0; loc.j < icec.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < icec.xpoints(); loc.i++) {
    ll = icec.locate(loc);
    tloc = sst.locate(ll);
    if (sst[tloc] > 275.15) {
      printf("resetting %f %f  %f  %f\n",ll.lat, ll.lon, icec[loc], sst[tloc]);
      icec[loc] = 0;
    }
  }
  }

  icec.binout(fout);

  return 0;
}
