#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  HIGH<unsigned char> mask;
  LOW<unsigned char> lowmask;
  LOW<float> pct, count;
  ijpt loc, lowloc;
  latpt ll;

  fin = fopen(argv[1],"r");
  mask.binin(fin);
  fclose(fin);

  pct.set( (float) 0.);
  count.set( (float) 0.);

  for (loc.j = 0; loc.j < mask.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < mask.xpoints(); loc.i++) {
    if (mask[loc] == 224) mask[loc] = 151;
    if (mask[loc] == 100) mask[loc] = 150;

    ll = mask.locate(loc);
    lowloc = pct.locate(ll);
    if (!pct.in(lowloc)) {
      printf("mapped off grid, %d %d  %f %f  %d %d\n",loc.i, loc.j,
         ll.lat, ll.lon, lowloc.i, lowloc.j);
      fflush(stdout);
    }
    else {
      count[lowloc] += 1;
      if (mask[loc] == 0 || mask[loc] == 150) {
        pct[lowloc] += 1;
      }
    }

  }
    printf("%d %d\n",loc.i, loc.j); fflush(stdout);
  }
  printf("average high %f\n",(float) mask.average());
  printf("gridmax %f\n",count.gridmax() );
  fflush(stdout);

  lowmask.set(0);
  for (loc.j = 0; loc.j < lowmask.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < lowmask.xpoints(); loc.i++) {
    if (count[loc] != 0) {
      pct[loc] /= count[loc];
    }
    else {
      printf("undefined point at %d %d\n",loc.i, loc.j);
    }

    if (pct[loc] != 0.) {
	lowmask[loc] = 0;
    }
    else {
	lowmask[loc] = 157;
    }
  }
  }
  printf("average low %f\n",(float) lowmask.average());

  fout = fopen("percent","w");
  pct.binout(fout);
  fclose(fout);
  fout = fopen("lowmask","w");
  lowmask.binout(fout);
  fclose(fout);

  return 0;
}
