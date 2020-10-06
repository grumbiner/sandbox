//  ./sstinterp qdoi.$daym qdoi.$dayp qdoi.$day

#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *finm, *finp, *fout;
  // -9.99 is reynolds flag value for no info
  float flagval = -5.0;  // Reynolds gridmin changes, not always -9.99 apparently
  float flag2   = 40.0;  //  Guard against absurd high values
  float offset  = 273.15; // offset, to manage K vs. C.  Use 0 for C
  ijpt loc, tloc, oloc;

  int x[4]; // extra is for the ftn unformatted header
  global_quarter<short int> daym, dayp, day;

  finm  = fopen(argv[1], "r");
  finp  = fopen(argv[2], "r");
  fout  = fopen(argv[3], "w");

  fread(&x, sizeof(int), 4, finm);
  daym.binin(finm);
  fclose(finm);

  fread(&x, sizeof(int), 4, finp);
  dayp.binin(finp);
  fclose(finp);

  day  = daym;
  day += dayp;
  day /= 2;
  for (loc.j = 0; loc.j < day.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < day.xpoints(); loc.i++) {
    if (day[loc] < -500) day[loc] = -999; 
    // Alternate roundings so that they aren't all down
    if (day[loc] % 4 > 1) day[loc] += 1;
  }
  }

  fwrite(&x, sizeof(int), 4, fout);
  day.binout(fout);
  fclose(fout);

  printf("day max, min %d %d\n",day.gridmax(), day.gridmin() );
  printf("daym max, min %d %d\n",daym.gridmax(), daym.gridmin() );
  printf("dayp max, min %d %d\n",dayp.gridmax(), dayp.gridmin() );

  return 0;
}

