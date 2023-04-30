#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_quarter<float> sstin;
  global_quarter<float> sstout;
  FILE *fin;
  float flagval = -5.0;  // Reynolds gridmin changes, not always -9.99 apparently
  float flag2   = 40.0;  //  Guard against absurd high values
  float offset  = 273.15; // offset, to manage K vs. C.  Use 0 for C
  ijpt loc, tloc, oloc;
  latpt ll;
  int x[4]; // extra is for the ftn unformatted header
  global_quarter<short int> sstread;

  fin  = fopen(argv[1], "r");
  // -9.99 is reynolds flag value for no info

  fread(&x, sizeof(int), 4, fin);
  sstread.binin(fin);
  conv(sstread, sstin);
  sstin /= 100.;
  //sstin.ftnin(fin);
  fclose(fin);

  printf("sst max, min %f %f\n",sstin.gridmax(), sstin.gridmin() );
  return 0;
}
