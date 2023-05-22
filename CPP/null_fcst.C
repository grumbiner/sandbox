#include "ncepgrids.h"

// Program to construct simple statistical predictions for sea ice concentration
// Robert Grumbine

// Args: skipfile concentration_observed outfile

float mrf(float &x);

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  GRIDTYPE<float> concin;
  GRIDTYPE<unsigned char> skip;
  GRIDTYPE<float> snowball, tropica;
  GRIDTYPE<float> persist, mrflike;
  ijpt loc;
  float tmp;

  fin = fopen(argv[1], "r");
  concin.binin(fin);
  fclose (fin);
  if (concin.gridmax() > 3.0) concin /= 100;

  fin = fopen(argv[2], "r");
  skip.binin(fin);
  fclose(fin);

  fout = fopen(argv[3], "w");

// Construct the zero information forecasters, snowball, tropica:
//   and the first level information (today's ob) forecasters persist and mrflike
  tropica.set((float) 0);
  snowball.set((float) 0);
  persist.set((float) 0);
  mrflike.set((float) 0);
  for (loc.j = 0; loc.j < skip.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < skip.xpoints(); loc.i++) {
    if (skip[loc] == 0) {
      tropica[loc]  = 0.0;
      snowball[loc] = 1.0;
      tmp = (float) concin[loc];
      persist[loc]  = tmp;
      mrflike[loc]  = mrf(tmp);
    }
  }
  }
  tropica.binout(fout);
  snowball.binout(fout);
  persist.binout(fout);
  mrflike.binout(fout);
  fclose(fout);

  return 0;
}
float mrf(float &x) {
  float y = 0.0;

  if (x > 0.55) {
    y = 1.00;
  }

  return y;
}
