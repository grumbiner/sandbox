#include "ncepgrids.h"

// Translate new (4 step 0 1 2 3) land mask flags to old (0 157 195 224) convention:
int main(int argc, char *argv[]) {
  global_12th<unsigned char> in, out;
  int i, j, k, line = 80;
  char cr;
  FILE *fin, *fice, *fout;
  ijpt loc, locflip;
  latpt ll;

  fin  = fopen(argv[1],"r");
  fout = fopen(argv[2],"w");
  in.binin(fin);
  fclose(fin);

  for (loc.j = 0; loc.j < out.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < out.xpoints(); loc.i++) {
    if (in[loc] == 0 ) {
      out[loc] = 0;
    }
    else if (in[loc] == 1 ) {
      out[loc] = 0;
    }
    else if (in[loc] == 2 ) {
      out[loc] = 157;
    }
    else if (in[loc] == 3 ) {
      out[loc] = 195;
    }
    else {
      printf("Impossible value %d at %d %d\n",in[loc], loc.i, loc.j);
    }
  }
  }

  out.binout(fout);
  fclose(fout);

  return 0;
}
