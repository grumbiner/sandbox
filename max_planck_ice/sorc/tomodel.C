#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin1, *fin2, *fout;
  GRIDTYPE<float> in, out;
  GRIDTYPE<unsigned char> land;
  ijpt loc;
  float maskval = 2.24;
  float minconc = 0.15;

  fin1 = fopen(argv[1],"r");
  in.binin(fin1);
  fclose(fin1);

  fin2 = fopen(argv[2],"r");
  land.binin(fin2);
  fclose(fin2);

  fout = fopen(argv[3],"w");

  for (loc.j = 0; loc.j < in.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < in.xpoints(); loc.i++) {
    if (land[loc] == 157 || in[loc] > 1.28 ) {
      out[loc] = maskval;
    }
    else if (in[loc] > 1.0 && in[loc] < 1.28) {
      out[loc] = 1.0;
    }
    else if (in[loc] > minconc) {
      out[loc] = in[loc];
    }
    else {
      out[loc] = 0.0;
    }
  }
  }

  out.binout(fout);
  fclose(fout);

  return 0;
}
