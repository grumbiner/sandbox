#include <stdio.h>

#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  GRIDTYPE<float> out;
  GRIDTYPE<unsigned char> in;
  ijpt loc;

  fin = fopen(argv[1],"r");
  fout = fopen(argv[2], "w");
  if (fin == (FILE *) NULL || fout == (FILE *) NULL) {
    printf("failed to open required file\n");
    return 1;
  }
  in.binin(fin);

  for ( loc.j = 0; loc.j < in.ypoints() ; loc.j++) {
  for ( loc.i = 0; loc.i < in.xpoints() ; loc.i++) {
     out[loc] = (float)  in[loc] ;
  }
  }

  out.binout(fout);

  return 0;
}
