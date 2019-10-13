#include "ncepgrids.h"
//1 Mar 2011
//Robert Grumbine

int main(int argc, char *argv[]) {
  global_12th<float> bathy;
  FILE *fin, *fout;
  ijpt loc, tloc;
  float depth;
  int i, j;
 
  fin = fopen(argv[1],"r");
  for (tloc.j = 0; tloc.j < bathy.ypoints(); tloc.j++) {
  for (tloc.i = 0; tloc.i < bathy.xpoints(); tloc.i++) {
    fscanf(fin, "%d %d %f\n",&i, &j, &depth);
    loc.i = i; loc.j = j;
    bathy[loc] = depth;
  }
  }
  fclose(fin);

  fout = fopen(argv[2],"w");
  bathy.binout(fout);
  fclose(fout);

  return 0;
}
