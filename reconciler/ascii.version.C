#include "ncepgrids.h"

int main(void) {
  grid2<float> orig(5400, 2700);
  FILE *fin, *fout;

  fin = fopen("outfin","r");
  orig.binin(fin);
  fclose(fin);

  fout = fopen("bathy.ascii","w");
  orig.printer(fout);
  fclose(fout);
    
  return 0;
}
