#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_12th<unsigned char> ages;
  global_12th<float> binary;
  mvector<double> histo(256);
  FILE *fin, *fout;
  ijpt loc, tloc;
 
  fin = fopen(argv[1],"r");
  ages.binin(fin);
  fclose(fin);

  conv(ages, binary);
  fout = fopen(argv[2],"w");
  binary.binout(fout);
  fclose(fout);

  for (loc.j = 0; loc.j < ages.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ages.xpoints(); loc.i++) {
    histo[ages[loc] ] += ages.cellarea(loc);
  }
  }
  for (int i = 0; i < 256; i++) {
    if (histo[i] > 0) {
      printf("%2d %e\n",i,histo[i]);
    }
  }
  
  return 0;
}
