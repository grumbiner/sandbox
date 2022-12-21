#include "gaussian.h"
#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  gaussian<float> x, mask;
  stlawrence<float> y;
  latpt ll;
  ijpt loc;
  float nonval = -999.0, flagval = -999.0;
  int pad = y.xpoints()*y.ypoints();
  FILE *fin, *fout;

  mask.set((float) 0.0);
  fin = fopen(argv[1],"r");
  fout = fopen(argv[2],"w");

//  fwrite(&pad, sizeof(float), 1, fout);
  int i = 1;
  while (!feof(fin) ) {
    x.ftnin(fin);
    y.fromall(x, mask, flagval, nonval);
    y.ftnout(fout);
//    if ( i % 14 == 0 ) fwrite(&pad, sizeof(int), 1, fout); 
    i += 1;
  }
//  fwrite(&pad, sizeof(int), 1, fout);

  fclose(fout);

  return 0;
}
