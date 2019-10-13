#include <stdio.h>

#include "ncepgrids.h"
#include "gaussian.h"

int main(void) {
  FILE *fin, *fout1, *fout2;
  gaussian<float> in(254), inmask(254);
  northgrid<float> out1;
  southgrid<float> out2;
  float nonval = 99990002605540088217.0, maskval = 99990002605540088217.0;
  int i;

  fout1 = fopen("metout.north", "w");
  fout2 = fopen("metout.south", "w");
  fin = fopen("metin","r");

  inmask.set((float) 0.0);

  for (i = 0; i < 53; i++) {
    in.binin(fin);
    if (in.gridmax() > 1.e8) {
      nonval = in.gridmax();
      maskval = in.gridmax();
    }
    //printf("i %2d %f %f\n",i, in.average(nonval), in.gridmax(nonval));
    out1.fromall(in, inmask, maskval, nonval);
    out1.binout(fout1);
    //printf("n %2d %f %f\n",i, out1.average(nonval), out1.gridmax(nonval));
    
    out2.fromall(in, inmask, maskval, nonval);
    out2.binout(fout2);
    //printf("s %2d %f %f\n",i, out2.average(nonval), out2.gridmax(nonval));
    //printf("\n");
  }
  fclose(fin);
  fclose(fout1);
  fclose(fout2);

  return 0;
}
