#include "ncepgrids.h"

#define nharm 6

int main(void) {
  FILE *fout, *ftxt;
  global_quarter<float> slope, intercept;
  global_quarter<float> ampl[nharm], phase[nharm];
  int j;

  fout = fopen("reference_fields","r");
  ftxt = fopen("reference.txt","w");

  intercept.binin(fout);
  slope.binin(fout);
  intercept.printer(ftxt);
  slope.printer(ftxt);

  for (j = 0; j < nharm; j++) {
    ampl[j].binin(fout);
    phase[j].binin(fout);
    ampl[j].printer(ftxt);
    phase[j].printer(ftxt);
  }
  fclose(fout);

  return 0;
}
