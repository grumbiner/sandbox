#include "ncepgrids.h"

int main(void) {
  FILE *fin;
  global_12th<float> climo, obs, old, newanom ;

  fin = fopen("all.bin","r");
  climo.binin(fin);
  obs.binin(fin);
  old.binin(fin);
  fclose(fin);

  if (climo.gridmax() > 200) climo -= 273.15;
  if (obs.gridmax() > 200)   obs   -= 273.15;
  newanom = obs; 
  newanom -= climo;

  fin = fopen("scale.out","w");
  climo.binout(fin);
  obs.binout(fin);
  old.binout(fin);
  newanom.binout(fin);
  fclose(fin);

  return 0;
}
