#include "ncepgrids.h"

int main(void) {
  FILE *fin;
  global_12th<float> bathy;
  
  fin = fopen("bathyout","r");
  bathy.binin(fin);
  fclose(fin);

  bathy.printer(stdout);
  return 0;
}
