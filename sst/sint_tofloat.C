#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  global_quarter<short int> ssti;
  global_quarter<float> fl;
  
  fin = fopen(argv[1],"r");
  ssti.binin(fin);
  fclose(fin);

  conv(ssti, fl);

  fin = fopen(argv[2],"w");
  fl.binout(fin);
  fclose(fin);

  return 0;
}
