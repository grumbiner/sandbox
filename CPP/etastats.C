#include <stdio.h>

#include "eta.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  eta32<unsigned char> uconc;
  eta32<float> conc, tmp;
  
  fin = fopen(argv[1], "r");
  uconc.binin(fin);
  fclose(fin);
  conv(uconc, conc);

  printf("field avg, max, min %f %f %f\n",conc.average(), conc.gridmax(), conc.gridmin() );
  conc.laplace(tmp);
  printf("lapl avg, max, min %f %f %f\n",tmp.average(), tmp.gridmax(), tmp.gridmin() );
  conc.gradsq(tmp);
  printf("gradsq avg, max, min %f %f %f\n",tmp.average(), tmp.gridmax(), tmp.gridmin() );

  return 0;
}
