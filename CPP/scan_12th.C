#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_12th<float> x;

  FILE *fin;

  fin = fopen(argv[1],"r");
  while(!feof(fin)) {
    x.binin(fin);
    printf("%f %f %f %f\n",x.gridmax(), x.gridmin(), x.average(), x.rms() );
  }

  return 0;
}
