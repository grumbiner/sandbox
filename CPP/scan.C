#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_ice<float> x[7];
  FILE* fin;

  fin = fopen(argv[1], "r");
  for (int i = 0; i < 7; i++) {
    x[i].binin(fin);
    printf("%d  %f %f %f %f\n",i,x[i].gridmax(), x[i].gridmin(), x[i].average(), x[i].rms() );
  }

  return 0;
}
