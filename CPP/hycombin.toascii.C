#include "grid_math.h"

#define nx 1200
#define ny 1684

int main(void) {
  grid2<float> x(1200,1684);
  FILE *fout, *fout2, *fin;

  fin = fopen("ssh","r");
  x.binin(fin);
  x.printer(stdout);

  return 0;
}
 
