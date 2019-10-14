#include "grid_math.h"

#define nx 1200
#define ny 1684

int main(void) {
  grid2<float> x(1200,1684);
  FILE *fout, *fout2, *fin;
  ijpt loc;
  int i, j, val;

  fin = fopen("ascii","r");
  for (loc.j = 0; loc.j < ny; loc.j++) {
  for (loc.i = 0; loc.i < nx; loc.i++) {
    fscanf(fin, "%d %d %f\n",&i,&j,&val);
    x[loc] = val;
  }
  }
  fclose(fin);

  fout = fopen("ssh","w");
  x.ftnout(fout);
  fclose(fout);

  return 0;
}
