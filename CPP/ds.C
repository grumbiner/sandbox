#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  grid2<float> grid_dx(384, 464), grid_dy(384, 464);
  FILE *fin;
  float x, y;

  fin = fopen(argv[1], "r");
//  return 0;

  for (int i = 1; i <= 32; i++) {
    fread(&x, sizeof(x), 1, fin); //printf("%d  %f\n",i, x); fflush(stdout);
    grid_dx.binin(fin); //printf("x %d\n",i); fflush(stdout);

    fread(&y, sizeof(y), 1, fin); //printf("%d  %f\n",i, y); fflush(stdout);
    grid_dy.binin(fin); //printf("y %d\n",i); fflush(stdout);

    printf("%2d  %8.3f %8.3f %7.3f %7.3f  %6.3f %6.3f %6.3f %6.3f\n",i, 
        grid_dx.gridmax(), grid_dx.gridmin(), grid_dx.average(), grid_dx.rms(),
        grid_dy.gridmax(), grid_dy.gridmin(), grid_dy.average(), grid_dy.rms() );
  }

  return 0;
}
