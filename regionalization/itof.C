#include "grid_math.h"

int main(void) {
  FILE *fin;
  grid2<int> x(720,360);
  grid2<float> y(720,360);
  fin = fopen("belongs", "r");
  x.binin(fin);
  fclose(fin);
  printf("x max %d\n",x.gridmax());
  conv(x, y);
  fin = fopen("belongs.fl","w");
  y.binout(fin);
  fclose(fin);
  printf("y max %f\n",y.gridmax());

  return 0;
}
