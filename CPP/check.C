#include "ncepgrids.h"

int main(void) {
  FILE *fin;
  northgrid<float> x;
  grid2<float> dx((x.xpoints()-1), (x.ypoints()-1));
  grid2<float> dy((x.xpoints()-1), (x.ypoints()-1));
  //mvector<float> dx((x.xpoints()-1) * (x.ypoints()-1));
  //mvector<float> dy((x.xpoints()-1) * (x.ypoints()-1));

  printf("nx ny = %d %d\n",x.xpoints(), x.ypoints() );
  printf("dx nx ny = %d %d\n",dx.xpoints(), dx.ypoints() );
  fflush(stdout);

  fin = fopen("grid_ds.gep01","r");
  for (int day = 0; day < 17 && !feof(fin); day++) {
    printf("day = %d ",day);
    dx.ftnin(fin); printf("dx max = %f ",dx.gridmax() );
    dy.ftnin(fin); printf("dy max = %f\n",dy.gridmax() );
    //dx.binin(fin);
    //dy.binin(fin);
  }
  fclose(fin);

  return 0;
}
