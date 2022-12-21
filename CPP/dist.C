#include "ncepgrids.h"

int main(void) {
  global_12th<float> dist;
  FILE *fin;
  mvector<float> histogram(5000);
  ijpt loc;

  fin = fopen("seaice_alldist.bin","r");
  dist.binin(fin);
  fclose(fin);

  printf("gridmax = %f\n",dist.gridmax() );
  if (dist.gridmax() > 5000) dist /= 1000.;

  for (loc.j = 0; loc.j < dist.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < dist.xpoints(); loc.i++) {
    histogram[ (int)(0.5 + dist[loc]) ] += dist.cellarea(loc);
  }
  }
  histogram /= 1e6;
  double sum = 0.0;
  for (int i = 0; i < histogram.xpoints(); i++) {
    sum += histogram[i];
    printf("%4d %f %f\n",i,histogram[i], sum);
  }

  return 0;
}
