#include "ncepgrids.h"

void reflag(float &flag, metricgrid<float> &grid);

int main(int argc, char *argv[]) {
  gaussian<float> tmp;
  FILE *fin, *fout;
  int n, i;
  float fn, flag;

  fin = fopen(argv[1],"r");
  fout = fopen(argv[2],"w");

  fread(&n,sizeof(int),1, fin);
  printf("n = %d\n",n);
  fn = (float) n;

  i = 1;
  while (!feof(fin) ) {
    tmp.set((float)0.0);
    tmp.binin(fin);
    tmp /= fn;
    tmp.binout(fout);
    flag = tmp.gridmax();
    if (flag > 1e8) {
      reflag(flag, tmp);
      printf("i = %d, flag = %f grid average = %f\n",i, flag, tmp.average(flag) );
    }
    else {
      printf("i = %d, unflag   grid average = %f\n",i, tmp.average() );
    }
    i += 1;
  }

  return 0;
}
void reflag(float &flag, metricgrid<float> &grid) {
  float tflag;
  int i;

  if (flag > 1e10) {
    tflag = 1.e10;
    for (i = 0; i < grid.xpoints()*grid.ypoints(); i++) {
      if (grid[i] > tflag) grid[i] = tflag;
    }
    flag = tflag;
  }
  return;
}
