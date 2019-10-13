#include "ncepgrids.h"

double rms(llgrid<float> &x, llgrid<unsigned char> &y) ;
double average(llgrid<float> &x, llgrid<unsigned char> &y) ;

int main(int argc, char *argv[]) {
  global_12th<unsigned char> land;
  global_12th<float> residue;
  FILE *fin;
  double x, y;

  fin = fopen(argv[1], "r");
  land.binin(fin);
  fclose(fin);
  fin = fopen(argv[2], "r");
  residue.binin(fin);
  fclose(fin);

  x = average(residue, land);
  y = rms(residue, land);
  printf("%6.3f %6.3f\n",x, y);


  return 0;
}
double average(llgrid<float> &x, llgrid<unsigned char> &y) {
  ijpt loc;
  double sum = 0.0, area = 0.0;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (y[loc] == 0) {
      area += x.cellarea(loc);
      sum += x[loc] * x.cellarea(loc);
    }
  }
  }
  return sum/area;
}

double rms(llgrid<float> &x, llgrid<unsigned char> &y) {
  ijpt loc;
  double sum = 0.0, area = 0.0;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (y[loc] == 0) {
      area += x.cellarea(loc);
      sum += x[loc]*x[loc] * x.cellarea(loc);
    }
  }
  }
  return sqrt(sum/area);
}

