#include <stdio.h>
#include <time.h>

#include "points.h"

#define GRIDI 0
#define GRIDJ 1

class grid {
  private:
    int nx, ny;
    float *gridi;
    float *gridj;
  public:
    grid();
    void augment();
    float& operator[](ijpt& );
    float& operator[](fijpt&);
};
grid::grid() {
  nx = NX;
  ny = NY;
  gridi = new float[nx*ny];
  gridj = new float[nx*ny];
}
float& grid::operator[](ijpt &x) {
  return gridi[x.i + x.j*nx];
}
float& grid::operator[](fijpt &x) {
  int i=x.i, j=x.j; 
  return gridj[j + i*ny];
}
void grid::augment() {
  ijpt loc;
  for (loc.j = 0; loc.j < ny; loc.j++) {
  for (loc.i = 0; loc.i < nx; loc.i++) {
    //printf("augment %d %d\n",loc.i, loc.j); fflush(stdout);
    gridj[loc.j + loc.i*ny] = gridi[loc.i + loc.j*nx];
  }
  }
}

int main(void) {
  grid x, y;
  ijpt loc, locip, locjp, locim, locjm;

  for (loc.j = 0; loc.j < NY; loc.j++) {
  for (loc.i = 0; loc.i < NX; loc.i++) {
    y[loc] = loc.i*loc.j;
    //printf("initialize %d %d\n",loc.i, loc.j,y[loc]); fflush(stdout);
  }
  }

  printf("%f\n",(float) clock()/ (float) CLOCKS_PER_SEC); fflush(stdout);
  y.augment();
  printf("%f\n",(float) clock()/ (float) CLOCKS_PER_SEC); fflush(stdout);

  // Now compute laplacean
  for (loc.j = 1; loc.j < NY -1 ; loc.j++) {
    locjp.j = loc.j + 1;
    locjm.j = loc.j - 1;
    locip.j = loc.j;
    locim.j = loc.j;
  for (loc.i = 1; loc.i < NX -1 ; loc.i++) {
    locjp.i = loc.i;
    locjm.i = loc.i;
    locip.i = loc.i + 1;
    locim.i = loc.i - 1;
    x[loc] = -4.*y[loc] + y[locip] + y[locjp] + y[locim] + y[locjm]; 
  }
  }
  printf("%f\n",(float) clock()/ (float) CLOCKS_PER_SEC); fflush(stdout);

//  printf("Split to i,j loops\n");
//  printf("%f\n",(float) clock()/ (float) CLOCKS_PER_SEC); fflush(stdout);
//  // im, ip loop
//  for (loc.j = 1; loc.j < NY -1 ; loc.j++) {
//    locip.j = loc.j;
//    locim.j = loc.j;
//  for (loc.i = 1; loc.i < NX -1 ; loc.i++) {
//    locip.i = loc.i + 1;
//    locim.i = loc.i - 1;
//    x[loc] = -4.*y[loc] + y[locip] + y[locim] ;
//  }
//  }
//  for (loc.j = 1; loc.j < NY -1 ; loc.j++) {
//    locjp.j = loc.j + 1;
//    locjm.j = loc.j - 1;
//  for (loc.i = 1; loc.i < NX -1 ; loc.i++) {
//    locjp.i = loc.i;
//    locjm.i = loc.i;
//    x[loc] +=  y[locjp] + y[locjm];
//  }
//  }
//  printf("%f\n",(float) clock()/ (float) CLOCKS_PER_SEC); fflush(stdout);

    
  

  return 0;
}
