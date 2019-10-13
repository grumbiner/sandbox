#include <stdio.h>

#include "points.h"
#include "mvector.h"
//Notes toward building 2d grids out of mvectors

class grid2 {
  private:
    int nx, ny, npts;
    mvector<float> *grid;
  public:
    grid2(int = 70, int = 70);
    float & operator[](const ijpt &x) {return grid->operator[](x.i+x.j*nx); }
};
grid2::grid2(int n1, int n2) {
  nx = n1;
  ny = n2;
  npts = n1*n2;
  grid = new mvector<float> (npts);
}
//float & grid2::operator[](const ijpt &x) {
//    int index = x.i + x.j*nx;
////  return (*grid->::operator[](x.i+x.j*nx) );
////  return ( *grid[x.i+x.j*nx] );
//// no compile    return (grid[x.i+x.j*nx]);
//// ''    return *grid.vec[index];
//// no, private member    return grid->vec[index];
//    return grid->operator[](index);
//}

int main(void) {
  grid2 x;
  ijpt y;

  y.i = 23; y.j = 24;
  
  printf("%f\n",x[y]);
  for (y.j = 0; y.j < 70; y.j++) {
  for (y.i = 0; y.i < 70; y.i++) {
    x[y] = y.i*y.j;
  }
  }
  for (y.j = 0; y.j < 70; y.j++) {
  for (y.i = 0; y.i < 70; y.i++) {
    printf("%f\n",x[y]);
  }
  }

  return 0;
}
