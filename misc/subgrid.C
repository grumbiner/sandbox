#include <stdio.h>
//#include <malloc.h>

#include "ncepgrids.h"

class x : public northgrid<float> {
  public:
    x(void);
    x(x &);
    void subgrid(latpt &, latpt &, x* );
};
x::x(void) {
  nx = 385;
  ny = 465;
  dx = 25.4e3;
  dy = 25.4e3;
  xorig = (-38.*5* dx );
  yorig = (-46.*5* dy );
  sgn = 1.0;
  slat = 60.0;
  slon = (-10.0);

  grid = (float *) malloc(nx * ny * sizeof(float) );
  if (grid == (float *) NULL) { printf("Failed to mallocate in northgrid(void)\n");
                            fflush(stdout); }
  pds.set_gridid(219);

}

x::x(x &y) {
  nx = y.xpoints();
  ny = y.ypoints();
  dx = y.dx;
  dy = y.dy;
  xorig = y.xorig;
  yorig = y.yorig;
  sgn = y.sgn;
  slat = y.slat;
  slon = y.slon;

  grid = (float *) malloc(nx * ny * sizeof(float) );
  if (grid == (float *) NULL) { printf("Failed to mallocate in x(void)\n");
                            fflush(stdout); }
  pds.set_gridid(219);

}

void x::subgrid(latpt &ll, latpt &ur, x *newgrid) {
  fijpt llij, urij;
  int ni1, ni2;
  int nj1, nj2;
  ijpt loc;
  grid2_base<float> *tempx;

  llij = this->locate(ll);
  urij = this->locate(ur);
  ni1 = (int) (0.5 + llij.i);
  ni2 = (int) (0.5 + urij.i);
  nj1 = (int) (0.5 + llij.j);
  nj2 = (int) (0.5 + urij.j);

  newgrid->nx = (ni2 - ni1 ) + 1;
  newgrid->ny = (nj2 - nj1 ) + 1;
  newgrid->grid = (float *) realloc(newgrid->grid, newgrid->nx * newgrid->ny * sizeof(float) );

  tempx = new grid2_base<float>(newgrid->nx, newgrid->ny);
  *tempx = this->subset(ni1, nj1, ni2, nj2);
  for (loc.j = 0; loc.j < newgrid->ny; loc.j++) {
  for (loc.i = 0; loc.i < newgrid->nx; loc.i++) { 
    newgrid->grid[loc.i + loc.j*newgrid->nx ] = tempx->grid[loc.i + loc.j*newgrid->nx ];
  }
  }

}
 
int main(void) {
  x ini;
  x locmap;
  latpt ll, ur;
  palette<unsigned char> gg(19,65);

  FILE *fin;
  fin = fopen("north", "r");
  ini.binin(fin);

  ll.lat = 50.0;
  ll.lon = -90.0;
  ur.lat = 75.0;
  ur.lon = -60.0;

  ini.subgrid(ll, ur, &locmap);
  locmap *= 100.;
  locmap.xpm("north.xpm", 15, gg);

  return 0;
}
