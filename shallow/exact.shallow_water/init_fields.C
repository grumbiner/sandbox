//gee
#include "grid_math.h"

void init_fields(int nx, int ny, grid2<DTYPE> &h, grid2<DTYPE> &u, 
                 grid2<DTYPE> &v, grid2<DTYPE> *eta, grid2<DTYPE> &tmpeta,
                 int &myrank, int &global_nx, int &global_ny) {

  ijpt loc, center;
  int i;
// Now on with the universalities:

  tmpeta.resize(nx, ny);
  h.resize(nx, ny);
  h.set((DTYPE) 100.);
  u.resize(nx, ny);
  v.resize(nx, ny);
  u.set((DTYPE) 0.);
  v.set((DTYPE) 0.);
  printf("h gridmax %f, max wave speed %f\n",h.gridmax(), sqrt(h.gridmax()*gee) );

  for (i = 0; i < 3; i++) {
    eta[i].resize(nx, ny);
    eta[i].set((DTYPE) 0.);
  }

// Initial conditions
  int dj;
  center.i = global_nx/2;
  center.j = global_ny/16;  // ensure it starts in southernmost box
  printf("center at %d %d dx dy = %f %f nx ny = %d %d\n",center.i, center.j, dx, dy, nx, ny);
  for (loc.j = 0; loc.j < ny; loc.j++) {
    dj = center.j - ny*myrank - loc.j;
    for (i = -nx/2; i < nx/2; i++) {
      loc.i = center.i + i;
      eta[2][loc] = 1.0 * exp(- (i*i*dx*dx+dj*dj*dy*dy) / (250*60)*(250*60) );
      eta[1][loc] = 1.0 * exp(- (i*i*dx*dx+dj*dj*dy*dy) / (250*60)*(250*60) );
      eta[0][loc] = 1.0 * exp(- (i*i*dx*dx+dj*dj*dy*dy) / (250*60)*(250*60) );
    }
  }

   for (i = 0; i < 3; i++) {
      printf("proc %5d Initial conditions max step %d is %e\n",myrank, i, eta[i].gridmax());
   }
   fflush(stdout);

  return;
}
