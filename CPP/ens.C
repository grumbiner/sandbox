#include "ncepgrids.h"

void flovel(grid2<float> &tmpu, grid2<float> &tmpv, const fijpt &floc, fijpt &dx) ;

int main(int argc, char *argv[]) {
  mrf1deg<float> tmpu, tmpv;
  FILE *fin;
  int i;
  latpt ll;
  fijpt dx, floc;
  float dt = 6.*3600.;

  fin = fopen(argv[1], "r");
  if (fin == (FILE*) NULL) {
    printf("failed to open winds\n");
    return 1;
  }

  ll.lat = 60.0; ll.lon = 180.0;
  for (i = 0; i < 64; i++) {
    tmpu.binin(fin); 
    tmpv.binin(fin); 

    // work from native grid:
    floc = tmpu.locate(ll);
    flovel(tmpu, tmpv, floc, dx);

    dx *= dt * 0.02; // approx downscaling for ice motion
    dx.i /= parameters::m_per_degree * 
                cos(parameters::radians_per_degree*ll.lat);
    dx.j /= parameters::m_per_degree;
    
    ll.lon += dx.i; 
    ll.lat += dx.j;
    printf("dx, dy %f %f ll = %f %f floc %f %f \n",dx.i, dx.j, ll.lat, ll.lon,
            floc.i, floc.j); fflush(stdout);

  }

  return 0;

}

// do a bilinear interpolation to the fijpt
void flovel(grid2<float> &u, grid2<float> &v, const fijpt &floc, fijpt &dx) {
  ijpt ll, lr, ul, ur;
  float eta, lambda;

  ll.i = (int) floc.i;
  ll.j = (int) floc.j;
  lr.i = ll.i + 1;
  lr.j = ll.j;
  ul.i = ll.i;
  ul.j = ll.j + 1;
  ur.i = ll.i + 1;
  ur.j = ll.i + 1;

  eta    = floc.i - ll.i;
  lambda = floc.j - ll.j; 

  dx.i = (1. - lambda)* ( (1.-eta)*u[ll] + eta*u[lr] ) + 
               lambda * ( (1.-eta)*u[ul] + eta*u[ur] )   ;
  dx.j = (1. - lambda)* ( (1.-eta)*v[ll] + eta*v[lr] ) + 
               lambda * ( (1.-eta)*v[ul] + eta*v[ur] )   ;

  return;
}
