#include "ncepgrids.h"
#include "params.h"
// Robert Grumbine 21 October 2008
// Compute divergence on a sphere

void divergence(llgrid<float> &u, llgrid<float> &v, llgrid<float> &div) ;

int main(int argc, char *argv[]) {
  global_ice<float> u, v, div;
  FILE *uin, *vin, *divout;
  float flag;
  ijpt loc;
  int i;

  uin = fopen(argv[1], "r");
  vin = fopen(argv[2], "r");
  divout = fopen(argv[3], "w");
  u.binin(uin);
  v.binin(vin);

  //printf("div max, min, avg, rms %e %e %e %e\n",div.gridmax(), div.gridmin(), div.average(), div.rms() );
  //printf("u max, min, avg, rms %e %e %e %e\n",u.gridmax(), u.gridmin(), u.average(), u.rms() );
  //printf("v max, min, avg, rms %e %e %e %e\n",v.gridmax(), v.gridmin(), v.average(), v.rms() );

  for (i = 0; i < u.xpoints()*u.ypoints(); i++) {
    if (fabs(u[i]) > 100) u[i] = 0;
    if (fabs(v[i]) > 100) v[i] = 0;
  }
  
  divergence(u, v, div);
  printf("div max, min, avg, rms %e %e %e %e\n",div.gridmax(), div.gridmin(), div.average(), div.rms() );
  printf("u max, min, avg, rms %e %e %e %e\n",u.gridmax(), u.gridmin(), u.average(), u.rms() );
  printf("v max, min, avg, rms %e %e %e %e\n",v.gridmax(), v.gridmin(), v.average(), v.rms() );
  div *= 1.e7;

  div.binout(divout);

  return 0;
}
void divergence(llgrid<float> &u, llgrid<float> &v, llgrid<float> &div) {
  ijpt loc, ip1, im1, jp1, jm1;
  float cosphi, phi, dphi, dlambda, rearth = parameters::a;
  latpt ll;
  
  dlambda = u.dlon / parameters::radians_per_degree;
  dphi    = v.dlat / parameters::radians_per_degree;
  
  for (loc.j = 1; loc.j < u.ypoints() - 1; loc.j++) {
    jp1.j = loc.j + 1;
    jm1.j = loc.j - 1;
    ip1.j = loc.j;
    im1.j = loc.j;
  for (loc.i = 1; loc.i < u.xpoints() - 1; loc.i++) {
    jp1.i = loc.i;
    jm1.i = loc.i;
    ip1.i = loc.i + 1;
    im1.i = loc.i - 1;

    ll = u.locate(loc);
    cosphi = cos(ll.lat); 
    div[loc] = ( (u[ip1] - u[im1])/2/dlambda + (v[jp1]-v[jm1])/2/dphi ) /
               rearth / cosphi;
  }
  }
 
  return;
}
