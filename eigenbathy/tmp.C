#include "ncepgrids.h"

#define gravity 9.806
#define DX 1000.
#define NX 200
#define NMODES 5

#include "shared.C"

int main(void) {
  mvector<double> bathy(NX), eta(NX);
  mvector<double> c2(NX), ddx(NX), div(NX);
  mvector<double> etain (NX), etaout(NX);
  mvector<mvector<double> > eigenmodes(NMODES);
  mvector<double> values(NMODES);
  int i, iter, modes;
  double deltax = DX;

  c2       = 4080.;
  c2[0]    = 0.;
  c2[NX-1] = 0.;
  c2 *= gravity;
  srand(1);

  for (modes = 0; modes < NMODES;  modes++) {
    eigenmodes[modes].resize(NX);
    eta = 1.;
    for (i = 0; i < NX; i++) {
      eta[i] = pow(i,modes);
    }
    eta /= sqrt( dot(eta,eta) );

    if (modes != 0) {
      orthog(eta, eigenmodes, modes);
    }
      
    show(eta);
    eigenmodes[modes] = eta;

  } // modes


  return 0;
}
