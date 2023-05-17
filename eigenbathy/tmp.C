#include "ncepgrids.h"

#define gravity 9.806
#define DX 1000.
#define NX 200
#define NMODES 5

void orthog(mvector<double> &x, mvector<mvector<double> > &y, int modemax) ;
double dot(mvector<double> &x, mvector<double> &y) ;

void random(mvector<double> &x) ;
void show(mvector<double> &eta) ;
void gradients(mvector<double> &eta, mvector<double> &dx);
void divergence(mvector<double> &dx, mvector<double> &div);

int main(void) {
  mvector<double> bathy(NX), eta(NX);
  mvector<double> c2(NX), ddx(NX), div(NX);
  mvector<double> etain (NX), etaout(NX);
  mvector<mvector<double> > eigenmodes(NMODES);
  mvector<double> values(NMODES);
  int i, iter, modes;

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
// orthogonalize x w.r.t. each of the first modemax members of y
void orthog(mvector<double> &x, mvector<mvector<double> > &y, int modemax) {
  int i, n;
  double rx, ry, tmp;
  mvector<double> tvec(x.xpoints());

  for (n = 0; n < modemax; n++) {
    tmp = dot(x,y[n]);
    ry  = dot(y[n],y[n]);
    tvec = y[n];
    tvec *= tmp/ry;
    x -= tvec;
  }
  x /= sqrt(dot(x,x));

  return;
}
double dot(mvector<double> &x, mvector<double> &y) {
  double sum = 0.0;
  for (int i = 0; i < x.xpoints(); i++) {
    sum += x[i]*y[i];
  }
  return (double) sum;
}

void show(mvector<double> &eta) {
  int loc;
  for (loc = 0; loc < eta.xpoints(); loc++) {
      printf("%3d  %12.5e\n",loc, eta[loc]);
  }

  return ;
}

void random(mvector<double> &x) {
  for (int loc = 0; loc < x.xpoints() ; loc++) {
    x[loc] = drand48();
  }
  return;
}
