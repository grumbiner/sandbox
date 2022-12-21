#include "ncepgrids.h"

#define gravity 9.806
#define DX 1000.
#define NX 23
#define NMODES (NX-2)

void orthog(mvector<double> &x, mvector<mvector<double> > &y, int modemax) ;
double dot(mvector<double> &x, mvector<double> &y) ;

void random(mvector<double> &x) ;
void show(mvector<double> &eta) ;
void gradients(mvector<double> &eta, mvector<double> &dx);
void divergence(mvector<double> &dx, mvector<double> &div);

int main(void) {
  mvector<double> c2(NX), ddx(NX), div(NX);
  mvector<double> etain (NX), etaout(NX);
  mvector<mvector<double> > eigenmodes(NMODES);
  mvector<double> values(NMODES);
  int i, iter, modes;

  c2       = 408.0;
  c2[0]    = 0.;
  c2[NX-1] = 0.;
  c2 *= gravity;
  srand(1);

  for (modes = 0; modes < NMODES;  modes++) {
    eigenmodes[modes].resize(NX);

    // Initialize with uniformly random field [-0.5,0.5)
    if (modes != 0) {
      random(etain);
      etain -= 0.5;
      etain /= sqrt( dot(etain,etain) );
    }
    else {
      for (i = 0; i < NX; i++) {
        etain[i] = sin((float)i*M_PI/((float) NX));
      }
      etain /= sqrt( dot(etain,etain) );
      show(etain);
    }

    // Iterations:
    for (iter = 0; iter < 500; iter++) {
      gradients(etain, ddx);
    
      ddx *= c2;
      divergence(ddx, div);
      etaout = div;
      values[modes] = sqrt(  dot(etain,etain)/dot(etaout,etaout)  );
      etaout *= values[modes];
  
      if (modes != 0) {
        orthog(etaout, eigenmodes, modes);
      }
      etain = etaout;
      
    }
    show(etain);
    eigenmodes[modes] = etain;

  } // modes

  //for (i = 0; i < NMODES; i++) {
  //  printf("val %d %e %e  %e\n",i,values[i], 1./values[i], sqrt(values[i]));
  //}
  //int j;
  //for (i = 0; i < NMODES ; i++) {
  //  for (j = i; j < NMODES; j++) {
  //    printf("dot %d %d  %e\n",i, j, dot(eigenmodes[i],eigenmodes[j]));
  //  }
  //}

  return 0;
}
// orthogonalize x w.r.t. each of the first modemax members of y
void orthog(mvector<double> &x, mvector<mvector<double> > &y, int modemax) {
  int i, n;
  double rx, ry, tmp;
  mvector<double> tvec(x.xpoints());

  for (n = 0; n < modemax; n++) {
    tmp = dot(x,y[n]);
    rx  = dot(x,x);
    ry  = dot(y[n],y[n]);
    //printf("%d %d  %e %e %e %d\n",n, modemax, tmp, rx, ry, y[n].xpoints() );
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
void gradients(mvector<double> &eta, mvector<double> &dx) {
  int i;
  dx = 0.0;
  for (i = 1; i < dx.xpoints() - 1 ; i++) {
    dx[i] = (eta[i+1]-eta[i-1])/2./DX;
  }

  return;
}
void divergence(mvector<double> &dx, mvector<double> &div) {
  int i;
  div = 0.0;

  for (i = 1; i < dx.xpoints() - 1 ; i++) {
    div[i] = (dx[i+1]-dx[i-1])/2./DX;
  }

  return;
}
