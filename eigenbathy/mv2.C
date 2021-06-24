#include "ncepgrids.h"

#define gravity 9.806
#define DX 10.
#define NX 2300
#define NMODES (NX)

void orthog(mvector<double> &x, mvector<mvector<double> > &y, int modemax) ;
double dot(mvector<double> &x, mvector<double> &y) ;

void random(mvector<double> &x) ;
void show(mvector<double> &eta) ;

void d2dx2(mvector<double> &eta, mvector<double> &out);
void d2dx2(mvector<double> &c2, mvector<double> &eta, mvector<double> &out);

int main(void) {
  mvector<double> c2(NX), ddx(NX);
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
      //show(etain);
    }

    // Iterations:
    for (iter = 0; iter < 500; iter++) {
      ddx = etain; 
      ddx *= c2;
      //d2dx2(ddx, etaout);
      d2dx2(c2, etain, etaout);
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

  FILE *fout;
  fout = fopen("eigenvalout", "w");
  for (i = 0; i < NMODES; i++) {
    fprintf(fout, "val %d %e %e  %e\n",i,values[i], 1./values[i], sqrt(values[i]));
  }
  int j;
  for (i = 0; i < NMODES ; i++) {
    for (j = i; j < NMODES; j++) {
      fprintf(fout, "dot %d %d  %e\n",i, j, dot(eigenmodes[i],eigenmodes[j]));
    }
  }

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

void d2dx2(mvector<double> &eta, mvector<double> &out) {
  int i;
  out = 0.;
  for (i = 1; i < eta.xpoints() - 1; i++) {
    out[i] = (eta[i+1]-2.*eta[i]+eta[i-1])/DX/DX;
  }
  return;
}
void d2dx2(mvector<double> &c2, mvector<double> &eta, mvector<double> &out) {
  int i;
  out = 0.;
  for (i = 1; i < eta.xpoints() - 1; i++) {
    out[i] = c2[i]*(eta[i+1]-2.*eta[i]+eta[i-1])/DX/DX;
    out[i] += (eta[i+1]-eta[i-1])*(c2[i+1]-c2[i-1])/4./DX/DX;
  }
  return;
}
