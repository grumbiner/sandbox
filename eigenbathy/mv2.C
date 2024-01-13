#include "ncepgrids.h"

#define gravity 9.806
#define DX 10.
#define NX 2300
#define NMODES (NX)

#include "shared.C"

int main(void) {
  mvector<double> c2(NX), ddx(NX);
  mvector<double> etain (NX), etaout(NX);
  mvector<mvector<double> > eigenmodes(NMODES);
  mvector<double> values(NMODES);
  int i, iter, modes;
  double deltax = DX;

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
      d2dx2(c2, etain, etaout, deltax);
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
