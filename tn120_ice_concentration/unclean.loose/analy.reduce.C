#include <stdio.h>

#include "ncepgrids.h"
template <class T>
class icefamily : public northgrid<T> {
  public:
    icefamily(float = 1.0);
};
template <class T>
icefamily<T>::icefamily(float n) {
  nx = (int) (0.5 + nx*n);
  ny = (int) (0.5 + ny*n);
  dx /= n;
  dy /= n;
  delete []grid;
  grid = new T[nx*ny];
  pds.set_gridid(255);
  return;
}

int main(void) {
  FILE *fin, *fout;
  northgrid<float> nin, nmask;
  icefamily<float> nout(0.5);
  palette<unsigned char> gg(19,65);
  ijpt x;
  float mask = -99.;

  fin = fopen("north","r");
  fout = fopen("nreduce","w");
  nin.binin(fin);
  if (nin.average() < 2.56 ) nin *= 100.;
  nmask.set(0.0);
  for (x.j = 0; x.j < nin.ypoints(); x.j++) {
  for (x.i = 0; x.i < nin.xpoints(); x.i++) {
     if (nin[x] > 128.) { nmask[x] = mask; }
  }
  }
  printf("Finished reading in the data\n"); fflush(stdout);

//  nout.reduce(nin);
  nout.reduce(nin, nmask, mask);
  printf("Returned from the reducer \n"); fflush(stdout);

  nout.xpm("nout.xpm",12,gg);
  nout.binout(fout);

  nin.xpm("nin.xpm",12,gg);

  printf("nout max, min %f %f\n",nout.gridmax(), nout.gridmin() );
  printf("nin max, min %f %f\n",nin.gridmax(), nin.gridmin() );

  return 0;
}

