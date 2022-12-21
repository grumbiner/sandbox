#include <stdio.h>
#include <stdlib.h>

#include "ncepgrids.h"

template<class T>
float shaninfo(grid2<T> &x) ;

template <class T, class U>
void retype(grid2<T> &x, grid2<U> &y);

// Experiment with idea of collapsing 0.5 degree sst grid into 1 
//   degree field and a set of deltas on a half degree grid.
int main(int argc, char *argv[]) {
  global_ice<float> sst, delta, tmp5;
  global_sst<float> averaged, tmp1;
  FILE *fin;
  int prec = 2;
  int nbit;
  ijpt hires, lores;
  int ratio = 2;
  latpt ll;
  float magnitude;
  palette<unsigned char> gg(19,65);

  magnitude = atof(argv[1]);
  fin = fopen("sst","r");
  sst.binin(fin);
  fclose(fin);
  sst -= sst.gridmin();
  sst.grib_scale(prec, nbit, tmp5);
  printf("original field, nbits = %d\n",nbit);

  averaged.reduce(sst);
  averaged.grib_scale(prec, nbit, tmp1);
  printf("1 degree field, nbits = %d, bits = %d\n",nbit, 
           nbit*averaged.xpoints()*averaged.ypoints() );

  for (hires.j = 0; hires.j < delta.ypoints(); hires.j++) {
    lores.j = hires.j / ratio;
    for (hires.i = 0; hires.i < delta.xpoints(); hires.i++) {
      lores.i = hires.i / ratio;
      delta[hires] = sst[hires] - averaged[lores];
      if (fabs(delta[hires]) > magnitude) {
        ll = sst.locate(hires);
        printf("%f %f  %f %f %f\n",ll.lat, ll.lon, sst[hires], averaged[lores], delta[hires]);
      }
    }
  }
  delta.grib_scale(prec, nbit, tmp5);
  printf("delta field, nbits = %d, bits = %d\n",nbit, 
                nbit*delta.xpoints()*delta.ypoints() );
  printf("delta max, min, average %f %f %f\n",delta.gridmax(), delta.gridmin(), delta.average() );
  printf("averaged max, min, average %f %f %f\n",averaged.gridmax(), averaged.gridmin(), averaged.average() );
  printf("sst max, min, average %f %f %f\n",sst.gridmax(), sst.gridmin(), sst.average() );

  fin = fopen("averaged","w");
  averaged.binout(fin);
  fclose(fin);
  fin = fopen("delta","w");
  delta.binout(fin);
  fclose(fin);

  sst *= 10.;
  delta *= 10.;
  averaged *= 10.;
  printf("shannon info of sst     : %f\n",shaninfo(sst));
  printf("shannon info of delta   : %f\n",shaninfo(delta));
  printf("shannon info of averaged: %f\n",shaninfo(averaged));
  sst.laplace(tmp5);
  printf("shannon info of del2 sst: %f\n",shaninfo(tmp5)) ;
  printf("lap max, min, avg %f %f %f\n",tmp5.gridmax(), tmp5.gridmin(), tmp5.average() );
  sst.scale();
  sst.xpm("sst.xpm", 7, gg);
  tmp5.scale();
  tmp5.xpm("del2sst.xpm",7,gg);

  delta.laplace(tmp5);
  printf("shannon info of del2 del: %f\n",shaninfo(tmp5)) ;
  averaged.laplace(tmp1);
  printf("shannon info of del2 avg: %f\n",shaninfo(tmp1)) ;

  return 0;
}
// Assume unit precision
template<class T>
float shaninfo(grid2<T> &x) {
  int i;
  mvector<long int> *count;
  grid2<int> delta(x.xpoints(), x.ypoints() );
  double tempor, total=0.0;
  float xmax = x.gridmax(), xmin = x.gridmin();

  count = new mvector<long int>(xmax - xmin + 1);
  for (i = 0; i < count->xpoints(); i++) {
    count->operator[]( i )  = 0;
  }

  retype(x, delta);
  delta -= (int)xmin;
  //printf("shan delta grid max, min = %d %d\n",delta.gridmax(), 
  //                     delta.gridmin());
  //printf("count points = %d\n",count->xpoints());

  for (i = 0; i < delta.xpoints()*delta.ypoints(); i++) {
    count->operator[]( delta[i] ) += 1;
  }

  printf(" \n");
  for (i = 0; i < count->xpoints(); i++) {
    if (count->operator[](i) != 0) {
      printf("%d %d\n",i, count->operator[](i) );
      tempor = (float) count->operator[](i) / (float) (x.xpoints()*x.ypoints());
      total += -tempor * log(tempor);
    }
  }
  total /= log(2.);
  return (float) total;
}

template <class T, class U>
void retype(grid2<T> &x, grid2<U> &y) {
  int i;
  for (i = 0; i < x.xpoints()*x.ypoints(); i++) {
    y[i] = (U) x[i];
  }
  return;
}

