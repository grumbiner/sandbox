#include <stdio.h>

#include "mvector.h"
#include "time_series.h"
#include "grid_base.h"

#define NDAYS 365
#define NX_NORTH 385

void enmask(mvector<float> &x, float maskval) ;
void demean(mvector<float> &x, float maskval);
void specprint(mvector<float> &re, mvector<float> &im, FILE *fout) ;

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  grid2_base<float> north(NX_NORTH,NDAYS);
  mvector<float> tmp(NDAYS), ccors(2*NDAYS+1);
  mvector<float> re(512), im(512);
  time_series<float> a(NDAYS), b(NDAYS);
  ijpt x, y;
  int i, j, deltax;
  float maskval = -1./128.;
  float c0, c0b;

  fin = fopen(argv[1], "r");
  north.binin(fin);
  x.j = 0;
  y.j = north.ypoints() - 1;
  ccors = (float) 0.0;

  fout = fopen(argv[2], "w");
  if (fout == (FILE *) NULL) {
    printf("Failed to open an output file\n");
    return 1;
  }
  deltax = atoi(argv[3]);

//Loop over all i's  (files are split by j of original grid)
  i = 0;


  for (i = 1; i < north.xpoints()-deltax ; i++) {
    x.i = i;
    y.i = i;
    north.get_transect(x, y, tmp);
    b.set(tmp);
    enmask(b, maskval);
    demean(b, maskval);
    c0b = b.autocovary(maskval, 0);

    x.i = i+deltax;
    y.i = i+deltax;
    north.get_transect(x, y, tmp);
    a.set(tmp);
    enmask(a, maskval);
    demean(a, maskval);
    c0 = a.autocovary(maskval, 0);

    if (a.complete(maskval) > 0.90) {
      //CDa.printer(stdout);
      a.fft(re, im);
      //CDre.printer(stdout); im.printer(stdout);
      printf("i = %d\n",i);
      specprint(re, im, fout);
    }
      

    //Find the cross-correlations
//    for (j = -NDAYS; j <= NDAYS ; j++) { //cross correlation w. previous
//      ccors.vec[j+NDAYS] = a.crossvary(b, maskval, j)/sqrt(c0*c0b) ;
//    }
//    //Only print out if there's a large one, on a fairly complete record:
//    if ( (ccors.maximum() > 0.5 || ccors.minimum() < -0.5) && 
//         (a.complete(maskval) > 0.5 && b.complete(maskval) > 0.5)  ) {
//      for (j = -NDAYS; j < NDAYS; j++) {
//         fprintf(fout, "%3d %3d  %6.3f\n", i, j, ccors[j+NDAYS] ); 
//      }
//      printf("i = %3d\n",i+deltax);
//      a.printer(stdout);
//    }

  }

  return 0;

}    
void enmask(mvector<float> &x, float maskval) {
  int i;
  for (i = 0; i < x.xpoints(); i++) {
    if (x[i] > 1.28 ) x[i] = maskval;
  }
  return;
}

void demean(mvector<float> &x, float maskval) {
  int i;
  float mean;

  mean = x.average(maskval);
  for (i = 0; i < x.xpoints() ; i++) {
    if (x[i] != maskval) x[i] -= mean;
  }
  return;
}
void specprint(mvector<float> &re, mvector<float> &im, FILE *fout) {
  int i;
  fprintf(fout, "%8.3f %7.5f %8.3f \n",(float) 2*re.xpoints(), 0.0,
               re[0]*re[0] + im[0]*im[0]);

  for (i = 1; i < re.xpoints() / 2 + 1; i++) {
    fprintf(fout, "%8.3f %7.5f %8.3f \n",(float) re.xpoints()/(float) i, 
                   (float) i / (float) re.xpoints(),
                   re[i]*re[i] + im[i]*im[i]);
  }
  return;
} 
  
