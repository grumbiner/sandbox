#include <stdio.h>
#include "grid3.h"
#include "ncepgrids.h"
#include "cofs.h"

// Program to test the horizontal interpolation between Regional COFS 
//   Ocean Model and Regular COFS
// 

int main(int argc, char *argv[]) {

  cfslevel<float> cfsmetric;

// CFS-related data:
  cfsgrid<float> cfsout, cfsmask;
  cfsreg<float> dummy_cfsreg;

// Regional COFS model data:
  cfsreg<float> otisin, tmask;
  cfsreg<float> reinterp;

// Utility variables:
  latpt alat;
  ijpt x, xijpt2;
  fijpt y, cofsloc, dummyloc;
  latpt otisloc;
  FILE *fin, *fout;
  char fname[600];
  int i, j, k;
  int count;
  float nonval = -99.9, maskval = 0.0;
  float avg, rms;

// First, read in the restart file and the mask file:
  tmask.set(5.);
  cfsmask.set(5.);

  for (j = 0; j < otisin.ypoints() ; j++) {
    for (i = 0; i < otisin.xpoints() ; i++) {
       otisin[i + j*otisin.xpoints() ] = i * j;
    }
  }

  cfsmetric.from( otisin, tmask, maskval, nonval, cfsout );
  cfsout.binout(fout);
  fclose(fout);
 
  cfsmetric = cfsout;
  cfsmetric.from( cfsmetric, cfsmask, maskval, nonval, reinterp ); 

// Do this so as to not penalize points flagged for being off the
//   cofs grid
  count = 0;
  for (x.j = 0; x.j < reinterp.ypoints(); x.j++ ) {
    for (x.i = 0; x.i < reinterp.xpoints(); x.i++) {
      otisloc = reinterp.locate(x);
      cofsloc = cfsmetric.locate(otisloc);
      if ( cfsmetric.in(cofsloc) ) {
        // ok
        //printf("htest %3d %3d %7.2f %6.2f  %7.2f %7.2f\n",x.i, x.j,
        //   otisloc.lon - 360., otisloc.lat, cofsloc.i, cofsloc.j);
      }
      else {
        reinterp[x] = otisin[x];
        count += 1;
      }
      xijpt2.i = (int) (cofsloc.i + 0.5);
      xijpt2.j = (int) (cofsloc.j + 0.5);
      if (cfsmetric.land(xijpt2) && reinterp[x] != otisin[x]) {
        reinterp[x] = otisin[x];
        count += 1;
      }
      if ( fabs(reinterp[x] - otisin[x]) > 10) {
        printf("Extreme delta at %3d %3d \n",x.i, x.j);
        reinterp[x] = otisin[x];
        count += 1;
      } 

    }
  }
  printf("%d points were reset \n",count);

  reinterp -= otisin;
  avg = reinterp.average();
  rms = reinterp.rms(); 
  printf("average, rms difference between original and reinterpolated = %f %f\n",avg, rms);
  avg = otisin.average();
  rms = otisin.rms();
  printf("original av, rms %f %f\n",avg, rms);

  for (x.j = 0; x.j < reinterp.ypoints(); x.j++ ) {
    for (x.i = 0; x.i < reinterp.xpoints(); x.i++) {
      if ( fabs(reinterp[x]) > 7.5 ) {
        otisloc = reinterp.locate(x);
        printf("%3d %3d %7.2f %6.2f orig %6.2f delta %7.2f new %6.2f\n",
           x.i, x.j, otisloc.lon - 360., otisloc.lat, otisin[x],
           reinterp[x], otisin[x] + reinterp[x]);
      }
    }
  }


  return 0;
  
}
