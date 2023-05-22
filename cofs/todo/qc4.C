#include <stdio.h>
#include "grid3.h"
#include "ncepgrids.h" 
#include "cofs.h"

#define NZ 19 

// Chalikov functions for stability correction
#ifdef LINUX
extern "C" void qcts_(int *l, int *la, float * z, float * t, float * s, 
                      float *gap, int *modcode);
#else
extern "C" void QCTS(int *l, int *la, float * z, float * t, float * s, 
                      float *gap, int *modcode);
#endif

// Utility for determining weights.
int setweight(mvector<float> &weight, mvector<float> &tsound, 
              mvector<float> &tclim, metricvector<float> &dtlim) ;

int main(int argc, char *argv[]) {

  cfslevel<float> cfsmetric;

// CFS-related data:
  cfsgrid3<float> t3today(NZ), s3today(NZ);
  cfsgrid3<float> t3qc(NZ), s3qc(NZ);
  mvector<float> vert(NZ), ssound(NZ), tsound(NZ);
  mvector<float> tclim(NZ), sclim(NZ);

// Falkovich vectors for qc purposes:
  mvector<float> falkdep(7);
  metricvector<float> dt(7), ds(7);
  metricvector<float> dtlim(NZ), dslim(NZ);
  mvector<float> tweight(NZ), sweight(NZ);

// Declare some other data and helper variables
  int i, j, k;
  FILE *fqc, *ftoday, *fqcout;
  float flag = -99.0;
  float long_weight = 0.40;
  float depth;
  int nx, ny, nz = NZ;
  ijpt xlocate;
  char fname[800];
  int modcode, passmod, chmods, ftmods, fsmods;
  int tot_chmods, tot_ftmods, tot_fsmods;

// Begin program by opening up the needed data files
  fqc    = fopen(argv[1], "r");
  ftoday = fopen(argv[2], "r");
  fqcout  = fopen(argv[3], "w");
  if (fqc == NULL || ftoday == NULL || fqcout == NULL ) {
    printf("Failed to open a required file!\n");
    if (fqc == NULL) printf(" -- qc file\n");
    if (fqcout == NULL) printf(" -- long average output file\n");
    if (ftoday    == NULL) printf(" -- today's interpolated otis file\n");
    return -1;
  }


  nx = t3qc.xpoints();
  ny = t3qc.ypoints();

// Set up the Falkovich QC vectors:
  falkdep[0] = 0.;
  falkdep[1] = 50.;
  falkdep[2] = 100.;
  falkdep[3] = 700.;
  falkdep[4] = 1000.;
  falkdep[5] = 2000.;
  falkdep[6] = 5000.;
  dt.set_metric(falkdep);
  ds.set_metric(falkdep);
  dt[0] = 3.5;
  dt[1] = 4.0;
  dt[2] = 4.0;
  dt[3] = 3.3;
  dt[4] = 1.9;
  dt[5] = 0.2;
  dt[6] = 0.1;
  ds[0] = 1.2;
  ds[1] = 1.0;
  ds[2] = 0.8;
  ds[3] = 0.4;
  ds[4] = 0.2;
  ds[5] = 0.1;
  ds[6] = 0.05;
// Print a sample of the limits for a 5000 m deep water column:
  vert = cfsmetric.z ;
  vert *= (-falkdep[6]);
  printf("vertical depths on cofs metric for 5000 m\n");
//CD  vert.printer();
  dtlim.set_metric(vert);
  dslim.set_metric(vert);
  dtlim.interp(dt, flag);
  dslim.interp(ds, flag);
  printf("sample dt limits\n");
  dtlim.printer(stdout);
  printf("sample ds limits\n");
  dslim.printer(stdout);

  
// Now read in the qc and today's file 
  t3qc.binin(fqc);
  s3qc.binin(fqc);
  t3today.binin(ftoday);
  s3today.binin(ftoday);


// Start qc process.  Loop around the grid, doing qc in each vertical column
//   one at a time.
  tot_chmods = 0;
  tot_fsmods = 0;
  tot_ftmods = 0;
  for (xlocate.j = 0; xlocate.j < ny; xlocate.j++) {
    for (xlocate.i = 0; xlocate.i < nx; xlocate.i++) {
       vert = cfsmetric.z ;
       vert *= ( - cfsmetric.depth[xlocate] );
       passmod = 0;

       // Now do Chalikov stability check
       t3today.get_sounding(xlocate, tsound);
       s3today.get_sounding(xlocate, ssound);
       #ifdef LINUX
         qcts_(&nz, &nz, vert.vec, tsound.vec, ssound.vec, &flag, &modcode);
       #else
         QCTS(&nz, &nz, vert.vec, tsound.vec, ssound.vec, &flag, &modcode);
       #endif
       if (modcode != 0) {
         tot_chmods += modcode;
         passmod = modcode;
         t3today.put_sounding(xlocate, tsound);
         s3today.put_sounding(xlocate, ssound);
       }

       // Now that the column is (marginally) stable, perform qc check
       //   against climatology (t3qc, s3qc)
         // First, find the climatology and get data limits put in to 
         //   local vector.
       t3qc.get_sounding(xlocate, tclim);
       s3qc.get_sounding(xlocate, sclim);
       dtlim.set_metric(vert);
       dslim.set_metric(vert);
       dtlim.interp(dt, flag);
       dslim.interp(ds, flag);
//CD       dtlim.printer();
//CD       dslim.printer();
       modcode = setweight(tweight, tsound, tclim, dtlim);
       passmod += modcode;
       tot_ftmods += modcode; 
       modcode = setweight(sweight, ssound, sclim, dslim);
       passmod += modcode;
       tot_fsmods += modcode;

       if (passmod != 0) {
         printf("%6d modifications at %3d %3d \n", passmod, xlocate.i, 
                   xlocate.j );
       }
       // Finally, given weights, compute the qc'd version of today's
       //   file.
       for (xlocate.k = 0; xlocate.k < NZ; xlocate.k++) {
          t3qc[xlocate.k*nx*ny + xlocate.j*nx + xlocate.i] *= 
               (1. - long_weight*tweight[xlocate.k]);
          t3qc[xlocate.k*nx*ny + xlocate.j*nx + xlocate.i] += 
               long_weight*tweight[xlocate.k]*t3today[xlocate] ;
          s3qc[xlocate.k*nx*ny + xlocate.j*nx + xlocate.i] *= 
               (1. - long_weight*sweight[xlocate.k]);
          s3qc[xlocate.k*nx*ny + xlocate.j*nx + xlocate.i] += 
               long_weight*sweight[xlocate.k]*s3today[xlocate] ;
       }
       

    }
  }
  printf("Total chmods %d ftmods %d fsmods %d\n",
                 tot_chmods, tot_ftmods, tot_fsmods);

  t3qc.binout(fqcout);
  s3qc.binout(fqcout);
  fclose(fqcout);
  sprintf(fname, "%s.asc", argv[3]);
  fqcout = fopen(fname, "w");
  t3qc.printout(fqcout);
  s3qc.printout(fqcout);
  fclose(fqcout);

  return 0;

}

int setweight(mvector<float> &weight, mvector<float> &tsound, 
              mvector<float> &tclim,  metricvector<float> &dtlim) {
   int i, mods;
   float tempor;

   mods = 0;
   for (i = 0; i < weight.xpoints(); i++) {
      tempor = fabs(tsound[i] - tclim[i]) / dtlim[i] ;
      if ( tempor <= 1. )  { 
        weight[i] = 1.;
      }
      else if (tempor >= 2. ) {
        weight[i] = 0.;
        mods += 1;
      }
      else {
        weight[i] = 2.0 - tempor;
        mods += 1;
      }
   }

   return mods;

}
