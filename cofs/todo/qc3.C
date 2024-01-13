#include <stdio.h>
#include "grid3.h"
#include "ncepgrids.h" 
#include "cofs.h"

// Program to conduct qc of today's otis file interpolated to cofs
//  grid.  Constructs a long term average file and a short term average
//  file.
// Robert Grumbine 15 August 1997

// Note that things following two slashes, such as started this line,
//  are comments.

#define NZ 19 

// Chalikov functions for stability correction
#ifdef LINUX
extern "C" void qcts_(int *l, int *la, float * z, float * t, float * s, 
                      float *gap, int *modcode);
#else
extern "C" void QCTS(int *l, int *la, float * z, float * t, float * s, 
                      float *gap, int *modcode);
#endif

int main(int argc, char *argv[]) {

  cfslevel<float> cfsmetric;

// CFS-related data:
  cfsgrid3<float> t3today(NZ), s3today(NZ);
  cfsgrid3<float> t3qc(NZ), s3qc(NZ);
  mvector<float> vert(NZ), ssound(NZ), tsound(NZ);

// Declare some other data and helper variables
  int i, j, k;
  FILE *fqc, *flongin, *fshortin, *ftoday, *flongout, *fshortout;
  float flag = -99.9;
  float tdel_gross = 10.0, sdel_gross = 5.0;
  float tdel_fine = 5.0,   sdel_fine = 5.0;
  float t_len_scale = 0.0023, s_len_scale = 0.0023;
  float short_weight = 0.40, long_weight = 0.03;
  float depth;
  int nx, ny, nz = NZ;
  ijpt xlocate;
  char fname[800];
  int modcode;


// Begin program by opening up the needed data files
  fqc    = fopen(argv[1], "r");
  flongin  = fopen(argv[2], "r");
  fshortin = fopen(argv[3], "r");
  ftoday = fopen(argv[4], "r");
  flongout  = fopen(argv[5], "w");
  fshortout = fopen(argv[6], "w");
  if (fqc == NULL || flongin == NULL || fshortin == NULL || ftoday == NULL
        || flongout == NULL || fshortout == NULL) {
    printf("Failed to open a required file!\n");
    if (fqc == NULL) printf(" -- qc file\n");
    if (flongin == NULL) printf(" -- long average input file\n");
    if (flongout == NULL) printf(" -- long average output file\n");
    if (fshortin == NULL) printf(" -- short average input file\n");
    if (fshortout == NULL) printf(" -- short average output file\n");
    if (ftoday    == NULL) printf(" -- today's interpolated otis file\n");
    return -1;
  }


  nx = t3qc.xpoints();
  ny = t3qc.ypoints();

// Now read in the qc and today's file 
  t3qc.binin(fqc);
  s3qc.binin(fqc);
  t3today.binin(ftoday);
  s3today.binin(ftoday);


//Perform gross error tests in this loop.  The logical testing on values
//  is being done in the if (       ) { 
//  section, inside the ()'s.  Change the logic as desired.  What is done
//  is to put the flag value (999) in place of the stored value for the
//  'today' grid.  When performing the averaging, values which are flagged
//  get ignored.  Logically, it would be possible to do the averaging within
//  this loop.  For reasons of style, I put them separately.  
//  fabs is the absolute value function for floating point numbers.
//  Logical tests are >, <, ==, >=, <=.  To negate any of the preceeding,
//    put a ! in front.  That is, != means not equal.  Note that I
//    put ==, rather than =.  A single = means put the value on the right
//    in to the variable on the left.  Two == means to make a logical
//    test.  The not equal is just !=.  Not greater than is !>.
//Note that indexing elements is rather bizzare (sp?).

  for (xlocate.j = 0; xlocate.j < ny; xlocate.j++) {
    for (xlocate.i = 0; xlocate.i < nx; xlocate.i++) {
       vert = cfsmetric.z ;
       vert *= ( - cfsmetric.depth[xlocate] );
       for (xlocate.k = 0; xlocate.k < NZ; xlocate.k++) {
          if (fabs(t3qc[xlocate] - t3today[xlocate] ) 
              > tdel_gross )    {
           t3today[xlocate] = flag;
          }
          if (fabs(s3qc[xlocate] - s3today[xlocate] ) 
              > sdel_gross )    {
           s3today[xlocate] = flag;
          }
       }
       // Done doing gross error checks, now do Chalikov stability check
       t3today.get_sounding(xlocate, tsound);
       s3today.get_sounding(xlocate, ssound);
       #ifdef LINUX
         qcts_(&nz, &nz, vert.vec, tsound.vec, ssound.vec, &flag, &modcode);
       #else
         QCTS(&nz, &nz, vert.vec, tsound.vec, ssound.vec, &flag, &modcode);
       #endif
       if (modcode != 0) {
         printf("xlocate restabilized %3d %3d %d\n", xlocate.i, 
                   xlocate.j, modcode);
         t3today.put_sounding(xlocate, tsound);
         s3today.put_sounding(xlocate, ssound);
       }
    }
  }

//[ts]3today now has the flag value in those points which look to have
//  changed by ludicrous amounts.
//Now read in and update the long term average for those points which
//  have not been flagged.
//The syntax below on t3qc.grid is a little strange to fortran eyes.
//    *= means to take the thing on the left, multiply it by the thing on
//       the right, and put the result into the thing on the left.
//    i.e. a *= 2 would be a = a * 2
//  similarly a += 2 would be a = a + 2.
//  After seeing the repulsive things we're doing to index elements of
//    the grid, you see why I don't want to repeat the item.
  t3qc.binin(flongin);
  s3qc.binin(flongin);
  for (xlocate.k = 0; xlocate.k < NZ; xlocate.k++) {
  for (xlocate.j = 0; xlocate.j < ny; xlocate.j++) {
  for (xlocate.i = 0; xlocate.i < nx; xlocate.i++) {
    if (t3today[xlocate] != flag) {
      t3qc[xlocate ] *= (1. - long_weight);
      t3qc[xlocate] += long_weight*t3today[xlocate] ;
    }
    if (s3today[xlocate] != flag) {
      s3qc[xlocate] *= (1. - long_weight);
      s3qc[xlocate] += long_weight*s3today[xlocate] ;
    }
  }
  }      
  }  
  t3qc.binout(flongout);
  s3qc.binout(flongout);
  fclose(flongout);
  sprintf(fname, "%s.asc", argv[5]);
  flongout = fopen(fname, "w");
  t3qc.printout(flongout);
  s3qc.printout(flongout);
  fclose(flongout);

  

// Now perform the fine QC checks.  Note that we're using today's updated
//  long term average.
  for (xlocate.j = 0; xlocate.j < ny; xlocate.j++) {
    for (xlocate.i = 0; xlocate.i < nx; xlocate.i++) {
       vert = cfsmetric.z ;
       vert *= ( - cfsmetric.depth[xlocate] );
       for (xlocate.k = 0; xlocate.k < NZ; xlocate.k++) {
          if (fabs(t3qc[xlocate] - t3today[xlocate] ) 
              > tdel_fine * exp(-t_len_scale*vert[xlocate.k] ) )    {
           t3today[xlocate] = flag;
          }
          if (fabs(s3qc[xlocate] - s3today[xlocate] ) 
              > sdel_fine * exp(-s_len_scale*vert[xlocate.k] ) )    {
           s3today[xlocate] = flag;
          }
       }
    }
  }

//  And at last, update the short term average.
  t3qc.binin(fshortin);
  s3qc.binin(fshortin);
  for (k = 0; k < NZ; k++) {
  for (j = 0; j < ny; j++) {
  for (i = 0; i < nx; i++) {
    if (t3today[k*nx*ny + j*nx + i] != flag) {
      t3qc[k*nx*ny + j*nx + i] *= (1. - short_weight);
      t3qc[k*nx*ny + j*nx + i] += short_weight*t3today[k*nx*ny + j*nx + i] ;
    }
    if (s3today[k*nx*ny + j*nx + i] != flag) {
      s3qc[k*nx*ny + j*nx + i] *= (1. - short_weight);
      s3qc[k*nx*ny + j*nx + i] += short_weight*s3today[k*nx*ny + j*nx + i] ;
    }
  }
  }      
  }  
  t3qc.binout(fshortout);
  s3qc.binout(fshortout);
  fclose(fshortout);
  sprintf(fname, "%s.asc", argv[6]);
  fshortout = fopen(fname, "w");
  t3qc.printout(fshortout);
  s3qc.printout(fshortout);
  fclose(flongout);

  return 0;
}
