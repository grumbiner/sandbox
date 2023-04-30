#include <stdio.h>
#include <stdlib.h> 
#include "grid3.h"
#include "ncepgrids.h" 
#include "cofs.h"

// Program to conduct span an n-day gap in the cofs(otis) fields.
//  Input is two otis fields interpolated to cofs grids, and the 
//  number of days that are in between them.  i.e., tsout.971204 and
//  tsout.971207 have a gap of 2 days.  Output is a linear interpolation
//  of the given data.  
// Robert Grumbine 18 December 1997

// Note that things following two slashes, such as started this line,
//  are comments.

#define NZ 19 

int main(int argc, char *argv[]) {

// CFS-related data:
  cfsgrid3<float> t3new(NZ), s3new(NZ);
  cfsgrid3<float> t3old(NZ), s3old(NZ);
  cfsgrid3<float> interp1(NZ), interp2(NZ);

// Declare some other data and helper variables
  int i, j, k;
  FILE *fnew, *fold, *fout;
  float flag = 999.;
  float alpha;
  int dinterp;
  int nx, ny;
  char fname[800];

// Begin program by opening up the needed data files
  fnew  = fopen(argv[1], "r");
  fold  = fopen(argv[2], "r");
  if (fnew == NULL || fold == NULL ) {
    printf("Failed to open a required file!\n");
    if (fnew == NULL) printf(" -- new file\n");
    if (fold == NULL) printf(" -- old file\n");
    return -1;
  }
  dinterp = atoi(argv[3]);

  nx = t3new.xpoints();
  ny = t3new.ypoints();

// Now read in the new and today's file 
  t3new.binin(fnew);
  s3new.binin(fnew);
  t3old.binin(fold);
  s3old.binin(fold);

  for (j = 1; j <= dinterp; j++) {
    sprintf(fname,"interpout.%1d",j); 
    fout = fopen(fname,"w");

    interp1 = t3new;
    alpha = ((float) j / (float) (dinterp+1)) ;
    interp1 *= alpha;
    interp2 = t3old;
    alpha  = ((float) (dinterp+1 -j) / (float)(dinterp+1) );
    interp2 *= alpha;
    interp1 += interp2;
    interp1.binout(fout);

    interp1 = s3new;
    alpha = ((float) j / (float) (dinterp+1)) ;
    interp1 *= alpha;
    interp2 = s3old;
    alpha  = ((float) (dinterp+1 -j) / (float)(dinterp+1) );
    interp2 *= alpha;
    interp1 += interp2;
    interp1.binout(fout);

    fclose(fout);
  }

  return 0;
}
