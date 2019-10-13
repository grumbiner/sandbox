#include <stdio.h>
#include <stdlib.h>

#include "ncepgrids.h"

//Program to interpolate from the hurricane model to the regional
//  wave model
//Robert Grumbine 29 March 1999

int splice(llgrid<float> *lowfield, llgrid<float> *lowmask, 
         llgrid<float> *highfield, llgrid<float> *highmask, 
         float maskval, ECG_hur<float> &wavefield, ECG_hur<float> &tfield, 
         float nonval, char *finbase, char *foutbase) ;

int main(int argc, char *argv[]) {
  int nx, ny;
  float lat1, lon1, dlat, dlon;
  llgrid<float> *lowfield, *lowmask;
  llgrid<float> *highfield, *highmask;
  ECG_hur<float> wavefield, tfield;
  float nonval = -666.0, maskval = 99.0;
  char *finbase, *foutbase;
  float tempor;

// Establish the low resolution grid:
  nx   = atoi(argv[1]);
  ny   = atoi(argv[2]);
  lat1 = atof(argv[3]);
  dlat = atof(argv[4]);
  lon1 = atof(argv[5]);
  dlon = atof(argv[6]);
  tempor = (float) ((int)(0.5 + 1./dlat) ) ;
  tempor = 1. / tempor;
  if (fabs(tempor - dlat) <= 0.002 ) {
    printf("changing dlat from %f to %f \n",dlat, tempor);
    dlat = tempor;
  }
  tempor = (float) ((int)(0.5 + 1./dlon) ) ;
  tempor = 1. / tempor;
  if (fabs(tempor - dlon) <= 0.002 ) {
    printf("changing dlon from %f to %f \n",dlon, tempor);
    dlon = tempor;
  }
  lowfield = new llgrid<float>(nx, ny, dlat, dlon, lat1, lon1);
  lowmask = new llgrid<float>(nx, ny, dlat, dlon, lat1, lon1);
  lowmask->set(0.);
  printf("Passed construction of low\n");

// Establish the high resolution grid:
  nx   = atoi(argv[7]);
  ny   = atoi(argv[8]);
  lat1 = atof(argv[9]);
  dlat = atof(argv[10]);
  lon1 = atof(argv[11]);
  dlon = atof(argv[12]);
  tempor = (float) ((int)(0.5 + 1./dlat) ) ;
  tempor = 1. / tempor;
  if (fabs(tempor - dlat) <= 0.002 ) {
    printf("high changing dlat from %f to %f \n",dlat, tempor);
    dlat = tempor;
    tempor = (float) ((int) (lat1 / (dlat/2.)  + lat1/fabs(lat1)* 0.5));
    tempor *= dlat/2.;
    printf("lat1, tempor %f %f \n", lat1, tempor);
    if (fabs(tempor - lat1) < 0.002) {
      printf("high changing lat1 from %f to %f\n",lat1, tempor);
      lat1 = tempor;
    }
  }
  tempor = (float) ((int)(0.5 + 1./dlon) ) ;
  tempor = 1. / tempor;
  if (fabs(tempor - dlon) <= 0.002 ) {
    printf("high changing dlon from %f to %f \n",dlon, tempor);
    dlon = tempor;
    tempor = (float) ((int) (lon1 / (dlon/2.)  + lon1/fabs(lon1)* 0.5));
    tempor *= dlon/2.;
    printf("lon1, tempor %f %f \n", lon1, tempor);
    if (fabs(tempor - lon1) < 0.002) {
      printf("high changing lon1 from %f to %f\n",lon1, tempor);
      lon1 = tempor;
    }
  }
  highfield = new llgrid<float>(nx, ny, dlat, dlon, lat1, lon1);
  highmask = new llgrid<float>(nx, ny, dlat, dlon, lat1, lon1);
  highmask->set(0.);
  printf("Passed construction of high\n"); fflush(stdout);

// Now that the grids are specified, carry out the interpolations and
//   write out the data
  finbase = argv[13];
  foutbase = argv[15];
  splice(lowfield, lowmask, highfield, highmask, maskval, 
         wavefield, tfield, nonval, finbase, foutbase);

  finbase = argv[14];
  foutbase = argv[16];
  splice(lowfield, lowmask, highfield, highmask, maskval, 
         wavefield, tfield, nonval, finbase, foutbase);

  return 0;
}
  
int splice(llgrid<float> *lowfield, llgrid<float> *lowmask, 
         llgrid<float> *highfield, llgrid<float> *highmask, 
         float maskval, ECG_hur<float> &wavefield, ECG_hur<float> &tfield, 
         float nonval, char *finbase, char *foutbase) {
  FILE *fin, *fout;
  ijpt loc;
  char fname[800];

  printf("Entered splice\n"); fflush(stdout);
//Now begin the processing, interpolate low, then high res on to 
//  ECG grid:
  sprintf(fname, "%s.low\0",finbase) ;
  printf("fname = %s\n",fname); fflush(stdout);
  fin = fopen(fname,"r");
  if (fin == (FILE *) NULL) {
     printf("Failed to open %s\n",fname); fflush(stdout);
     return -1;
  }
  else {
     printf("Opened file %s\n", fname); fflush(stdout);
  }
  lowfield->binin(fin);
  wavefield.set(nonval);
  wavefield.fromall(*lowfield, *lowmask, maskval, nonval);
  fclose(fin);
  //CDwavefield.printer(stdout); fflush(stdout);

  printf("About to sprint the high file name \n"); fflush(stdout);
  sprintf(fname, "%s.high",finbase) ;
  printf("fname = %s\n",fname); fflush(stdout);
  fin = fopen(fname,"r");
  if (fin == (FILE *) NULL) {
     printf("Failed to open %s\n",fname); fflush(stdout);
     return -1;
  }
  else {
     printf("Opened %s\n", fname); fflush(stdout);
  }
  highfield->binin(fin);
  fclose(fin);
  tfield.set(nonval);
  tfield.fromall(*highfield, *highmask, maskval, nonval);
  printf("Returned from fromall\n"); fflush(stdout);

//For those points where the high res interpolation has data, use it.
  for (loc.j = 0; loc.j < wavefield.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < wavefield.xpoints() ; loc.i++) {
     if (tfield[loc] != nonval) {
       printf("update %3d %3d %f\n", loc.i, loc.j, tfield[loc]); fflush(stdout);
       wavefield[loc] = tfield[loc];
     }
  }
  }

// Now put out the data:
// Open in append mode so that outside may decide to put multiple 
//   interpolation to the same file. 
  fout = fopen(foutbase, "a");  
  if (fout == (FILE *) NULL) {
    printf("Failed to open the output file %s\n",foutbase);
    return -1;
  }
// This should be changed to binout for the final version.
  wavefield.printer(fout);

  return 0;
}
