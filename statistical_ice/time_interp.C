#include <stdio.h>
#include <stdlib.h>

#include "ncepgrids.h"
//Program to interpolate linearly between grids from two separate days.
//If one of the days has no data for a point, then use the other day.
//If neither day has data, leave blank.  Start with the global grids.
//Robert Grumbine 19 November 1998
//Modified: -DGRIDTYPE= expected in the compile step
//28 June 1999
// 8 November 2000: -DDTYPE= expected in compile step

#define MAXGAP   10
#define NO_DATA 2.24
#define MAXICE  1.27
#define MINICE   .15

int main(int argc, char *argv[]) {
  GRIDTYPE<DTYPE> first, last, test;
  GRIDTYPE<DTYPE> *interp[MAXGAP];

  FILE *fin1, *fin2, *fout[MAXGAP];
  int i, length, index;
  ijpt x;
  char fname[256];

//////// Process inputs
  fin1 = fopen(argv[1], "r");
  fin2 = fopen(argv[2], "r");
  length = atoi(argv[3]);

  if (fin1 == (FILE *) NULL ) {
    printf("Failed to open the first data file\n");
    return 1;
  }
  if (fin2 == (FILE *) NULL ) {
    printf("Failed to open the second data file\n");
    return 1;
  }
  if (length >= MAXGAP) {
    printf("Gap is too large! %d is limit, %d is request\n",MAXGAP, length);
    return 3;
  }

///// Start up:
  first.binin(fin1);
  last.binin(fin2);

  #ifdef VERBOSE
    printf("Have read in the data, now preparing to create the new grids\n");
    fflush(stdout);
  #endif 
  for (i = 0; i < length; i++) {
    interp[i] = new GRIDTYPE<DTYPE>;
    sprintf(fname,"out.%02d",i);
    fout[i] = fopen(fname, "w");
    if (fout[i] == (FILE *) NULL) {
      printf("Failed to open %s\n",fout[i]);
    }
  } 
  #ifdef VERBOSE
    printf("Have created the new grids, now trying to interpolate\n");
    fflush(stdout);
  #endif 
// Interpolate
  for (x.j = 0; x.j < first.ypoints() ; x.j++) {
  for (x.i = 0; x.i < first.xpoints() ; x.i++) {
    index = x.i + x.j*first.xpoints();
      if (first[x] > MAXICE && last[x] > MAXICE ) {
        for (i = 0; i < length; i++) {
           interp[i]->operator[](x) = NO_DATA;
        }
      }
      else if (first[x] > MAXICE  && last[x] <= MAXICE) {
        for (i = 0; i < length; i++) {
           interp[i]->operator[](x) = last[x];
        }
      }
      else if (first[x] <= MAXICE && last[x] > MAXICE ) {
        for (i = 0; i < length; i++) {
           interp[i]->operator[](x) = first[x];
        }
      }
      else {  // valid data on both days
        for (i = 0; i < length; i++) {
           interp[i]->operator[](x) = (DTYPE) (0.5 + first[x] + 
                 (float)(i+1)/(float)(length+1) *
                (  (float)last[x] - (float)first[x] )   ) ;
        }
      }
//    Clean up 
      for (i = 0; i < length; i++) {
        if (interp[i]->operator[](x) < MINICE) interp[i]->operator[](x) = 0;
        if (interp[i]->operator[](x) > MAXICE) interp[i]->operator[](x) = 0;
      }
      #ifdef VERBOSE
        i = 0;
        printf("%3d %3d  %3d %3d %3d\n",x.i, x.j, first[x], last[x], interp[i]->operator[](x) );
      #endif 

   } //x.i
   } //x.j
       
/// Now should have full data sets for missing days,
   for (i = 0; i < length; i++) {
    test = *interp[i];
    test.binout(fout[i]);
    fclose(fout[i]);
    printf("step %d of %d max min %d %d  first %d %d last %d %d\n",i, length, 
              test.gridmax(), test.gridmin(),
              first.gridmax(), first.gridmin(),
              last.gridmax(), last.gridmin()  
           );
   }

  return 0;

}
