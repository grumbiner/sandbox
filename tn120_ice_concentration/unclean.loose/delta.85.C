#include <stdio.h>
#include <math.h>

#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  GRIDTYPE<unsigned char> ref, algorithm;
  GRIDTYPE<int> delta;
  FILE *fin, *fout;

  float sum = 0.0, sumsq = 0.0, var;
  ijpt loc;
  int count = 0;
  
  fin = fopen(argv[1], "r");
  ref.binin(fin);
  fclose(fin);
  fin = fopen(argv[2], "r");
  algorithm.binin(fin);
  fclose(fin);

  for (loc.j = 0; loc.j < ref.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ref.xpoints(); loc.i++) {
     delta[loc] = (int) ref[loc];
     delta[loc] -= (int) algorithm[loc];
     if ( (ref[loc] > 14 && ref[loc] <= 100) || 
          (algorithm[loc] > 14 && algorithm[loc] <= 100) ) {
       sum += (float) delta[loc];
       sumsq += (float) delta[loc]*delta[loc];
       count += 1;
     }
  }
  }

  var = (sumsq - count*(sum/count)*(sum/count) ) / count;

  printf("Average %f, rms %f var %f sd %f\n", sum/count, sqrt(sumsq/count), 
            var, sqrt(var) );

  return 0;
} 
  
