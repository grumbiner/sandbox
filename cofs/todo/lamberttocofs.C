// Program to interpolate from the ETA 32 km lambert conformal
//   grid to the COFS native grid.
// Robert Grumbine 6 December 2000
//
// Program History:
//   2000-12-06 Robert Grumbine  -- Original Implementation
// 
// Usage:
//   lambertcofs input_file_name output_file_name
//   files cfs.bini and cfs.binj must be in the current working directory
// 
// Attributes:
//   Language: C++
//   Machine:  ASP (for operations, also tested on sgi100 and penguin (linux) 


#include <stdlib.h>

#include "ncepgrids.h"
#include "cofs.h"

int main(int argc, char *argv[]) {
  cfslevel<float> x;
  lambert<float> ref, refmask;
  ijpt loc;
  fijpt floc;
  latpt ll;
  float maskval = -900., nonval = -999., toler;
  int errcount = 0, count = 0;
  FILE *fin, *fout;

  if (argc < 3) { 
    printf("Usage is lambertcofs input_file_name output_file_name\n");
    return 1;
  }
  else {
    fin = fopen(argv[1], "r");
    if (fin == (FILE *) NULL) {
      printf("Failed to open input file %s\n",argv[1]);
      return 2;
    }
    fout = fopen(argv[2], "w");
    if (fout == (FILE *) NULL) {
      printf("Failed to open output file %s\n",argv[2]);
      return 3;
    }
  }

// Translate from the lambert grid to the cofs grid.  No masking on the
//   lambert grid.
  refmask.set(0.0);
  do {
    ref.binin(fin);
    if (!feof(fin)) {
      x.fromall(ref, refmask, maskval, nonval);
      x.binout(fout);
    }
  } while (!feof(fin) );

  return 0;
}
