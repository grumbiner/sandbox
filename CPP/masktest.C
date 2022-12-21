#include <stdio.h>
#include "ncepgrids.h"
#include "resops.h"
#include "legacy.h"

int main(int argc, char *argv[]) {
  cofstype<float> x;
  global_ice<float> sst, sstmask;
 
  FILE *fin, *fout;
  ijpt l, loc;
  float landval = -199., flagval = -99., sstmax, sstmin;

  // These lines open the input and output files, and check for success
  fin  = fopen(argv[1],"r");
  fout = fopen(argv[2],"w");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the input file\n");
    return 1;
  }
  if (fout == (FILE *) NULL) {
    printf("Failed to open the output file\n");
    return 2;
  }

  sstmask.set(0.); 
// // Now loop through and put flagvalues on every 2 points
//   for (loc.j = 0; loc.j < sstmask.ypoints() ; loc.j += 2) {
//   for (loc.i = 0; loc.i < sstmask.xpoints() ; loc.i += 1) {
//      sstmask[loc] = landval;
//   }
//   }

  
  // Read in the data file:
  sst.binin(fin);
  sstmax = sst.gridmax();
  sstmin = sst.gridmin();
  printf("max, min of input sst field: %f %f\n",sstmax, sstmin);
// Loop through and put in flagvalues every 3 point
   for (loc.j = 0; loc.j < sstmask.ypoints() ; loc.j += 3) {
   for (loc.i = 0; loc.i < sstmask.xpoints() ; loc.i += 3) {
      sst[loc] = flagval;
   }
   }

  // Interpolate onto the native DC grid:
  x.fromall(sst, sstmask, landval, flagval);
  printf("max, min of output sst field: %f %f\n",x.gridmax(), x.gridmin());
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x[loc] < sstmin && x[loc] != flagval) {
      printf("%d %d %f\n",loc.i, loc.j, x[loc]);
    }
  }
  }

  // Write out (unformatted unblocked binary) native field:
  x.binout(fout);


  // close the input/output files, not strictly necessary:
  fclose(fin); 
  fclose(fout);

  return 0;
}
