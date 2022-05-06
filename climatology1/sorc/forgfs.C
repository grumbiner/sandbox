#include "ncepgrids.h"

// Program to read in climatology for the day and write out
//   an estimated sea ice field.
// Use count >= 15, then condavg for concentration field
// Write out both 30' and 5' fields.
//
#define count 0
#define avg   1
#define sumsq 2
#define var   3
#define CONDAVG  4
#define CONSUMSQ 5
#define CONDVAR  6

int main(int argc, char *argv[]) {
  global_ice<float> climo[7];
  global_ice<float> low;
  global_12th<float> high;
  FILE *fin, *fout;
  ijpt loc;
  float landval = 1.57, nonval = 1.57;
  int nreq;

  fin = fopen(argv[1],"r");
  for (int i = 0; i < 7; i++) {
    climo[i].binin(fin);
  }
  fclose(fin);
  nreq = climo[count].gridmax() / 2;

  low = climo[avg];
#ifdef CONDITIONAL
  low = climo[CONDAVG];
  for (loc.j = 0; loc.j < low.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < low.xpoints(); loc.i++) {
    if(climo[count][loc] < nreq) low[loc] = 0;
  }
  }
#endif

  high.fromall(low, landval, nonval); 

  fout = fopen(argv[2],"w");
  low.binout(fout);
  fclose(fout);

  fout = fopen(argv[3],"w");
  high.binout(fout);
  fclose(fout);

  return 0;
}
