#include "ncepgrids.h"
#include "time_series.h"
#include <errno.h>
#include <stack>
using namespace std;

#define MAXPTS  54221
#define TIME    10957

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  mvector<float> avg(MAXPTS), count(MAXPTS), sd(MAXPTS);
  mvector<int> index(MAXPTS);
  mvector<ijpt> loc(MAXPTS);
  mvector<latpt> ll(MAXPTS);

  int i, j, k = 0, ti, tj;
  ijpt tloc;
  latpt tll;


// Scan the summary file for point locations
  int countin, indexin;
  float ain, sdin;
  float lat, lon, d1, f1, f2, f3;
  fin = fopen("allice.summary","r");
  if (fin == (FILE*) NULL) {
    printf("failed to open allice.summary\n");
    return 1;
  }
  for (i = 0; i < MAXPTS; i++) {
    fscanf(fin, "%d %d %d %f %f %d %f %f %f %f %f %f\n",&indexin, &ti, &tj, &lat, &lon, &countin, &ain, &d1, &sdin, &f1, &f2, &f3);
    index[i] = indexin;
    tloc.i = ti; tloc.j = tj;
    loc[i] = tloc;
    tll.lat = lat; tll.lon = lon;
    ll[i]   = tll;
    count[i] = (float) countin;
    avg[i] = ain;
    sd[i]  = sqrt(sdin);
  }
  fclose(fin);

/////////////////////////////////////////////////////////////////////
// Now scan the input data file(s) and read in to memory the N data, as vectors
  mvector<float> trans[MAXPTS];
  global_ice<float> ice;

  printf("about to resize the vectors\n"); fflush(stdout);
  for (i = 0; i < MAXPTS; i++) {
    trans[i].resize(TIME);
  }
  printf("done resizing the vectors\n"); fflush(stdout);
// note that the vectors could be written out to separate files.
  for (i = 1; i <= TIME; i++) {
    fin = fopen(argv[i],"r");
    ice.binin(fin); 
    fclose(fin);
    if ( (i-1)% 30 == 0) {
      printf("read in %s\n",argv[i]); fflush(stdout);
    }
    for (j = 0; j < MAXPTS; j++) {
      trans[j ] [i-1] = ice[loc[j] ];
    }
  }
  printf("done setting up the vectors\n"); fflush(stdout);

  char fname[90];
  for (i = 0; i < MAXPTS; i++) { 
    printf("trans i %d average %f\n", i, trans[i].average() ); fflush(stdout);
    sprintf(fname, "transposed_data.%05d",i);
    fout = fopen(fname, "w");
    trans[i].binout(fout);
    fclose(fout);
  }

  return 0;
}
