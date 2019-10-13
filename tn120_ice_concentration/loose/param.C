#include <stdio.h>

#include "ncepgrids.h"
#define GR37LIM 0.05
#define GR22LIM 0.045

typedef struct {
  short int t19v, t19h, t22v, t37v, t37h;
} nsidcpt;

int main(int argc, char *argv[]) {
  FILE *fin1, *fin2, *fout;
  nsidcpt nsidcread[448][304];
  nsidcnorth<float> t37v, t37h, t22v, t19v, t19h;
  nsidcnorth<float> param;
  ijpt loc;
  latpt ll;
  int i, count = 0;
  float gr22, gr37;

  fin1 = fopen(argv[1], "r");
  fout = fopen(argv[2], "w");
  if (fin1 == (FILE*)NULL) {
    printf("Failed to open %s\n",argv[1]);
    return 1;
  }
  if (fout == (FILE*)NULL) {
    printf("Failed to open %s\n",argv[2]);
    return 1;
  }

  i = fread(&nsidcread[0][0], sizeof(nsidcpt), 448*304, fin1);
  if (i != 448*304) {
    printf("only read in %d of %d\n",i,448*304); fflush(stdout);
    exit(1);
  }
  fclose(fin1);
  for (loc.j = 0; loc.j < t37v.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < t37v.xpoints(); loc.i++) {
// Do following because there is a flip in the nsidc grids
    t22v[loc] = (float) nsidcread[t37v.ypoints() -1 -loc.j][loc.i].t22v;
    t19v[loc] = (float) nsidcread[t37v.ypoints() -1 -loc.j][loc.i].t19v;
    t19h[loc] = (float) nsidcread[t37v.ypoints() -1 -loc.j][loc.i].t19h;
    t37v[loc] = (float) nsidcread[t37v.ypoints() -1 -loc.j][loc.i].t37v;
    t37h[loc] = (float) nsidcread[t37v.ypoints() -1 -loc.j][loc.i].t37h;
  }
  }

  printf("t37v stats %f %f  %f %f\n", 
       t37v.gridmax(-1.), t37v.gridmin(-1.), t37v.average(-1.), t37v.rms(-1.) );
  printf("t37h stats %f %f  %f %f\n", 
       t37h.gridmax(-1.), t37h.gridmin(-1.), t37h.average(-1.), t37h.rms(-1.) );

  for (loc.j = 0; loc.j < param.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < param.xpoints(); loc.i++) {
    gr37 = (t37v[loc] - t19v[loc]) / (t37v[loc] + t19v[loc]);
    gr22 = (t22v[loc] - t19v[loc]) / (t22v[loc] + t19v[loc]);
    if (t37h[loc] > t37v[loc] || t37h[loc] < 1000. || t37v[loc] < 1500. 
                              || t37h[loc] > 2950. || t37v[loc] > 2950. ||
        gr22 > GR22LIM || gr37 > GR37LIM ) {
      // printf("error %d %d  %d %d\n",loc.i, loc.j, t37v[loc], t37h[loc]);
      param[loc] = -1.;
      count += 1;
    }
    else {
      param[loc] = sqrt( t37v[loc]*t37v[loc] - t37h[loc]*t37h[loc] ) / 10.;
    }

    #ifdef VERBOSE
    ll = param.locate(loc);
    if (ll.lat > 87.5) {
      printf("pole param = %d %d %4.1f %6.1f  %6.2f %6.1f %6.1f\n",
           loc.i, loc.j, ll.lat, ll.lon, 
           param[loc], t37v[loc], t37h[loc]);
    }
    #endif

  }
  } 
  printf("error count %d\n",count);

  printf("max, min, avg, rms %f %f %f %f\n",
        param.gridmax(-1.), param.gridmin(-1.), param.average(-1.), 
        param.rms(-1.) );

  param.binout(fout);
  fclose(fout);

  return 0;

}
