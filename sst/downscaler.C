#include "ncepgrids.h"

void downscale(global_12th<float> &sstin, global_12th<float> &sstout, 
               global_12th<unsigned char> &land, int n) ;

int main(int argc, char *argv[]) {
  global_12th<unsigned char> land;
  global_12th<float> sstin, sstout;
  FILE *fin;
  int n, count = 0;
  float toler = 0.5;
  ijpt loc;
  latpt ll;
  
  fin = fopen(argv[1], "r");
  land.binin(fin);
  fclose(fin);

  fin = fopen(argv[2], "r");
  sstin.binin(fin);
  fclose(fin);

// to be sbr
  n = atoi(argv[3]);
  downscale(sstin, sstout, land, n);
  for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
    if (land[loc] >= 100) { 
      sstout[loc] = 0;
      sstin[loc]  = 0;
    }
    //if (fabs(sstout[loc] - sstin[loc]) > toler && land[loc] < 100) {
    if (fabs(sstout[loc] - sstin[loc]) > toler ) {
      ll = sstin.locate(loc);
      printf("large delta %d %d  %6.2f %6.2f  %6.2f %6.2f  %6.2f\n",
              loc.i, loc.j, ll.lat, ll.lon,
              sstin[loc], sstout[loc], sstout[loc] - sstin[loc]);
      count += 1;
    }
  }
  }
  sstout -= sstin;
  printf("n = %d count = %d  delta max, min, average, rms %f %f %f %f\n",n, count,
      sstout.gridmax(), sstout.gridmin(), sstout.average(), sstout.rms() );

  
  return 0;
}
void downscale(global_12th<float> &sstin, global_12th<float> &sstout, 
               global_12th<unsigned char> &land, int n) {
  grid2<float> x(sstin.xpoints()/n, sstin.ypoints()/n); 
  grid2<int> count(x.xpoints(), x.ypoints() );
  ijpt loc, tloc, delta;
  int i, j;

  x.set((float) 0.0);
  count.set(0);

// Count and sum temperatures onto the target grid
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    for (j = 0; j < n; j++) {
    for (i = 0; i < n; i++) {
      tloc.j = n*loc.j + j;
      tloc.i = n*loc.i + i;
      if (land[tloc] < 100) {
        count[loc] += 1;
        x[loc]     += sstin[tloc];
      }
    }
    }
  }
  }
// Run over the target grid and if there's valid info on that, and the point si
//   not land on original, put target's average onto the output grid
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    for (j = 0; j < n; j++) {
    for (i = 0; i < n; i++) {
      tloc.j = loc.j*n + j;
      tloc.i = loc.i*n + i;
      if (land[tloc] < 100 && count[loc] != 0) {
        sstout[tloc] = x[loc] / count[loc];
      }
    }
    } 
  }
  }
    
  return;
}
