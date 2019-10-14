#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  global_ice<float> sst, sum, sumsq, tsq, var;
  float maxval, tmax, tcrit = 307.0, varcrit = 36.0;
  ijpt loc;
  latpt ll;

  maxval = -5;
  sum.set((float) 0.);
  sumsq.set((float) 0.);

  fin = fopen(argv[1], "r");
  for (int i = 0; i < 366; i++) {
    sst.binin(fin);
    sst -= 273.15;
    tmax = sst.gridmax();
    printf("day %d max %f\n",i,tmax);
    maxval  = max(maxval, tmax );

    sum += sst;
    tsq = sst; tsq *= sst;
    sumsq += tsq;
    
    if (tmax > tcrit) {
      for (loc.j = 0; loc.j < sst.ypoints(); loc.j++) {
      for (loc.i = 0; loc.i < sst.xpoints(); loc.i++) {
        if (sst[loc] > tcrit) {
          ll = sst.locate(loc);
          printf("day %d loc %f %f val %f\n",i+1, ll.lat, ll.lon, sst[loc]);
        }
      }
      }
    }

  }
  for (loc.j = 0; loc.j < sst.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < sst.xpoints(); loc.i++) {
    var[loc] = (sumsq[loc] - sum[loc]*sum[loc]/366.)/366.;
    //if (var[loc] < 0) printf("negative %4d %4d  %f %f %f\n",loc.i, loc.j,
    //     sumsq[loc], sum[loc], var[loc]);
    if (var[loc] > varcrit) {
      ll = var.locate(loc);
       printf("%6.2f %6.2f  %f %f %f\n",ll.lat, ll.lon,
         sumsq[loc], sum[loc], var[loc]);
    }
      
  }
  }
  printf("var max, min, average %f %f %f\n",var.gridmax(), var.gridmin(), var.average() );


  fclose(fin);

  printf("\n max of year %f\n",maxval);

  return 0;
}
