#include "ncepgrids.h"

// Read in information from a gaussian grid,
//    interpolate it to a 1 degree lat-long in
//    NCEP convention, and average all to a 
//    single output file, written in fortran form
// This is done to hide from the sea ice drift program
//   the variable resolution of the GFS native grid,
//   versus the 1 degree of the ensembles.
// 14 March 2007
// Modify/update: quarter degree output, more consistent with
//   intended drift model resolution and GFS T574 resolution.
// 3 November 2011
// New variant for working with ensemble output 25 July 2013

void stats(gfs_quarter<float> &x) ;
void stats(mrf1deg<float> &x) ;
void flip(mrf1deg<float> &x) ;

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
// Input grid:
  mrf1deg<float> tmp;
// Output grid:
  gfs_quarter<float> otmp, avg;
  int i, t;
  float nonval = -9.e20;
  int navg = 2;

  fin = fopen(argv[2],"r");

// For ensembles, grid is fixed through time, all times are in same file
  fout = fopen(argv[1],"a");
  for (t = 0; t < 64/navg ; t++) {
    avg.set((float) 0.0);
    for (i = 0; i < navg; i++) {
      tmp.ftnin(fin);
      flip(tmp); // Flip grid because of wgrib2's inversion

      printf("%2d %2d raw ",t,i); stats(tmp);
    
      otmp.fromall(tmp, nonval, nonval);
      avg += otmp;
    } // end of looping over arguments
    avg /= (float) (navg);

    stats(avg);
    avg.ftnout(fout);
  }

  fclose(fout);
  fclose(fin);

  return 0;
}
void stats(gfs_quarter<float> &x) { 
    gfs_quarter<float> x2;
    double sum = 0., sum2 = 0., area = 0.;
    ijpt loc;

    x2 = x;
    x2 *= x;
    for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
      sum += x.cellarea(loc)*x[loc];
      sum2 += x.cellarea(loc)*x2[loc];
      area += x.cellarea(loc);
    }
    }
    
    printf("winds max, min, average, rms: %6.2f %6.2f %6.3f %5.2f\n",x.gridmax(), x.gridmin(),
            sum / area, sqrt(sum2/area) );
    fflush(stdout);

    return;
}
void stats(mrf1deg<float> &x) { 
    mrf1deg<float> x2;
    double sum = 0., sum2 = 0., area = 0.;
    ijpt loc;

    x2 = x;
    x2 *= x;
    for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
      sum += x.cellarea(loc)*x[loc];
      sum2 += x.cellarea(loc)*x2[loc];
      area += x.cellarea(loc);
    }
    }
    
    printf("winds max, min, average, rms: %6.2f %6.2f %6.3f %5.2f\n",x.gridmax(), x.gridmin(),
            sum / area, sqrt(sum2/area) );
    fflush(stdout);

    return;
}
void flip(mrf1deg<float> &x) {
  mrf1deg<float> y;
  ijpt loc, tloc;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
    tloc.j = x.ypoints() - 1 - loc.j;
    for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
      tloc.i = loc.i;
      y[tloc] = x[loc];
    }
  }
  x = y;
}
