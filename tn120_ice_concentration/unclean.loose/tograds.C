#include "ncepgrids.h"

// Program to interpolate from sea ice grid on to a corresponding
//   nominal resolution latitude-longitude grid.  Purpose is to
//   ensure that grads (et al.) have something that they can work
//   with intelligently.

int main(int argc, char *argv[]) {
  GRIDIN<unsigned char> x;
  GRIDIN<float> xconv, landref;
//Define these as pointers because we'll define the projection parameters
//  later, as we determine the appropriate equivalent resolution.
  //llgrid<float> *y;
  //llgrid<unsigned char> *yout;
  global_high<float> y;
  global_high<unsigned char> yout;
  FILE *fin, *fout;
  float landmask = 157., nonval = 998.;
  float dlat, dlon, firstlat;
  int nx, ny;
  ijpt loc;
  latpt ll;

  fin = fopen(argv[1], "r");
  fout = fopen(argv[2], "w");
  if (fin == (FILE *) NULL || fout == (FILE *) NULL) {
    printf("Failed to open required file\n");
    return 1;
  }
  
  x.binin(fin);
  fclose(fin);
  conv(x, xconv);
  printf("max, min, avg on input %f %f %f\n",(float) x.gridmax(), (float) x.gridmin(), (float) x.average() );
  printf("max, min, avg on input %f %f %f\n",xconv.gridmax(), xconv.gridmin(), xconv.average() );
  landref.set(999.);

// Establish the correct parameters for the latitude-longitude grid:
  dlat = 1./8.;
  dlon = 1./8.; 
  nx = 360. / dlon;
  ny = 90. / dlat;
  printf("nx, ny %d %d\n",nx, ny);
  ll.lat = 90.0;
  ll.lon = 0.0;
  if ( x.in(x.locate(ll)) ) {
    firstlat = 90. - dlat /2.;
  }
  else {
    firstlat = -90. + dlat/2.;
  }
  printf("firstlat = %f\n",firstlat);

  //y    = new llgrid<float>         (nx, ny, dlat, dlon, firstlat, dlon / 2.);
  //yout = new llgrid<unsigned char> (nx, ny, dlat, dlon, firstlat, dlon / 2.);

  y.fromall(xconv, landref, landmask, nonval);

  printf("max, min %f %f %f\n",y.gridmax(), y.gridmin(), y.average() );
  for (loc.j = 0; loc.j < y.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < y.xpoints(); loc.i++) {
    //if (y[loc] != nonval && y[loc] != 0) {
    //  printf("i, j, val %d %d %f\n",loc.i, loc.j, 0.5 + y[loc] );
   // }
    yout[loc] = (unsigned char) (0.5 + y[loc] );
  }
  }

  printf("max, min %f %f\n",(float) yout.gridmax(), (float) yout.gridmin() );
  yout.binout(fout);

  return 0;
}
