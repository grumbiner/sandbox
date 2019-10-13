#include "ncepgrids.h"

// Construct a traditional-style climate average -- average of N days
//    flip grid to my convention for later use
// Extended 23 December 2011 to also compute slope, intercept, correl

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  // -999 is reynolds flag value for no info
  int x[4]; // extra is for the ftn unformatted header
  global_quarter<short int> day;
  global_quarter<float> average;
  global_quarter<double> sx, sy, sx2, sxy, sy2;
  global_quarter<float> a, b, r;
// y = a + bx
  int npts;

  ijpt loc, tloc;
  int i;

  average.set((float) 0.0);

  sx.set((double) 0.0);
  sy.set((double) 0.0);
  sxy.set((double) 0.0);
  sx2.set((double) 0.0);
  sy2.set((double) 0.0);

  a.set((float) 0.0);
  b.set((float) 0.0);
  r.set((float) 0.0);

  npts = argc - 2;
  for (i = 2; i < argc; i++) {
    fin  = fopen(argv[i], "r");
    fread(&x, sizeof(int), 4, fin);
    day.binin(fin);
    fclose(fin);

    sx  += (i-2);
    sx2 += (i-2)*(i-2);

    // flip in to my convention:
    for (loc.j = 0; loc.j < day.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < day.xpoints(); loc.i++) {
      tloc.j = day.ypoints() - loc.j - 1;
      tloc.i = loc.i;
      average[tloc] += day[loc];

// rescale to degrees:
      sy[tloc]  += day[loc]/100.;
      sy2[tloc] += day[loc]/100.*day[loc]/100.;
      sxy[tloc] += day[loc]/100. * (i-2);
    }
    }
  }
  
 
  average /= (float) npts;
  sx      /= (float) npts;
  sy      /= (float) npts;

  latpt ll;
  for (loc.j = 0; loc.j < sx.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < sx.xpoints(); loc.i++) {

    if (sy[loc] > -3.0) {
      b[loc] = (sxy[loc] - npts*sx[loc]*sy[loc]) / 
               (sx2[loc] - npts*sx[loc]*sx[loc]);       // slope
      a[loc] = (sy[loc] - b[loc]*sx[loc]);
      r[loc] = (sxy[loc] - npts*sx[loc]*sy[loc]) / 
                   sqrt(sx2[loc] - npts*sx[loc]*sx[loc]) /  
                   sqrt(sy2[loc] - npts*sy[loc]*sy[loc]) ;

      //ll = a.locate(loc);
      //printf("%7.2f %7.2f  %6.2f  %8.5f %7.3f  %6.3f\n",ll.lon, ll.lat, 
      //       sy[loc], b[loc], a[loc], r[loc]);
    }

  }
  }

  fout = fopen(argv[1], "w");
  average.binout(fout);
  b.binout(fout);
  a.binout(fout);
  r.binout(fout);
  fclose(fout);

  printf("avg   max, min %f %f\n",average.gridmax(), average.gridmin() );
  printf("sx    max, min %f %f\n",sx.gridmax(), sx.gridmin() );
  printf("sy    max, min %f %f\n",sy.gridmax(), sy.gridmin() );
  printf("slope max, min %e %e\n",b.gridmax(), b.gridmin() );
  printf("int   max, min %e %e\n",a.gridmax(), a.gridmin() );
  printf("r     max, min %f %f\n",r.gridmax(), r.gridmin() );

  return 0;
}
