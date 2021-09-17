#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  OUTGRID<float> out; 
  INGRID<float> in, mask;
  float maskval = 9.e5, nonval = -75.;
  FILE *fin1, *fout;
  ijpt loc;
  latpt ll;

  fin1 = fopen(argv[1], "r");
  in.ftnin(fin1);
  fclose(fin1);
  printf("in max, min avg %f %f %f\n",
       in.gridmax(), in.gridmin(), in.average() );
  #ifdef VERBOSE
  for (loc.j = 0; loc.j < in.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < in.xpoints(); loc.i++) {
    ll = in.locate(loc);
    printf("%3d %3d %f %f\n",loc.i, loc.j, ll.lat, ll.lon, in[loc]);
  }
  }
  #endif

  mask.set((float) 77777.);
  out.fromall(in, mask, maskval, nonval);
  printf("out max, min avg %f %f %f\n",
       out.gridmax(), out.gridmin(), out.average() );
  if (out.gridmax() > 10000. || out.gridmin() < 0.) {
    printf("conversion failed somehow\n");
    for (loc.j = 0; loc.j < out.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < out.xpoints(); loc.i++) {
      if (out[loc] > 10000. || out[loc] < 0.) {
        ll = out.locate(loc);
        printf("%3d %3d %f %f  %f\n",loc.i, loc.j, ll.lat, ll.lon, out[loc]);
        // Try near neighbor average for the point:
        out[loc] = out[loc.i-1 + loc.j*out.xpoints() ];
      }
    }
    }
  }
  
  fout = fopen(argv[2], "w");
  out.ftnout(fout);
  fclose(fout);

  return 0;
}
