#include "stdlib.h"
#include "ncepgrids.h"

#define MINUTES 8
int main(int argc, char *argv[]) {
  ramp_low<unsigned short int> tmp;
  ramp_low<float> aatopo, mask;
  llgrid<float> dest((360*60)/MINUTES+1, ((-60- -90)*60)/MINUTES+1, 
                       MINUTES/60., MINUTES/60., -90, 0.0);
  FILE *fin, *fout;
  float flagval = -1, nonval = -99;
  ijpt loc, l2;
  latpt ll;
  palette<unsigned char> gg(19,65);

  mask.set(0);

  fin = fopen(argv[1], "r");
  tmp.binin(fin);
  fclose(fin);
  for (loc.j = 0; loc.j < aatopo.ypoints(); loc.j++) {
    l2.j = aatopo.ypoints() - 1 - loc.j;
  for (loc.i = 0; loc.i < aatopo.xpoints(); loc.i++) {
    l2.i = loc.i;
    aatopo[l2] = (float) tmp[loc];
  }
  }
  printf("grid max, min, average, rms %f %f %f %f\n",
    aatopo.gridmax(), aatopo.gridmin(), aatopo.average(), aatopo.rms() );

  dest.fromall(aatopo, mask, flagval, nonval);
  
  fijpt fij1, fij2;
  for (ll.lon = -75; ll.lon < (360-75); ll.lon += 0.25) {
    for (ll.lat = -85; ll.lat <= -60; ll.lat += 0.25) {
      fij1 = aatopo.locate(ll);
      fij2 = dest.locate(ll);
  
      if (fij2.i >= 0 && fij2.j >= 0) {
        printf("%f %f  %f %f\n",ll.lon, ll.lat, aatopo[fij1], dest[fij2]);
      }
      else {
        printf("%f %f  %f %f\n",ll.lon, ll.lat, aatopo[fij1], flagval);
      }

    }
    printf("zzz\n");
  }

  fout = fopen(argv[2], "w");
  dest.ftnout(fout);
  dest.scale();
  dest.xpm("dest.xpm",7,gg);

  aatopo.scale();
  aatopo.xpm("aaorig.xpm",7,gg);

  return 0;
}
