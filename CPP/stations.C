#include "ncepgrids.h"

void neighborhood(fijpt &floc, metricgrid<unsigned short int> &elevs, int range) ;

int main(int argc, char *argv[]) {
  FILE *fin;
  ramp_low<unsigned short int> elevs;
  latpt ll, test, mirny, mawson, dumont, davis;
  ijpt loc, l2;
  fijpt floc;
  mvector<unsigned short int> line(elevs.xpoints());
  
  fin = fopen("1kmflip", "r");
// Need to flip the lines:
  for (loc.j = 0; loc.j < elevs.ypoints(); loc.j++) {
    l2.j = elevs.ypoints() - 1 - loc.j;
    line.binin(fin);
    for (l2.i = 0; l2.i < elevs.xpoints(); l2.i++) {
      elevs[l2] = line[l2.i];
    } 
  }
  fclose(fin);

  test.lat = atof(argv[1]);
  test.lon = atof(argv[2]);
  floc = elevs.locate(test);
  printf("test  %f %f  %f %f  %d\n",floc.i, floc.j, test.lat, test.lon, elevs[floc]);
  neighborhood(floc, elevs, 5);
      
  return 0;
}
void neighborhood(fijpt &floc, metricgrid<unsigned short int> &elevs, int range) {
  ijpt center, loc;
  latpt ll;
  center = floc;
  for (loc.j = center.j - range; loc.j <= center.j + range; loc.j++) {
  for (loc.i = center.i - range; loc.i <= center.i + range; loc.i++) {
    ll = elevs.locate(loc);
    printf("  %5d %5d  %f %f  %5d\n",loc.i, loc.j, ll.lat, ll.lon, elevs[loc]);
  }
  }
  return;
} 
