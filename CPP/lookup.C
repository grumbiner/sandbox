#include "ncepgrids.h"

void neighborhood(fijpt &floc, metricgrid<unsigned short int> &elevs, int range) ;

int main(int argc, char *argv[]) {
  FILE *fin;
  ramp_low<unsigned short int> elevs;
  latpt ll, mirny, mawson, dumont, davis;
  ijpt loc;
  fijpt floc;
  
  printf("started program\n"); fflush(stdout);
  loc.j = 0; loc.i = 0;
  ll = elevs.locate(loc);
  printf("%d %d  %f %f \n",loc.i, loc.j, ll.lat, ll.lon);
  printf("slat = %f\n",elevs.slat);
  
  mirny.lat = -66.55, mirny.lon = 93.02;
  mawson.lat = -67.60, mawson.lon = 62.88;
  dumont.lat = -66.67, dumont.lon = 140.02;
  davis.lat =  -68.58, davis.lon = 77.98;

  fin = fopen(argv[1], "r");
  elevs.binin(fin);
  fclose(fin);

  floc = elevs.locate(mirny);
  printf("mirny  %f %f  %f %f  %d\n",floc.i, floc.j, mirny.lat, mirny.lon, elevs[floc]);
  neighborhood(floc, elevs, 5);
  floc = elevs.locate(mawson);
  printf("mawson %f %f  %f %f  %d\n",floc.i, floc.j, mawson.lat, mawson.lon, elevs[floc]);
  neighborhood(floc, elevs, 5);
  floc = elevs.locate(dumont);
  printf("dumont %f %f  %f %f  %d\n",floc.i, floc.j, dumont.lat, dumont.lon, elevs[floc]);
  neighborhood(floc, elevs, 5);
  floc = elevs.locate(davis);
  printf("davis  %f %f  %f %f  %d\n",floc.i, floc.j, davis.lat, davis.lon, elevs[floc]);
  neighborhood(floc, elevs, 5);

  printf("grid max, min, average, rms %d %d %d %d\n",
    elevs.gridmax(), elevs.gridmin(), elevs.average(), elevs.rms() );

  return 0;
}
void neighborhood(fijpt &floc, metricgrid<unsigned short int> &elevs, int range) {
  ijpt center, loc;
  center = floc;
  for (loc.j = center.j - range; loc.j <= center.j + range; loc.j++) {
  for (loc.i = center.i - range; loc.i <= center.i + range; loc.i++) {
    printf("  %5d %5d %5d\n",loc.i, loc.j, elevs[loc]);
  }
  }
  return;
} 
