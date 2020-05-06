#include "ncepgrids.h"

void neighborhood(fijpt &floc, metricgrid<unsigned short int> &elevs, int range) ;

int main(int argc, char *argv[]) {
  FILE *fin;
  ramp_low<unsigned short int> elevs;
  latpt ll, test, mirny, mawson, dumont, davis;
  ijpt loc, l2;
  fijpt floc;
  mvector<unsigned short int> line(elevs.xpoints());
  
  
  mirny.lat = -66.55, mirny.lon = 93.02;
  mawson.lat = -67.60, mawson.lon = 62.88;
  dumont.lat = -66.67, dumont.lon = 140.02;
  davis.lat =  -68.58, davis.lon = 77.98;
  test.lat = -69.70516, test.lon = -0.1286;

  fin = fopen(argv[1], "r");
// Need to flip the lines:
  for (loc.j = 0; loc.j < elevs.ypoints(); loc.j++) {
    l2.j = elevs.ypoints() - 1 - loc.j;
    line.binin(fin);
    for (l2.i = 0; l2.i < elevs.xpoints(); l2.i++) {
      elevs[l2] = line[l2.i];
    } 
  }
  //elevs.binin(fin);
  fclose(fin);

  floc = elevs.locate(test);
  printf("test  %f %f  %f %f  %d\n",floc.i, floc.j, test.lat, test.lon, elevs[floc]);

// Spiral inward for first nonzero elevation
  bool done = false;
  for (ll.lat = -60.0; ll.lat > -65.0; ll.lat -= 0.01) {
  for (ll.lon = 0; ll.lon < 360.; ll.lon += 0.01) {
    floc = elevs.locate(ll);
    if (elevs[floc] > 0) {
      //if (ll.lon > 180) ll.lon -= 360.;
      printf("first nonzero at %f %f, val = %d\n",ll.lat, ll.lon, elevs[floc]);
      neighborhood(floc, elevs, 5);
      done = true;
      break;
    }
    if (done) break;
  }
    if (done) break;
  }

// Spiral out for first zero elevations:
  done = false;
  for (ll.lat = -85.0; ll.lat < -75.0; ll.lat += 0.01) {
  for (ll.lon = 0; ll.lon < 360.; ll.lon += 0.01) {
    floc = elevs.locate(ll);
    if (elevs[floc] ==  0) {
      //if (ll.lon > 180) ll.lon -= 360.;
      printf("first zero at %f %f, val = %d\n",ll.lat, ll.lon, elevs[floc]);
      neighborhood(floc, elevs, 5);
      done = true;
      break;
    }
    if (done) break;
  }
    if (done) break;
  }

  //return 0;

  floc = elevs.locate(mirny);
  printf("mirny  %f %f  %f %f  %d\n",floc.i, floc.j, mirny.lat, mirny.lon, elevs[floc]);
  neighborhood(floc, elevs, 3);
  floc = elevs.locate(mawson);
  printf("mawson %f %f  %f %f  %d\n",floc.i, floc.j, mawson.lat, mawson.lon, elevs[floc]);
  neighborhood(floc, elevs, 3);
  floc = elevs.locate(dumont);
  printf("dumont %f %f  %f %f  %d\n",floc.i, floc.j, dumont.lat, dumont.lon, elevs[floc]);
  neighborhood(floc, elevs, 3);
  floc = elevs.locate(davis);
  printf("davis  %f %f  %f %f  %d\n",floc.i, floc.j, davis.lat, davis.lon, elevs[floc]);
  neighborhood(floc, elevs, 3);

//  printf("grid max, min, average, rms %d %d %d %d\n",
//    elevs.gridmax(), elevs.gridmin(), elevs.average(), elevs.rms() );

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
