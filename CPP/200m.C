#include "ncepgrids.h"

void neighborhood(fijpt &floc, metricgrid<unsigned short int> &elevs, int range) ;

#define MINUTES 8
int main(int argc, char *argv[]) {
  FILE *fin;
  ramp_high<unsigned short int> elevs;
  latpt ll, mirny, mawson, dumont, davis, molod, dovers, vostok;
  ijpt loc, l2;
  fijpt floc;
  mvector<unsigned short int> line(elevs.xpoints());
  llgrid<float> dest((360*60)/MINUTES+1, ((-60- -90)*60)/MINUTES+1,
                       MINUTES/60., MINUTES/60., -90, 0.0);
  float landval=-99.0, nonval=-99.0;
  
  printf("initialized program ll nx ny = %d %d\n", dest.xpoints(), dest.ypoints() ); 
  fflush(stdout);
  
  mirny.lat  = -66.55; mirny.lon  =  93.02;
  mawson.lat = -67.60; mawson.lon =  62.88;
  dumont.lat = -66.67; dumont.lon = 140.02;
  davis.lat  = -68.58; davis.lon  =  77.98;
  molod.lat  = -67.67; molod.lon  =  45.85;
  dovers.lat = -70.25; dovers.lon =  65.87;
  vostok.lat = -78.45; vostok.lon = 106.87;

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

// Spiral inward for first nonzero elevation
  bool done = false;
  for (ll.lat = -60.0; ll.lat > -62.0; ll.lat -= 0.001) {
  for (ll.lon = 0; ll.lon < 360.; ll.lon += 0.001) {
    floc = elevs.locate(ll);
    if (elevs[floc] > 0) {
      printf("first nonzero at %f %f, val = %d\n",ll.lat, ll.lon, elevs[floc]);
      neighborhood(floc, elevs, 15);
      done = true;
      break;
    }
    if (done) break;
  }
    if (done) break;
  }

// Spiral out for first zero elevations:
  done = false;
  for (ll.lat = -83.0; ll.lat < -77.0; ll.lat += 0.001) {
  for (ll.lon = 0; ll.lon < 360.; ll.lon += 0.001) {
    floc = elevs.locate(ll);
    if (elevs[floc] ==  0) {
      //if (ll.lon > 180) ll.lon -= 360.;
      printf("first zero at %f %f, val = %d\n",ll.lat, ll.lon, elevs[floc]);
      neighborhood(floc, elevs, 15);
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
  neighborhood(floc, elevs, 10);
  floc = elevs.locate(mawson);
  printf("mawson %f %f  %f %f  %d\n",floc.i, floc.j, mawson.lat, mawson.lon, elevs[floc]);
  neighborhood(floc, elevs, 10);
  floc = elevs.locate(dumont);
  printf("dumont %f %f  %f %f  %d\n",floc.i, floc.j, dumont.lat, dumont.lon, elevs[floc]);
  neighborhood(floc, elevs, 10);

  floc = elevs.locate(davis);
  printf("davis  %f %f  %f %f  %d\n",floc.i, floc.j, davis.lat, davis.lon, elevs[floc]);
  neighborhood(floc, elevs, 10);
  floc = elevs.locate(molod);
  printf("molod  %f %f  %f %f  %d\n",floc.i, floc.j, molod.lat, molod.lon, elevs[floc]);
  neighborhood(floc, elevs, 10);

  floc = elevs.locate(vostok);
  printf("vostok  %f %f  %f %f  %d\n",floc.i, floc.j, vostok.lat, vostok.lon, elevs[floc]);
  neighborhood(floc, elevs, 10);
  floc = elevs.locate(dovers);
  printf("dovers  %f %f  %f %f  %d\n",floc.i, floc.j, dovers.lat, dovers.lon, elevs[floc]);
  neighborhood(floc, elevs, 10);

  //dest.fromall(elevs, landval, nonval); 
  //FILE *fout;
  //fout = fopen("ll.ascii","w");
  //dest.printer(fout);
  //fclose(fout);

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
