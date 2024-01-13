#include <stdio.h>
#include <stdlib.h>
#include "ncepgrids.h"
#include "resops.h"

void nearest_water(fijpt &ijout, GRIDTYPE<float> &x) ;
float mindist(mvector<latpt> &periphery, mvector<float> &dist, latpt &llin,
              fijpt &llout, GRIDTYPE<float> &x);
extern "C" float arcdis_(float &long1, float &lat1, float &long2, 
                           float &lat2 );

int main(int argc, char *argv[]) {
  latpt llin;
  int station, count = 0;
  fijpt ijout, llout;
  ijpt intloc, loc;
  GRIDTYPE<float> x;
  FILE *fin;
  float toler;
  mvector<latpt> periphery(x.xpoints()*2 + x.ypoints()*2 - 3);
  mvector<float> dist(x.xpoints()*2 + x.ypoints()*2 - 3);

  fin = fopen(argv[1],"r");
  toler = atof(argv[2]);  // minimum distance to be in buffer zone

// Extract the locations of the peripheral points
  count = 0;

  loc.i = 0;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
    periphery[count] = x.locate(loc);
    //printf("%d  %d %d\n",count, loc.i, loc.j);
    count += 1;
  }
  loc.j = x.ypoints() - 1;
  for (loc.i = 1; loc.i < x.xpoints(); loc.i++) {
    periphery[count] = x.locate(loc);
    //printf("%d  %d %d\n",count, loc.i, loc.j);
    count += 1;
  }
  loc.i = x.xpoints() - 1;
  for (loc.j = x.ypoints() - 2; loc.j >= 0; loc.j--) {
    periphery[count] = x.locate(loc);
    //printf("%d  %d %d\n",count, loc.i, loc.j);
    count += 1;
  }
  loc.j = 0;
  for (loc.i = x.xpoints() - 2;  loc.i >= 0; loc.i--) {
    periphery[count] = x.locate(loc);
    //printf("%d  %d %d\n",count, loc.i, loc.j);
    count += 1;
  }

//  int i;
//  for (i = 0; i < periphery.xpoints(); i++) {
//    printf("%d %f %f\n",i, periphery[i].lat, periphery[i].lon);
//  }

// Loop over the input points
  while ( !feof(fin) ) {
    fscanf(fin,"%d %f %f\n",&station, &llin.lat, &llin.lon);
    ijout = x.locate(llin);
    intloc = ijout;
    if (!x.land(intloc) && x.in(ijout) ) {
      printf("%d %5.2f %6.2f  %8.4f %8.4f water\n",station, 
               llin.lat, llin.lon, ijout.i, ijout.j);
    }
    else if (x.in(ijout) ) {
      printf("%d %5.2f %6.2f  %8.4f %8.4f land\n",station, 
               llin.lat, llin.lon, ijout.i, ijout.j);
      nearest_water(ijout, x) ;
    }
    else {
      printf("%d %5.2f %6.2f  %8.4f %8.4f off %f\n",station, 
               llin.lat, llin.lon, ijout.i, ijout.j, 
               mindist(periphery, dist, llin, llout, x) );
      if (mindist(periphery, dist, llin, llout, x) < toler) {
        nearest_water(llout, x);
      }
    }
    
  }

  return 0;

}
void nearest_water(fijpt &ijout, GRIDTYPE<float> &x) {
  ijpt tloc, refloc, delta;
  int range = 1, range_lim = 15; //range_lim is an empirical number
  bool found = false; 

  refloc = ijout;
  if (x.land(refloc)) {
    while (range < range_lim && ! found) {
      //printf("range %d ijout %f %f refloc %d %d\n",range, ijout.i, ijout.j,
      //        refloc.i, refloc.j);
      for (delta.j = -range; delta.j <= range; delta.j++) {
      for (delta.i = -range; delta.i <= range; delta.i++) {
        tloc = refloc;
        tloc += delta;
        if (!x.land(tloc)) {
          found = true;
          printf("        %6.2f %6.2f  %3d %3d  %3d %3d neighbor\n",
                       ijout.i, ijout.j, delta.i, delta.j, tloc.i, tloc.j);
        }
      }
      }
      range += 1;
    }
  }
  else {
    printf("you're already on water\n");
  }
  return;
}
float mindist(mvector<latpt> &periphery, mvector<float> &dist, latpt &llin, 
              fijpt &llout, GRIDTYPE<float> &x) {
  float least = 1.e90;
  int i;

  llout.i = -100;
  llout.j = -100;

  for (i = 0; i < periphery.xpoints(); i++) {
    dist[i] = arcdis_(llin.lon, llin.lat, periphery[i].lon, periphery[i].lat);
    if (dist[i] < least) {
      least = min(least, dist[i]);
      llout = x.locate(periphery[i]);
    }
  }
 
  return least;
}
