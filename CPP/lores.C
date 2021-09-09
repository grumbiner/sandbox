#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  ijpt loc, tloc;
  latpt ll1, ll2;
  global_ice<unsigned char> landin;
  global_ice<float> dist;

  fin = fopen(argv[1], "r");
  landin.binin(fin);

  for (loc.j = 0; loc.j < landin.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < landin.xpoints(); loc.i++) {
    if (landin[loc] != 0 && landin[loc] != 157 && landin[loc] != 195) {
      printf("%d %d  %d\n",loc.i, loc.j, landin[loc]);
    }
    if (landin[loc] == 0) {
      dist[loc] = 0.0;
    }
    else { 
      dist[loc] = 1e10; 
    }
  }
  }

  float tmp, iter = 1;
  while (dist.gridmax() == 1e10) {
    printf("iter = %f\n",iter); fflush(stdout);
  for (loc.j = 0; loc.j < landin.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < landin.xpoints(); loc.i++) {
    if (landin[loc] != 0 && dist[loc] > (iter-1)*111. - 50. ) {
      ll1 = dist.locate(loc);
      for (tloc.j = 0; tloc.j < dist.ypoints(); tloc.j++) {
      for (tloc.i = 0; tloc.i < dist.xpoints(); tloc.i++) {
        if (landin[tloc] == 0) {
          ll2 = dist.locate(tloc);
          if (fabs(ll2.lat - ll1.lat) > iter || fabs(ll2.lat - ll1.lat) < (iter-1) ) continue;
          tmp = ARCDIS(ll1.lon, ll1.lat, ll2.lon, ll2.lat);
          dist[loc] = min(dist[loc], ARCDIS(ll1.lon, ll1.lat, ll2.lon, ll2.lat) );
        }
      }
      }
//      printf("%d  %d %d  %f %f %f\n",(int) iter, loc.i, loc.j, ll1.lon, ll1.lat, dist[loc]); 
//      fflush(stdout); 
    } 
  }
  }
    iter += 1.;
  }

  for (loc.j = 0; loc.j < landin.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < landin.xpoints(); loc.i++) {
    if (dist[loc] > 0) {
      ll1 = dist.locate(loc);
      printf("%d %d  %f %f %f\n",loc.i, loc.j, ll1.lon, ll1.lat, dist[loc]); 
    }
  }
  }
  fflush(stdout); 

  return 0;
}
