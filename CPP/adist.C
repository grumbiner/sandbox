#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  ijpt loc, tloc;
  latpt ll1, ll2;
  global_12th<unsigned char> landin;
  global_12th<float> dist;

  fin = fopen(argv[1], "r");
  landin.binin(fin);

  for (loc.j = 0; loc.j < landin.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < landin.xpoints(); loc.i++) {
    if (landin[loc] != 0 && landin[loc] != 157 && landin[loc] != 195) {
      printf("%d %d  %d\n",loc.i, loc.j, landin[loc]);
    }
    if (landin[loc] == 157) {
      dist[loc] = 0.0;
    }
    else { 
      dist[loc] = 1e10; 
    }
  }
  }

  float tmp;
  for (loc.j = 0; loc.j < landin.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < landin.xpoints(); loc.i++) {
    if (landin[loc] != 157) {
      ll1 = dist.locate(loc);
      for (tloc.j = 0; tloc.j < dist.ypoints(); tloc.j++) {
      for (tloc.i = 0; tloc.i < dist.xpoints(); tloc.i++) {
        if (landin[tloc] == 157) {
          ll2 = dist.locate(tloc);
          if (fabs(ll2.lat - ll1.lat) > 9.) continue;
          // replace with arcdis 
          tmp = ARCDIS(ll1.lon, ll1.lat, ll2.lon, ll2.lat);
          dist[loc] = min(dist[loc], ARCDIS(ll1.lon, ll1.lat, ll2.lon, ll2.lat) );
          //printf("tloc %d %d tmp %f dist %f\n",tloc.i, tloc.j, tmp, dist[loc]); 
          //fflush(stdout);
        }
      }
      }
      printf("loc = %d %d  %f %f dist %f\n",loc.i, loc.j, ll1.lon, ll1.lat, dist[loc]); 
      fflush(stdout); 
    } 
  }
  }

  return 0;
}
