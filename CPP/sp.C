#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  ijpt loc, ttloc, tloc;
  latpt ll, ll1, ll2;
  global_ice<unsigned char> landin;
  northgrid<float> dist;
  float tmp;

  fin = fopen(argv[1], "r");
  landin.binin(fin);
  fclose(fin);
  printf("read in land mask\n"); fflush(stdout);

  for (loc.j = 0; loc.j < dist.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < dist.xpoints(); loc.i++) {
    ll = dist.locate(loc);
    tloc = landin.locate(ll);

    if (landin.in(tloc)) {
      if (landin[tloc] != 0 && landin[tloc] != 157 && landin[tloc] != 195) {
        printf("%4d %4d  %3d\n",tloc.i, tloc.j, landin[tloc]); fflush(stdout);
      }
      if (landin[tloc] == 0) {
        dist[loc] = 0.0;
      }
      else { 
        dist[loc] = 1e10; 
      }
    }
  }
  }

  float dlat = 1.0;
  float c1, c2, dlon;

  for (dlat = 4; dlat < 20; dlat += 1) {
    for (loc.j = 0; loc.j < dist.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < dist.xpoints(); loc.i++) {
      if (dist[loc] != 0 && dist[loc] >= dlat*111 - 50.) {
        ll1 = dist.locate(loc);
 
        for (tloc.j = 0; tloc.j < landin.ypoints(); tloc.j++) {
        for (tloc.i = 0; tloc.i < landin.xpoints(); tloc.i++) {
          ll2 = landin.locate(tloc);
//          if ((fabs(ll2.lat - ll1.lat) > dlat ) || 
//              (fabs(ll2.lat - ll1.lat) < (dlat-1 ) ) ) continue;
          if (landin[tloc] == 0 && (fabs(ll2.lat - ll1.lat) < dlat) ) {
            tmp = ARCDIS(ll1.lon, ll1.lat, ll2.lon, ll2.lat);
            dist[loc] = min(dist[loc], ARCDIS(ll1.lon, ll1.lat, ll2.lon, ll2.lat) );
          }
        }
        }
        printf("%2d  %3d %3d  %f %f dist %f\n",(int)dlat, loc.i, loc.j, ll1.lon, ll1.lat, dist[loc]);
        fflush(stdout);
      }
    }
    }
  }

  fout = fopen("fout", "w");
  dist.binout(fout);
  fclose(fout);


  return 0;
}
