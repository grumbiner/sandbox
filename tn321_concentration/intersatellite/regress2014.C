#include <cstdio>
#include <stack>
using namespace std;
// Perform intersatellite regression on tb for passive microwave sea ice
// Robert Grumbine 1 Jul 2014 -- start up for ssmis on F16, 18 versus F17

#include "ssmisu.h"
#include "icessmis.h"

#include "mvector.h"
#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin16, *fin17, *fin18;
  ssmisupt x;
  global_ice<float> dist;
  grid2<stack<ssmisupt> > f16(dist.xpoints(), dist.ypoints()), 
                          f17(dist.xpoints(), dist.ypoints()), 
                          f18(dist.xpoints(), dist.ypoints());
  ijpt loc;
  latpt ll;

  fin16 = fopen(argv[1], "r");
  fin17 = fopen(argv[2], "r");
  fin18 = fopen(argv[3], "r");
  while (!feof(fin16)) {
    fread(&x, sizeof(ssmisupt), 1, fin16);
    ll.lat = x.clat; ll.lon = x.clon;
    loc    = dist.locate(ll);
    f16[loc].push(x);
  }
  while (!feof(fin17)) {
    fread(&x, sizeof(ssmisupt), 1, fin17);
    ll.lat = x.clat; ll.lon = x.clon;
    loc    = dist.locate(ll);
    f17[loc].push(x);
  }
  while (!feof(fin18)) {
    fread(&x, sizeof(ssmisupt), 1, fin18);
    ll.lat = x.clat; ll.lon = x.clon;
    loc    = dist.locate(ll);
    f18[loc].push(x);
  }
  fclose(fin16);
  fclose(fin17);
  fclose(fin18);

  mvector<double> sum(8), sumsq(8);
  int i, count;
  double ref = 150.0;
  for (loc.j = 0; loc.j < f16.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < f16.xpoints(); loc.i++) {
    if ((f17[loc].size() != 0) && ( f16[loc].size() != 0 || f18[loc].size() != 0) ) {
      ll = dist.locate(loc);
      printf("%3d %3d  %6.2f %6.2f  %2d %2d %2d  ",loc.i, loc.j, ll.lon, ll.lat, 
          f16[loc].size(), f17[loc].size(), f18[loc].size() );
      sum   = 0.0;
      sumsq = 0.0; 
      count = f17[loc].size();
      while (!f17[loc].empty() ) {
        x = f17[loc].top();
        for (i = 0; i < sum.xpoints(); i++) {
          sum[i]   += (x.obs[i].tmbr-ref);
          sumsq[i] += (x.obs[i].tmbr-ref)*(x.obs[i].tmbr-ref);
        }
        f17[loc].pop();
      }
      sum   /= count;
      sumsq /= count;
      for (i = 0; i < sum.xpoints(); i++) {
        printf("%1d %6.2f %5.2f ",i, sum[i], sqrt(sumsq[i] - sum[i]*sum[i]));
      }
      printf("\n");

    }
  }
  }

  return 0;
}
