#include <cstdio>
#include <stack>

#include "ncepgrids.h"
#include "avhrr.h"

#define MAXOBS 1000
#ifdef IBM
  extern "C" float arcdis(float &long1, float &lat1, float &long2,
                          float &lat2 );
#else
  extern "C" float arcdis_(float &long1, float &lat1, float &long2,
                           float &lat2 );
#endif

// Check for 'extrapolation error' issues with a set of input data
// This occurrs in 2dvar/GSI/... when the gradient imposed by two observations
//   is higher than can be handled by the system

int main(int argc, char *argv[]) {
  avhrrpt x, ref, prev, now;
  FILE *fin, *foutlow, *fouthigh;
  int i = 0, npts;
  global_ice<int> count;
  global_ice<float> sst;
// below assignment needed as we can't do math on stacks, and math
//   is needed for the metricgrids.  Doing our binning to half degree resolution.
  grid2<stack<avhrrpt> > data(count.xpoints(), count.ypoints() );
  mvector<avhrrpt> tmpin(MAXOBS);

  latpt ll;
  ijpt loc;

  fin = fopen(argv[1],"r");
  if (fin == (FILE *) NULL ) {
    printf("failed to open %s\n",argv[1]);
    return 1;
  }

  count.set(0);
  sst.set((float) 0.0);

// Loop over input file and distribute to stacks in a data grid
  while (!feof(fin) && i < MAXOBS-1) {
    fread(&x, sizeof(x), 1, fin);
    if (!feof(fin)) {i++; }
      else {break;}

    tmpin[i] = x;

    ll.lat = x.clat;
    ll.lon = x.clon;
    loc    = count.locate(ll);
    data[loc].push(x);

  }
  fclose(fin);
  npts = i;
  printf("npts = %d\n",npts);

// Do exhaustive binary comparison
  int j;
  float grad, dist;
  latpt ll1, ll2;

  for (i = 0; i < npts; i++) {
    prev = tmpin[i]; 
    ll1.lat = prev.clat;
    ll1.lon = prev.clon;
    for (j = i+1; j <= npts; j++) {
      now = tmpin[j];
      ll2.lat = now.clat;
      ll2.lon = now.clon;

      grad = (prev.sst - now.sst);
      dist = arcdis_(ll1.lon, ll1.lat, ll2.lon, ll2.lat);
      grad /= dist;
      printf("%d %d  %f \n",i,j,grad);
    }
  }


// Loop over the data grid and parcel out to the target grids:
  for (loc.j = 0; loc.j < count.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < count.xpoints(); loc.i++) {
     if (!data[loc].empty() ) {
       count[loc] = data[loc].size();
       ll = count.locate(loc);

       // Now pop through stack and do something with data for that point
       while (!data[loc].empty() ) {
          ref = data[loc].top();
          data[loc].pop();
          
          sst[loc] += ref.sst;

          // put in secondary grid here -- average out in later loop
          ll.lat = ref.clat;
          ll.lon = ref.clon;
       }
     }
  }
  }

//---------------------------------------------------------
  palette<unsigned char> gg(19,65);

  return 0;
}
