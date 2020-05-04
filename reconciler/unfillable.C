#include <stdio.h>
#include <stdlib.h>

#include "icessmi.h"
#include "ncepgrids.h"
#define MAX_ICE 128
#define MIN_CONC 15

/* Note that hooks are in place for filtering the hemispheric ice grids,
    though this isn't done currently. */
/* Robert Grumbine 4 June 1997 */


//////////////////////////////////
// Something to incorporate in to a library
template <class T, class U>
void gridset(grid2_base<T> &x, grid2_base<U> &y);
//////////////////////////////////


int main(int argc, char *argv[])
{
  FILE *in16, *in17, *in18, *fout;

// Change to 12.7 km polar grids, 5 minute ice grid
  northhigh<unsigned char> nland, nmap;
  southhigh<unsigned char> sland, smap;
  northhigh<float> nf, count, nlandf;
  southhigh<float> sf, scount, slandf;
  global_12th<unsigned char> gmap, refmap;
  global_12th<float> outmap, altmap;

  ijpt ijloc, destloc, ijloc2;
  fijpt tloc;
  latpt outloc;
  float flag = 224.;

  #ifdef VERBOSE
    printf("Entered the filtering program\n"); fflush(stdout);
  #endif
  outmap.set(0.0);
  altmap.set(0.0);
  count.set(0.0);

  in16 = fopen(argv[1],"r");
  in17 = fopen(argv[2],"r");
  in18 = fopen(argv[3],"r");

  gmap.binin(in16);
  nland.binin(in17);
  sland.binin(in18);
  gridset(nlandf, nland);
  gridset(slandf, sland);

  nmap.set((unsigned char) 100);
  gridset(nf, nmap);

  for (ijloc.j = 0; ijloc.j < nf.ypoints(); ijloc.j++) {
    for (ijloc.i = 0; ijloc.i < nf.xpoints(); ijloc.i++) {
       if (nf[ijloc] < MIN_CONC) {
          nf[ijloc] = 0.0;
       }
       if (nlandf[ijloc] == LAND || 
             ijloc.i == 0 || ijloc.j == 0) {
         nf[ijloc] = flag;
       }
    }
  }
  outmap.fromall(nf, nlandf, flag, flag);

// Transfer values, cap, and then fill in gaps.
  smap.set((unsigned char) 100);
  gridset(sf, smap);

  for (ijloc.j = 0; ijloc.j < smap.ypoints(); ijloc.j++) {
    for (ijloc.i = 0; ijloc.i < smap.xpoints(); ijloc.i++) {
       if (slandf[ijloc] == LAND) {
         sf[ijloc] = flag;
       }
    }
  }
// altmap is used as secondary map to check on filling from south hemisphere
  altmap.fromall(sf, slandf, flag, flag);
  
// combine and check for unfillables:
  refmap = gmap;
  for (ijloc.j = 0; ijloc.j < outmap.ypoints(); ijloc.j++) {
  for (ijloc.i = 0; ijloc.i < outmap.xpoints(); ijloc.i++) {
     outloc = outmap.locate(ijloc);
     if (outloc.lat < -20.) outmap[ijloc] = altmap[ijloc];

     if (outmap[ijloc] > 100.1 && outmap[ijloc] != flag) {
       printf("%4d %4d %f over\n",ijloc.i, ijloc.j, (float) outmap[ijloc]);
     }

     if (outloc.lat < -20.) {
       tloc = sf.locate(outloc);
       if (sf.in(tloc) && fabs(outmap[ijloc] - 100.) > 0.01 && gmap[ijloc] < 100) {
         printf("south %4d %4d  %7.3f %8.3f  %8.3f\n",ijloc.i, ijloc.j, 
                          outloc.lat, outloc.lon,  outmap[ijloc]);
         refmap[ijloc] = (unsigned char) flag;
       }
     }
     else if (outloc.lat > 20.0) {
       tloc = nf.locate(outloc);
       if (nf.in(tloc) && fabs(outmap[ijloc]  - 100.) > 0.01 && gmap[ijloc] < 100) {
         printf("north %4d %4d  %7.3f %8.3f  %8.3f\n",ijloc.i, ijloc.j, 
                          outloc.lat, outloc.lon,  outmap[ijloc]);
         refmap[ijloc] = (unsigned char) flag;
       }
     }

     if (outmap[ijloc] == 100. && gmap[ijloc] > 100) {
       printf("%4d %4d %f deland\n",ijloc.i, ijloc.j, (float) outmap[ijloc]);
       refmap[ijloc] = (unsigned char) 100;
     }

       
  }
  }

  fout = fopen(argv[4],"w");
  refmap.binout(fout);
  fclose(fout);
 
   return 0;

}

//Set grid x = grid y, regardless of types
template <class T, class U>
void gridset(grid2_base<T> &x, grid2_base<U> &y) {
  int i, npts = x.xpoints()*x.ypoints();

  if (x.xpoints() != y.xpoints() || x.ypoints() != y.ypoints() ) {
    x.set((T) 0);
  }
  else {
    for (i = 0; i < npts; i++) {
       x[i] = y[i];
    }
  }
  return;
} 
