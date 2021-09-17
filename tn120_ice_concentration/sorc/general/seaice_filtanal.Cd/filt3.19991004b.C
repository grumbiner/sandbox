#include <stdio.h>
#include <stdlib.h>

#include "icessmi.new.h"
#include "ncepgrids.h"
#define MAX_ICE 128

/* Filter the global ice concentration grid according to the SST and
    to the land mask */
/* Note that hooks are in place for filtering the hemispheric ice grids,
    though this isn't done currently. */
/* Robert Grumbine 4 June 1997 */
/* Major revision: change to C++, go to wgrib decoding of sst files
   Robert Grumbine 8 September 1999 */

int main(int argc, char *argv[])
{
  FILE *in10, *in11, *in12, *out13, *out14, *out15, *in16, *in17, *in18;
  FILE *fin;

  northgrid<unsigned char> nmap, nland;
  northgrid<float> nf, nlf, count;
  southgrid<unsigned char> smap, sland;
  southgrid<float> sf, slf, scount;
  global_ice<unsigned char> cout2, gmap, refmap;
  global_ice<float> outmap, altmap;
  global_sst<float> sst, errs; 

  ijpt ijloc, destloc, ijloc2;
  fijpt tloc;
  latpt iceloc, outloc;
  unsigned char tmp;
  float filt_temp, flag;

  outmap.set(0.0);
  altmap.set(0.0);
  count.set(0.0);

  in12 = fopen(argv[1],"r");
  in10 = fopen(argv[2],"r");
  in11 = fopen(argv[3],"r");
  in16 = fopen(argv[7],"r");
  in17 = fopen(argv[9],"r");
  in18 = fopen(argv[10],"r");
  if (in10 == NULL || in11 == NULL || in12 == NULL || in16 == NULL ||
      in17 == NULL || in18 == NULL ) {
    printf("Failed to open a required input file in file\n");
    if (in10 == NULL) { printf(" - northern hemisphere file\n");}
    if (in11 == NULL) { printf(" - southern hemisphere file\n");}
    if (in12 == NULL) { printf(" - global sst file\n");}
    if (in16 == NULL) { printf(" - global land mask file\n");}
    if (in17 == NULL) { printf(" - n. hemisphere land mask file\n");}
    if (in18 == NULL) { printf(" - s. hemisphere land mask file\n");}
    return -1;
  }

  out13 = fopen(argv[4],"w");
  out14 = fopen(argv[5],"w");
  out15 = fopen(argv[6],"w");
  if (out13 == NULL || out14 == NULL || out15 == NULL) {
    printf("Failed to open a required output file in file\n");
    if (out13 == NULL) { printf(" - global ice file\n");}
    if (out14 == NULL) { printf(" - n. hemisphere ice file\n");}
    if (out15 == NULL) { printf(" - s. hemisphere ice file\n");}
    return -1;
  }
    
  filt_temp = (float) atof(argv[8]);
  printf("filt_temp = %f\n",filt_temp);

  nmap.binin(in10);
  nland.binin(in17);
  smap.binin(in11);
  sland.binin(in18);
  sst.binin(in12);
  gmap.binin(in16);

  // Testing to see if all points get sea ice
  //nmap.set(100);
  //smap.set(100);
/* Now average from polar stereographic on to the lat-long grid */
/* RG 8 Sept 1999: In C++, this can now be made a trivial function passing
    a metricgrid in, arbitrary mapping and origin */
  flag = (float) LAND;
// Mask out a range of 2 grid points near land
  for (ijloc.j = 0; ijloc.j < nmap.ypoints(); ijloc.j++) {
    for (ijloc.i = 0; ijloc.i < nmap.xpoints(); ijloc.i++) {
       nlf[ijloc] = (float) nland[ijloc];
       if (nland.anyof(flag, 2, ijloc) > 0) count[ijloc] = flag;
    }
  }
  for (ijloc.j = 0; ijloc.j < nmap.ypoints(); ijloc.j++) {
    for (ijloc.i = 0; ijloc.i < nmap.xpoints(); ijloc.i++) {
       if (count[ijloc] > 0.) nlf[ijloc] = flag;
    }
  }
  for (ijloc.j = 0; ijloc.j < nmap.ypoints(); ijloc.j++) {
    for (ijloc.i = 0; ijloc.i < nmap.xpoints(); ijloc.i++) {
       nf[ijloc] = (float) nmap[ijloc];
       if (nf[ijloc] > 100. && nf[ijloc] < 128.) nf[ijloc] = 100.;
       if (nf[ijloc] >= 128. || nlf[ijloc] == COAST ) {
        nlf[ijloc] == flag;
       }
    }
  }
  outmap.fromall(nf, nlf, flag, 224.);

// Mask out a range of 2 grid points near land
  for (ijloc.j = 0; ijloc.j < smap.ypoints(); ijloc.j++) {
    for (ijloc.i = 0; ijloc.i < smap.xpoints(); ijloc.i++) {
       slf[ijloc] = (float) sland[ijloc];
       if (sland.anyof(flag, 2, ijloc) > 0) scount[ijloc] = flag;
    }
  }
  for (ijloc.j = 0; ijloc.j < smap.ypoints(); ijloc.j++) {
    for (ijloc.i = 0; ijloc.i < smap.xpoints(); ijloc.i++) {
       if (scount[ijloc] > 0.) slf[ijloc] = flag;
    }
  }
  for (ijloc.j = 0; ijloc.j < smap.ypoints(); ijloc.j++) {
    for (ijloc.i = 0; ijloc.i < smap.xpoints(); ijloc.i++) {
       sf[ijloc] = (float) smap[ijloc];
       if (nf[ijloc] > 100. && nf[ijloc] < 128.) nf[ijloc] = 100.;
       slf[ijloc] = (float) sland[ijloc];
       if (sf[ijloc] >= 128. || slf[ijloc] == COAST ) {
        slf[ijloc] == flag;
       }
    }
  }
  altmap.fromall(sf, slf, flag, 224.);
  
  for (ijloc.j = 0; ijloc.j < outmap.ypoints(); ijloc.j++) {
  for (ijloc.i = 0; ijloc.i < outmap.xpoints(); ijloc.i++) {
     outloc = outmap.locate(ijloc);
     if (outmap[ijloc] != 224. || altmap[ijloc] != 224.) {
        printf("%3d %3d  %6.1f %7.2f  %6.1f %6.1f\n",ijloc.i, ijloc.j, 
           outloc.lat, outloc.lon, outmap[ijloc], altmap[ijloc]);
     }
     if (outloc.lat < -20.) outmap[ijloc] = altmap[ijloc];
  }
  }

  
/* Begin SST filtering: Note that while the gribbed fields are in K, the
   operational OISST is in C.
   Only change if there is a fair degreeof confidence in the SST,
   confidence being ranged from 0 to 1 with 0 being best.  */
    for (ijloc.j = 0; ijloc.j < outmap.ypoints(); ijloc.j++) {
      for (ijloc.i = 0; ijloc.i < outmap.xpoints() ; ijloc.i++) {
        outloc = outmap.locate(ijloc);
        tloc = sst.locate(outloc);
        ijloc2 = tloc;
        if (sst[ijloc2] >= filt_temp && outmap[ijloc] > 0.0 ) {
          printf("zeroing %3d %3d %6.3f %6.3f\n" ,
            ijloc.i, ijloc.j, sst[ijloc2], outmap[ijloc] );
          outmap[ijloc] = 0.0; 
        }
        cout2[ijloc] = (unsigned char) (outmap[ijloc] + 0.5); 
      }
    }
/* end SST filtering */

/* Now do land mask filtering */
      for (ijloc.j = 0; ijloc.j < outmap.ypoints(); ijloc.j++ ) {
         for (ijloc.i = 0; ijloc.i < outmap.xpoints(); ijloc.i++ ) {
            if (gmap[ijloc] == (unsigned char) LAND) cout2[ijloc] = (unsigned char) 0;
         }
      }

/* Write out the filtered global grid */
     cout2.binout(out13);

/* Place sst filtering of polar stereo grids here */

     return 0;

}
