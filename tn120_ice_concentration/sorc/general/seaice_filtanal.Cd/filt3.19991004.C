#include <stdio.h>
#include <stdlib.h>

#include "icessmi.new.h"
#include "ncepgrids.h"
#define MAX_ICE 128
#define MIN_CONC 15

/* Filter the global ice concentration grid according to the SST and
    to the land mask */
/* Note that hooks are in place for filtering the hemispheric ice grids,
    though this isn't done currently. */
/* Robert Grumbine 4 June 1997 */
/* Major revision: change to C++, go to wgrib decoding of sst files
   Robert Grumbine 8 September 1999 */

/* Function to fill in the large number of single-point dropouts (bad,
     missing, or weather) that are surrounded by valid ice concentrations */
template <class T>
int gapfill(grid2<T> &x, T lowest_valid, T highest_valid, T flag);
template <class T>
int isolani(metricgrid<T> &x, T flag, T replace); // remove points which 
               //are surrounded by flagged values and replace 
template <class T>
void weather_zero(grid2<T> &x);


int main(int argc, char *argv[])
{
  FILE *in10, *in11, *in12, *out13, *out14, *out15, *in16, *in17, *in18;
  FILE *fin;

  northgrid<unsigned char> nland;
  northgrid<float> nmap, nf, nlf, count, nlandf;
  southgrid<unsigned char> sland;
  southgrid<float> smap, sf, slf, scount, slandf;
  global_ice<unsigned char> cout2, gmap, refmap;
  global_ice<float> outmap, altmap;
  global_sst<float> sst, errs; 

  ijpt ijloc, destloc, ijloc2;
  fijpt tloc;
  latpt iceloc, outloc;
  unsigned char tmp;
  float filt_temp, flag;
  int range = 0; //min distance to land for a valid concentration
  int isolates, filled;
  palette<unsigned char> gg(19, 65);

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
  //printf("filt_temp = %f\n",filt_temp);

  nmap.binin(in10);
  nland.binin(in17);
  smap.binin(in11);
  sland.binin(in18);
  sst.binin(in12);
  gmap.binin(in16);
  if (nmap.average() < 2.56) nmap *= 100;
  if (smap.average() < 2.56) smap *= 100;
  for (ijloc.j = 0; ijloc.j < nland.ypoints(); ijloc.j++) {
  for (ijloc.i = 0; ijloc.i < nland.xpoints(); ijloc.i++) {
    nlandf[ijloc] = nland[ijloc];
  }
  }
  for (ijloc.j = 0; ijloc.j < sland.ypoints(); ijloc.j++) {
  for (ijloc.i = 0; ijloc.i < sland.xpoints(); ijloc.i++) {
    slandf[ijloc] = sland[ijloc];
  }
  }

  // Testing to see if all points get sea ice
  //nmap.set(100);
  //smap.set(100);
/* Now average from polar stereographic on to the lat-long grid */
/* RG 8 Sept 1999: In C++, this can now be made a trivial function passing
    a metricgrid in, arbitrary mapping and origin */
  flag = (float) LAND;
// Mask out a range of 2 grid points near land
//DEB  count = nmap;
//DEB  count.colorproc(nlandf, 7, 65, &std_ice_coloring);
//DEB  count.xpm("nmap.xpm", 1, gg);
//DEB  count.set(0.0);
  for (ijloc.j = 0; ijloc.j < nmap.ypoints(); ijloc.j++) {
    for (ijloc.i = 0; ijloc.i < nmap.xpoints(); ijloc.i++) {
       nlf[ijloc] = (float) nland[ijloc];
       if (nland.anyof(flag, range, ijloc) > 0) { 
         count[ijloc] = flag;
       }
    }
  }
  for (ijloc.j = 0; ijloc.j < nmap.ypoints(); ijloc.j++) {
    for (ijloc.i = 0; ijloc.i < nmap.xpoints(); ijloc.i++) {
       if (count[ijloc] > 0.) nlf[ijloc] = flag;
       nf[ijloc] = (float) nmap[ijloc];
       if (nf[ijloc] > 100. && nf[ijloc] < MAX_ICE) nf[ijloc] = 100.;
       if (nf[ijloc] < MIN_CONC) nf[ijloc] = 0.0;
    }
  }
  filled  = gapfill(nf, (float)0., (float)100., (float)224.); //no-data points
  filled += gapfill(nf, (float)0., (float)100., (float)166.); //bad-data points
  filled += gapfill(nf, (float)0., (float)100., (float)177.); //weather points
  weather_zero(nf);
  filled += gapfill(nf, (float)0., (float)100., (float)224.); //no-data points
  filled += gapfill(nf, (float)0., (float)100., (float)166.); //bad-data points

//DEB  count = nf;
//DEB  count.colorproc(nlandf, 7, 65, &std_ice_coloring);
//DEB  count.xpm("nf.xpm", 1, gg);
//DEB  count.set(0.0);
  for (ijloc.j = 0; ijloc.j < nmap.ypoints(); ijloc.j++) {
    for (ijloc.i = 0; ijloc.i < nmap.xpoints(); ijloc.i++) {
       if (nf[ijloc] == WEATHER) nf[ijloc] = 0.;
       if (nf[ijloc] >= MAX_ICE || nlf[ijloc] == COAST ) {
        nf[ijloc] = 224.;
        nlf[ijloc] == flag;
       }
    }
  }
  outmap.fromall(nf, nlf, flag, 224.);

// Mask out a range of 2 grid points near land
//DEB  scount = smap;
//DEB  scount.colorproc(slandf, 7, 65, &std_ice_coloring);
//DEB  scount.xpm("smap.xpm", 1, gg);
//DEB  scount.set(0.0);
  for (ijloc.j = 0; ijloc.j < smap.ypoints(); ijloc.j++) {
    for (ijloc.i = 0; ijloc.i < smap.xpoints(); ijloc.i++) {
       slf[ijloc] = (float) sland[ijloc];
       if (sland.anyof(flag, range, ijloc) > 0) scount[ijloc] = flag;
    }
  }
// Transfer values, cap, and then fill in gaps.
  for (ijloc.j = 0; ijloc.j < smap.ypoints(); ijloc.j++) {
    for (ijloc.i = 0; ijloc.i < smap.xpoints(); ijloc.i++) {
       if (scount[ijloc] > 0.) slf[ijloc] = flag;
       sf[ijloc] = (float) smap[ijloc];
       if (sf[ijloc] > 100. && sf[ijloc] < MAX_ICE) sf[ijloc] = 100.;
       if (sf[ijloc] < MIN_CONC) sf[ijloc] = 0.0;
    }
  }
  filled += gapfill(sf, (float)0., (float)100., (float)224.); //no-data points
  filled += gapfill(sf, (float)0., (float)100., (float)166.); //bad-data points
  filled += gapfill(sf, (float)0., (float)100., (float)177.); //weather points
  weather_zero(sf);
  filled += gapfill(sf, (float)0., (float)100., (float)224.); //no-data points
  filled += gapfill(sf, (float)0., (float)100., (float)166.); //bad-data points
  printf("filled %d points\n",filled);
//  isolates += isolani(sf, (float) 224., (float) 224.);
//  isolates += isolani(sf, (float) 166., (float) 224.);
//  isolates += isolani(sf, (float) 177., (float) 224.);
//DEB  scount = sf;
//DEB  scount.colorproc(slandf, 7, 65, &std_ice_coloring);
//DEB  scount.xpm("sf.xpm", 1, gg);
//DEB  scount.set(0.0);
//  printf("isolates %d\n",isolates);

  for (ijloc.j = 0; ijloc.j < smap.ypoints(); ijloc.j++) {
    for (ijloc.i = 0; ijloc.i < smap.xpoints(); ijloc.i++) {
       //slf[ijloc] = (float) sland[ijloc];
       if (sf[ijloc] == WEATHER) sf[ijloc] = 0.;
       if (sf[ijloc] >= MAX_ICE || slf[ijloc] == COAST ) {
        sf[ijloc] = 224.;
        slf[ijloc] == flag;
       }
    }
  }
  altmap.fromall(sf, slf, flag, 224.);
  
  for (ijloc.j = 0; ijloc.j < outmap.ypoints(); ijloc.j++) {
  for (ijloc.i = 0; ijloc.i < outmap.xpoints(); ijloc.i++) {
     outloc = outmap.locate(ijloc);
     //DEBUGif (outmap[ijloc] != 224. || altmap[ijloc] != 224.) {
     //DEBUG   printf("%3d %3d  %6.1f %7.2f  %6.1f %6.1f\n",ijloc.i, ijloc.j, 
     //DEBUG      outloc.lat, outloc.lon, outmap[ijloc], altmap[ijloc]);
     //DEBUG}
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
         //DEBUG printf("zeroing %3d %3d %6.3f %6.3f\n" ,
         //DEBUG   ijloc.i, ijloc.j, sst[ijloc2], outmap[ijloc] );
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

template <class T>
int gapfill(grid2<T> &x, T lowest_valid, T highest_valid, T flag) {
  ijpt loc, ip1, jp1, im1, jm1;
  ijpt tip1, tjp1, tim1, tjm1;
  int count = 0, passno=0, valid, tcount = 0;
  T tmp;

  ip1.i =  1; ip1.j =  0;
  im1.i = -1; im1.j =  0;
  jp1.i =  0; jp1.j =  1;
  jm1.i =  0; jm1.j = -1;
  //printf("gapfill %f %f %f\n", lowest_valid, highest_valid, flag);

//This portion does a 1 point fill -- point surrounded by valid
//  points.
  count = 0;
  for (loc.j = 0; loc.j < x.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints() ; loc.i++) {
    if (x[loc] == flag) {
      tip1 = loc ; tip1 += ip1;
      tjp1 = loc ; tjp1 += jp1;
      tim1 = loc ; tim1 += im1;
      tjm1 = loc ; tjm1 += jm1;
      //if (x[tip1] >= lowest_valid && x[tip1] <= highest_valid &&  
      //    x[tim1] >= lowest_valid && x[tim1] <= highest_valid &&  
      //    x[tjp1] >= lowest_valid && x[tjp1] <= highest_valid &&  
      //    x[tjm1] >= lowest_valid && x[tjm1] <= highest_valid ) {
      valid = (x[tip1] >= lowest_valid && x[tip1] <= highest_valid ) +
              (x[tim1] >= lowest_valid && x[tim1] <= highest_valid ) +
              (x[tjp1] >= lowest_valid && x[tjp1] <= highest_valid ) +
              (x[tjm1] >= lowest_valid && x[tjm1] <= highest_valid ) ;
      if (valid == 4) {
        tmp =  0.25 * (x[tip1] + x[tim1] + x[tjp1] + x[tjm1]);
        x[loc] = tmp;
        if (x[loc] < MIN_CONC) x[loc] = 0.0;
        count += 1;
      }
    }
  }
  }

// This one is to be iterated as it is possible to fill in portions that
//   can later be used to fill in further.
  passno = 0;
  do {
    tcount = 0;
    passno += 1;
    for (loc.j = 0; loc.j < x.ypoints() ; loc.j++) {
    for (loc.i = 0; loc.i < x.xpoints() ; loc.i++) {
      if (x[loc] == flag) {
        tip1 = loc ; tip1 += ip1;
        tjp1 = loc ; tjp1 += jp1;
        tim1 = loc ; tim1 += im1;
        tjm1 = loc ; tjm1 += jm1;
        valid = (x[tip1] >= lowest_valid && x[tip1] <= highest_valid ) +
                (x[tim1] >= lowest_valid && x[tim1] <= highest_valid ) +
                (x[tjp1] >= lowest_valid && x[tjp1] <= highest_valid ) +
                (x[tjm1] >= lowest_valid && x[tjm1] <= highest_valid ) ;
        if (valid == 4) {
          tmp =  0.25 * (x[tip1] + x[tim1] + x[tjp1] + x[tjm1]);
          x[loc] = tmp;
          if (x[loc] < MIN_CONC) x[loc] = 0.0;
          tcount += 1;
          //printf("4 pt %3d %3d\n",loc.i, loc.j);
        }
        else if (valid == 3) {
          tmp = 0.;
          if (x[tip1] >= lowest_valid && x[tip1] <= highest_valid ) tmp += x[tip1];
          if (x[tim1] >= lowest_valid && x[tim1] <= highest_valid ) tmp += x[tim1];
          if (x[tjp1] >= lowest_valid && x[tjp1] <= highest_valid ) tmp += x[tjp1];
          if (x[tjm1] >= lowest_valid && x[tjm1] <= highest_valid ) tmp += x[tjm1];
          tmp /= valid;
          tcount += 1;
          x[loc] = tmp;
          if (x[loc] < MIN_CONC) x[loc] = 0.0;
          //printf("3 pt %3d %3d\n",loc.i, loc.j);
        }
      }
    }
    }
    count += tcount;
    printf("Pass %d tcount %d\n",passno, tcount);

  } while (tcount > 0 && passno < 10);

  return count;
}

template <class T>
int isolani(metricgrid<T> &x, T flag, T replace) {
  int count = 0;
  ijpt loc, ip1, jp1, im1, jm1 ;
  ijpt  tip1, tjp1, tim1, tjm1 ;

  ip1.i =  1; ip1.j = 0;
  im1.i = -1; ip1.j = 0;
  jp1.i =  0; jp1.j =  1;
  jm1.i =  0; jm1.j = -1;

  for (loc.j = 1; loc.j < x.ypoints() - 1; loc.j++) {
  for (loc.i = 1; loc.i < x.xpoints() - 1; loc.i++) {
    if ( x[loc] != flag && x[loc] != replace) {
       tip1 = loc ; tip1 += ip1;
       tim1 = loc ; tim1 += im1;
       tjp1 = loc ; tjp1 += jp1;
       tjm1 = loc ; tip1 += jm1;
       if (x[tip1] == flag && x[tim1] == flag && x[tjp1] == flag && 
           x[tjm1] == flag) {
         x[loc] = replace;
         count += 1;
       }
    }
  }
  }
  //printf("%d isolated points removed\n",count);
  return count;
}
template <class T>
void weather_zero(grid2<T> &x) {
  ijpt loc;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x[loc] == WEATHER) x[loc] = 0;
  }
  }
  return;
}

