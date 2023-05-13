#include <stdio.h>
#include <stdlib.h>
#include <math.h>
//#include <macros.h>

// #include "icessmi.h"
#include "ncepgrids.h"
#define LAND 157
#define WEATHER 177
#define COAST 195
#define MAXICE 127
#define MINICE  15

#undef VERBOSE

extern "C" void mapll(const float lat, const float lon, int *i, int *j, const float xorig,
           const float yorig, const float eccen2, const float slat, 
           const float slon, const float rearth, const float dx, 
           const float dy, const float sgn);
extern "C" void mapxy(float *alat, float *along, int i, int j, const float xorig,
            const float yorig, const float dx, const float dy,
            const float slat, const float slon, const float sgn,
            const float rearth, const float eccen2);

#ifdef OPERATIONS
  extern int DEGRIB(int *nx, int *ny, float *sst);
#endif

/* Filter the global ice concentration grid according to the SST and
    to the land mask */
/* Note that hooks are in place for filtering the hemispheric ice grids,
    though this isn't done currently. */
/* Robert Grumbine 4 June 1997 */
/* Do on-the-fly checking as to whether the concentrations given were */
/*   in percents or a fraction 10/98 */
/* Read in land masks and use in decision process 11/98 */ 
   

int main(int argc, char *argv[])
{
  FILE *in10, *in11, *in12, *out13, *out14, *out15, *in16, *in17, *in18;

  global_ice<float> count, outmap;
  global_sst<float> sst;
  northgrid<float> nmap;
  northgrid<unsigned char> nland;
  southgrid<float> smap; 
  southgrid<unsigned char> sland;
  global_ice<unsigned char> cout2;
  global_ice<unsigned char> gmap;

  int i, j;
  int tmp, xrat, yrat;
  float llat, llon, xorig, yorig, filt_temp;
  ijpt x, y ;
  latpt loc;
  float scalen, scales;

  for (x.j = 0; x.j < outmap.ypoints() ; x.j++ ) {
    for (x.i = 0; x.i < outmap.xpoints() ; x.i++ ) {
       outmap[x] = 0.0;
       count[x] = 0.0;
    }
  }

  in10 = fopen(argv[2],"r");
  in11 = fopen(argv[3],"r");
  in16 = fopen(argv[7],"r");
  in17 = fopen(argv[9],"r");
  in18 = fopen(argv[10],"r");
  if (in10 == (FILE *) NULL || in11 == (FILE *) NULL || in16 == (FILE *) NULL
       || in17 == (FILE *) NULL || in18 == (FILE *) NULL ) {
    printf("Failed to open a required input file in filt\n");
    if (in10 == (FILE *) NULL) { printf(" - northern hemisphere file\n");}
    if (in11 == (FILE *) NULL) { printf(" - southern hemisphere file\n");}
    if (in16 == (FILE *) NULL) { printf(" - global land mask file\n");}
    if (in17 == (FILE *) NULL) { printf(" - nhem land mask file\n");}
    if (in18 == (FILE *) NULL) { printf(" - shem land mask file\n");}
    
    return -1;
  }

  out13 = fopen(argv[4],"w");
  out14 = fopen(argv[5],"w");
  out15 = fopen(argv[6],"w");
  if (out13 == (FILE *) NULL || out14 == (FILE *) NULL || out15 == (FILE *) NULL) {
    printf("Failed to open a required output file in filt\n");
    if (out13 == (FILE *) NULL) { printf(" - global ice file\n");}
    if (out14 == (FILE *) NULL) { printf(" - n. hemisphere ice file\n");}
    if (out15 == (FILE *) NULL) { printf(" - s. hemisphere ice file\n");}
    return -1;
  }
    

  #ifdef VERBOSE
    printf("about to read in nmap\n"); fflush(stdout);
  #endif
  nland.binin(in17);
  nmap.binin(in10);
  scalen = nmap.gridmax();
  if (scalen > 100. ) {
    scalen = 1.;
  }
  else if (scalen < 2.6) {
    scalen = 100.;
  }
  else {
    printf("Erroneous data range in nmap %f is maximum \n",scalen);
  }

  #ifdef VERBOSE
    printf("about to read in smap\n"); fflush(stdout);
  #endif
  sland.binin(in18);
  smap.binin(in11);
  scales = smap.gridmax();
  if (scales > 100. ) {
    scales = 1.;
  }
  else if (scales < 2.6) {
    scales = 100.;
  }
  else {
    printf("Erroneous data range in smap %f is maximum \n",scales);
  }

  #ifdef VERBOSE
    printf("about to read in land mask\n"); fflush(stdout);
  #endif
  gmap.binin(in16);


/* Now average from polar stereographic on to the lat-long grid */
  for (x.j = 0; x.j < nmap.ypoints() ; x.j++) {
    for (x.i = 0; x.i < nmap.xpoints() ; x.i++) {
       mapxy(&llat, &llon, x.i, x.j, nmap.xorig, nmap.yorig, nmap.dx, nmap.dy, 
              nmap.slat, nmap.slon, 1.0, nmap.rearth, nmap.eccen2); 

       y.i = (int) (llon / fabs(outmap.dlon) + 0.5) ; 
       if (y.i < 0. ) y.i += (int) (0.5 + 360./fabs(outmap.dlon) );
       y.j = (int) ((90. - llat) / fabs(outmap.dlat)  - 0.5);
       if ( nmap[x]*scalen < (float) MAXICE && (int) nland[x] < MAXICE ) {
         count[y] += 1;
         outmap[y] += min(100, nmap[x]*scalen);
       }
//       if ( nmap[x]*scalen == WEATHER ) {
//         count[y] += 1;
//         outmap[y] += 0;
//       }
     }
   }

  for (x.j = 0; x.j < smap.ypoints() ; x.j++) {
    for (x.i = 0; x.i < smap.xpoints() ; x.i++) {
       mapxy(&llat, &llon, x.i, x.j, smap.xorig, smap.yorig, smap.dx, smap.dy, 
              smap.slat, smap.slon, -1.0, smap.rearth, smap.eccen2); 

       y.i = (int) (llon / fabs(outmap.dlon) + 0.5) ; 
       if (y.i < 0. ) y.i += (int) (0.5 + 360./fabs(outmap.dlon) );
       y.j = (int) ((90. - llat) / fabs(outmap.dlat)  - 0.5);
       if ( smap[x]*scales < (float) MAXICE && (int) sland[x] < MAXICE ) {
         count[y] += 1;
         outmap[y] += min(100, smap[x]*scales);
       }
//       if ( smap[x]*scales == WEATHER ) {
//         count[y] += 1;
//         outmap[y] += 0;
//       }
     }
   }
 

/* Verify that all points have a concentration (they won't, due to
   the remapping ps - ll) */
  for (y.j = 0; y.j < outmap.ypoints() ; y.j++ ) {
    for ( y.i = 0; y.i < outmap.xpoints() ; y.i++ ) {
 
      loc = outmap.locate(y);
      llat = loc.lat;
      llon = loc.lon;

      if (count[y] != 0 ) { 
        outmap[y] = outmap[y] / count [y] ;
      }
      else {
        if (llat > 0.) {
          xorig = nmap.xorig;
          yorig = nmap.yorig;
          mapll(llat, llon, &x.i, &x.j, xorig, yorig, nmap.eccen2, 
            nmap.slat, nmap.slon, nmap.rearth, nmap.dx, nmap.dy, 1.0);
          if (x.i > 0 && x.i < nmap.xpoints()  && x.j > 0 && x.j < nmap.ypoints() ) {
            tmp = (int) (0.5 + nmap[x]*scalen);
            if (tmp < MAXICE) {
              outmap[y] = min(tmp, 100);
            }
            else if (tmp == WEATHER) {
              outmap[y] = 0;
            }
            else {
              outmap[y] = tmp;
            }
          } /* End of if on north grid */
        } /* End of if in northern hemisphere */
        
  
        else {
          xorig = smap.xorig;
          yorig = smap.yorig;
          mapll(llat, llon, &x.i, &x.j, xorig, yorig, smap.eccen2, 
            smap.slat, smap.slon, smap.rearth, smap.dx, smap.dy, -1.0);
          if (x.i > 0 && x.i < smap.xpoints()  && x.j > 0 && x.j < smap.ypoints() ) {
            tmp = (int) (0.5 + smap[x]*scales);
            if (tmp < MAXICE) {
              outmap[y] = min(tmp, 100);
            }
            else if (tmp == WEATHER) {
              outmap[y] = 0;
            }
            else {
              outmap[y] = tmp;
            }
          } /* end of if on southern map */
        } /* End of hemisphere selection */
      } /* End of filling in at a missing point */
   
    } /* End of for y.i */
  } /* End of for y.j */

//Ensure that there are no points with less than the minimum ice concentrations
  for (y.j = 0; y.j < outmap.ypoints() ; y.j++ ) {
    for ( y.i = 0; y.i < outmap.xpoints() ; y.i++ ) {
       if (outmap[y] < MINICE) { outmap[y] = 0; }
    }
  } 

///////////////////////////////////////////////////////////////////////////
/* Begin SST filtering: Note that while the gribbed fields are in K, the
   operational OISST is in C. */
#ifdef OPERATIONS
  j = DEGRIB(&sst.xpoints() , &sst.ypoints() , sst.grid);
  if (j != 0) { fprintf(stderr,"Failed to degrib sst file \n"); return j; }
#else
  in12 = fopen(argv[1],"r");
  if (in12 == (FILE *) NULL) { printf("failed to open - global sst file\n");}
  #ifdef VERBOSE
    printf("about to read in sst\n"); fflush(stdout);
  #endif
  sst.binin(in12);
  if (sst.average() == 0.) {
    printf("Null sst field\n"); fflush(stdout);
  }
#endif
  xrat = (int) (0.5 + fabs(sst.dlon) / fabs(outmap.dlon) ) ;
  yrat = (int) (0.5 + fabs(sst.dlat) / fabs(outmap.dlat) ) ;
  filt_temp = (float) atof(argv[8]);
  #ifdef VERBOSE
    printf("Filtering temperature is %f\n",filt_temp);
  #endif 
        for (y.j = 0; y.j < outmap.ypoints() ; y.j++) {
          for (y.i = 0; y.i < outmap.xpoints()  ; y.i++) {
            x.i = y.i / xrat;
            x.j = y.j / yrat;
            if (sst[x ] >= filt_temp && outmap[y] > 0.0 ) {
              #ifdef VERBOSE
                printf("zeroing %d %d %f %f\n" ,y.i, y.j, sst[y], 
                                                        outmap[y] );
              #endif
              outmap[y] = 0.0; 
            }
            cout2[y] = (unsigned char) (0.5 + outmap[y]) ; 
          }
        }
/* end SST filtering */

/* Now do land mask filtering */
      for (y.j = 0; y.j < outmap.ypoints() ; y.j++ ) {
      for (y.i = 0; y.i < outmap.xpoints() ; y.i++ ) {
        if (gmap[y] == LAND || gmap[y] == COAST) {
           cout2[y] = (unsigned char) 0;
        }
        if ((int) gmap[y] > MAXICE || outmap[y] > (float) MAXICE) {
           outmap[y] = 0.0;
        }
      }
      }

/* Write out the filtered global grid */
     cout2.binout(out13);
     printf("filt Ice area e6 km2 %f\n",outmap.integrate()/1.e12/100. );

/* Place sst filtering of polar stereo grids here */
     #ifdef VERBOSE
       printf("Finished the grid filling \n");
     #endif

     return 0;

}

