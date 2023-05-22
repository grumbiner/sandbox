#include <stdio.h>
#include <stdlib.h>

#include "ncepgrids.h"
#define LAND 157
#define WEATHER 177
#define COAST 195
#define MAXICE 127

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
  FILE *in10, *in11, *out13, *out14, *out15, *in16, *in17, *in18;

  global_high<float> count, outmap;
  global_sst<float> sst, errs;
  northgrid<float> nmap;
  northgrid<unsigned char> nland;
  southgrid<float> smap; 
  southgrid<unsigned char> sland;
  global_high<unsigned char> cout2;
  global_high<unsigned char> gmap;

  int tmp, xrat, yrat;
  float llat, llon, filt_temp;
  ijpt x, y ;
  fijpt fx;
  latpt loc, lloc;
  float scalen, scales;

  xrat = (int) (0.5 + fabs(sst.dlon) / fabs(outmap.dlon) ) ;
  yrat = (int) (0.5 + fabs(sst.dlat) / fabs(outmap.dlat) ) ;

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
    

  filt_temp = (float) atof(argv[8]);

  nland.binin(in17);
  // nmap.binin(in10);
  nmap.set(100);
  scalen = nmap.gridmax();
  if (scalen >= 100. ) {
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
  smap.set(100);
  scales = smap.gridmax();
  if (scales >= 100. ) {
    scales = 1.;
  }
  else if (scales < 2.6) {
    scales = 100.;
  }
  else {
    printf("Erroneous data range in smap %f is maximum \n",scales);
  }

  sst.set(0.0);
  #ifdef VERBOSE
    printf("about to read in land mask\n"); fflush(stdout);
  #endif
  gmap.binin(in16);

/* Now average from polar stereographic on to the lat-long grid */
  for (x.j = 0; x.j < nmap.ypoints() ; x.j++) {
    for (x.i = 0; x.i < nmap.xpoints() ; x.i++) {
       lloc = nmap.locate(x);
       llat = lloc.lat;
       llon = lloc.lon;

       y.i = (int) (llon / fabs(outmap.dlon) + 0.5) ; 
       if (y.i < 0. ) y.i += (int) (0.5 + 360./fabs(outmap.dlon) );
       y.j = (int) (0.5 + (90. - llat) / fabs(outmap.dlat)  - 0.5);
       if ( nmap[x]*scalen < MAXICE && nland[x] < MAXICE ) {
         count[y] += 1;
         outmap[y] += min(100, (int) (0.5 + nmap[x]*scalen) );
       }
     }
   }

  for (x.j = 0; x.j < smap.ypoints() ; x.j++) {
    for (x.i = 0; x.i < smap.xpoints() ; x.i++) {
       lloc = smap.locate(x);
       llat = lloc.lat;
       llon = lloc.lon;

       y.i = (int) (llon / fabs(outmap.dlon) + 0.5) ; 
       if (y.i < 0. ) y.i += (int) (0.5 + 360./fabs(outmap.dlon) );
       y.j = (int) (0.5 + (90. - llat) / fabs(outmap.dlat)  - 0.5 );
       if ( smap[x]*scales < MAXICE && sland[x] < MAXICE ) {
         count[y] += 1;
         outmap[y] += min(100, (int) (0.5 + smap[x]*scales) );
       }
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
          lloc.lat = llat;
          lloc.lon = llon;
          fx = nmap.locate(lloc);
          x = fx;
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
          lloc.lat = llat;
          lloc.lon = llon;
          fx = smap.locate(lloc);
          x = fx;
          if (x.i > 0 && x.i < smap.xpoints()  && x.j > 0 && x.j < smap.ypoints() ) {
            tmp = (int) (0.5 + smap[x]*scales );
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


/* Begin SST filtering: Note that while the gribbed fields are in K, the
   operational OISST is in C.
   Only change if there is a fair degreeof confidence in the SST,
   confidence being ranged from 0 to 1 with 0 being best.  */
        for (y.j = 0; y.j < outmap.ypoints() ; y.j++) {
          for (y.i = 0; y.i < outmap.xpoints()  ; y.i++) {
            x.i = y.i / xrat;
            x.j = y.j / yrat;
            if (sst[x ] >= filt_temp && 
/*                errs[x] <= 0.5 && Remove for reanalysis*/
                outmap[y] > 0.0 ) {
/*              printf("zeroing %d %d %f %f\n" ,y.i, y.j, sst[y], 
                                                        outmap[y] ); */ 
              outmap[y] = 0.0; 
            }
            cout2[y] = (unsigned char) (0.5 + outmap[y]) ; 
          }
        }
/* end SST filtering */

/* Now do land mask filtering */
      for (y.j = 0; y.j < outmap.ypoints() ; y.j++ ) {
         for (y.i = 0; y.i < outmap.xpoints() ; y.i++ ) {
            if (gmap[y] == LAND) cout2[y] = (unsigned char) 0;
         }
      }

/* Tests on the filtered grid */
/* Print out all points which are land in the global grid, and which are on
   either the northern or southern hemispheric maps, and which do not show
   up as having ice cover */
     cout2.binout(out13);
     printf("Ice area %f\n",outmap.integrate()/1.e12/100. );
     {
       palette<unsigned char> gg(19,65);
       ijpt n, s;
       fijpt fn, fs;
       for (y.j = 0; y.j < cout2.ypoints() ; y.j++) {
       for (y.i = 0; y.i < cout2.xpoints() ; y.i++) {
          loc = cout2.locate(y);
          fn = nmap.locate(loc);
          fs = smap.locate(loc);
          n = fn;
          s = fs;
          if (cout2[y] == 0 && gmap[y] != LAND && 
              (nmap.in(n) || smap.in(s) ) ) {
            printf("No ice for point %3d %3d, %6.3f %6.3f tag %d \n",
                     y.i, y.j, loc.lat, loc.lon, 
                   (int) gmap[y]);
          }
       }
       }
       for (y.j = 0; y.j < cout2.ypoints() ; y.j++) {
       for (y.i = 0; y.i < cout2.xpoints() ; y.i++) {
          if (cout2[y] == 0 && gmap[y] == LAND) {
            cout2[y] = 128;
          }
          else if (cout2[y] == 0 ) {
            cout2[y] = 64;
          }
          else {
            cout2[y] = 0;
          } 
       }
       }
       cout2.xpm("glob.xpm", 7, gg);
     }

     return 0;
}
