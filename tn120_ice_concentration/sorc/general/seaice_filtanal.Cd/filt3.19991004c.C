#include <stdio.h>
#include <stdlib.h>

#include "icessmi.new.h"
#include "ncepgrids.h"
#define MAX_ICE 128
extern "C" void mapll(const float lat, const float lon, int *ilat, int *ilon,
           const float xorig, const float yorig, const float eccen2,
           const float slat, const float slon, const float rearth,
           const float dx, const float dy, const float sgn);
                     

/* Filter the global ice concentration grid according to the SST and
    to the land mask */
/* Note that hooks are in place for filtering the hemispheric ice grids,
    though this isn't done currently. */
/* Robert Grumbine 4 June 1997 */
/* Major revision: change to C++, go to wgrib decoding of sst files
   Robert Grumbine 8 September 1999 */

int main(int argc, char *argv[])
{
  FILE *in10, *in11, *in12, *out13, *out14, *out15, *in16;
  FILE *fin;

  northgrid<unsigned char> nmap;
  southgrid<unsigned char> smap;
  global_ice<unsigned char> cout2, gmap, refmap;
  global_ice<float> count, outmap;
  global_sst<float> sst, errs; 

  ijpt ijloc, destloc, ijloc2;
  fijpt tloc;
  latpt iceloc, outloc;
  unsigned char tmp;
  float filt_temp;

  outmap.set(0.0);
  count.set(0.0);

  in12 = fopen(argv[1],"r");
  in10 = fopen(argv[2],"r");
  in11 = fopen(argv[3],"r");
  in16 = fopen(argv[7],"r");
  if (in10 == NULL || in11 == NULL || in12 == NULL || in16 == NULL) {
    printf("Failed to open a required input file in filt\n");
    if (in10 == NULL) { printf(" - northern hemisphere file\n");}
    if (in11 == NULL) { printf(" - southern hemisphere file\n");}
    if (in12 == NULL) { printf(" - global sst file\n");}
    if (in16 == NULL) { printf(" - global land mask file\n");}
    return -1;
  }

  out13 = fopen(argv[4],"w");
  out14 = fopen(argv[5],"w");
  out15 = fopen(argv[6],"w");
  if (out13 == NULL || out14 == NULL || out15 == NULL) {
    printf("Failed to open a required output file in filt\n");
    if (out13 == NULL) { printf(" - global ice file\n");}
    if (out14 == NULL) { printf(" - n. hemisphere ice file\n");}
    if (out15 == NULL) { printf(" - s. hemisphere ice file\n");}
    return -1;
  }
    

  filt_temp = (float) atof(argv[8]);

  nmap.binin(in10);
  smap.binin(in11);
  sst.binin(in12);
  gmap.binin(in16);

/* Now average from polar stereographic on to the lat-long grid */
/* RG 8 Sept 1999: In C++, this can now be made a trivial function passing
    a metricgrid in, arbitrary mapping and origin */

  for (ijloc.j = 0; ijloc.j < nmap.ypoints(); ijloc.j++) {
    #ifdef ORIG
      int lli, llj;
      float llat, llon;
      float dx = 25.4e3, dy = 25.4e3, slat = 60.0, slon = -10.0, sgn = 1.;
      float rearth = 6378.160e3 , eccen2 = 0.006694604 ;
      float xorig =  (-38.*5* dx ), yorig = (-46.*5* dy );
      float outdx = 0.5, outdy = 0.5; 
    #endif
    for (ijloc.i = 0; ijloc.i < nmap.xpoints(); ijloc.i++) {
       iceloc = nmap.locate(ijloc); // iceloc = latlon
       tloc = outmap.locate(iceloc); //destloc = ij
       destloc = tloc;
       #ifdef ORIG
         mapxy(&llat, &llon, ijloc.i, ijloc.j, xorig, yorig, dx, dy, 
                slat, slon, sgn, rearth, eccen2);
         lli = (int) (llon / outdx + 0.5);
         if (lli < 0. ) lli += (int) (360./outdx);
         llj = (int) ((90. - llat) / outdy  - 0.5);
         //FIXEDif (llon < 0.) llon += 360.;
         //FIXED lli = (int) ( (llon - 0.25) / outdx + 0.5) ; 
         //FIXED llj = (int) ( (89.75 - llat)/outdy + 0.5) ;
         if (lli != destloc.i || llj != destloc.j) {
         printf("orig %3d %3d new %3d %3d  %3d\n",
              lli, llj, destloc.i, destloc.j, nmap[ijloc]);
         //printf("lat-long orig %f %f  new %f %f\n",llat, llon, \
         // iceloc.lat, iceloc.lon);
         }
         destloc.i = lli;
         destloc.j = llj;
       #endif 
       if (nmap[ijloc] > (unsigned char) MAX_ICE && 
           nmap[ijloc] < (unsigned char) LAND) {
          printf("Erroneous ice concentration in nh file");
          printf(" %3d %3d  %3d\n",ijloc.i, ijloc.j, nmap[ijloc]);
       }
       //NEW if ( nmap[ijloc]  <= (unsigned char) MAX_ICE ) {
       if ( nmap[ijloc]  < (unsigned char) 126 ) {
         count[destloc] += 1;
         outmap[destloc] += min((unsigned char)100, nmap[ijloc]);
       }
       #ifdef ORIG
       if ( nmap[ijloc] == (unsigned char) WEATHER ) {
         count[destloc]  += 1;
       #else
       if ( nmap[ijloc] == (unsigned char) WEATHER || 
            nmap[ijloc] == (unsigned char) BAD_DATA ) {
       #endif
         outmap[destloc] += 0;
       }
     }
   }

  for (ijloc.j = 0; ijloc.j < smap.ypoints(); ijloc.j++) {
    #ifdef ORIG
      int lli, llj;
      float llat, llon;
      float dx = 25.4e3, dy = 25.4e3, slat = 60.0, slon = 170.0, sgn = -1.;
      float rearth = 6378.160e3 , eccen2 = 0.006694604 ;
      float xorig =  (-30.*5* dx ), yorig = (-36.*5* dy );
      float outdx = 0.5, outdy = 0.5;
    #endif
    for (ijloc.i = 0; ijloc.i < smap.xpoints(); ijloc.i++) {
       iceloc = smap.locate(ijloc);
       tloc = outmap.locate(iceloc); //this rather unaesthetic business
       destloc = tloc;  // is to make the conversion from fijpt to ijpt return
       #ifdef ORIG
         mapxy(&llat, &llon, ijloc.i, ijloc.j, xorig, yorig, dx, dy,
                slat, slon, sgn, rearth, eccen2);
         lli = (int) (llon / outdx + 0.5);
         if (lli < 0. ) lli += (int) (360./outdx);
         llj = (int) ((90. - llat) / outdy  - 0.5);
         //FIXEDif (llon < 0.) llon += 360.;
         //FIXED lli = (int) ( (llon - 0.25) / outdx + 0.5) ;
         //FIXED llj = (int) ( (89.75 - llat)/outdy + 0.5) ;
         if (lli != destloc.i || llj != destloc.j) {
         printf("orig %3d %3d new %3d %3d  %3d\n",
              lli, llj, destloc.i, destloc.j, smap[ijloc]);
         //printf("lat-long orig %f %f  new %f %f\n",llat, llon, \
         // iceloc.lat, iceloc.lon);
         }
         destloc.i = lli;
         destloc.j = llj;
       #endif

       if (smap[ijloc] > (unsigned char) MAX_ICE &&
           smap[ijloc] < (unsigned char) LAND) {
          printf("Erroneous ice concentration in sh file");
          printf(" %3d %3d  %3d\n",ijloc.i, ijloc.j, smap[ijloc]);
       }
       if ( smap[ijloc] <= (unsigned char) MAX_ICE ) {
         count[destloc] += 1;
         outmap[destloc] += min((unsigned char)100, smap[ijloc]);
       }
       #ifdef ORIG
       if ( smap[ijloc] == (unsigned char) WEATHER ) {
         count[destloc]  += 1;
       #else
       if ( smap[ijloc] == (unsigned char) WEATHER || 
            smap[ijloc] == (unsigned char) BAD_DATA ) {
       #endif
         outmap[destloc] += 0;
       }
     }
   }
 

/* Verify that all points have a concentration (they won't, due to
   the remapping ps - ll) */
  for (ijloc.j = 0; ijloc.j < outmap.ypoints(); ijloc.j++ ) {
    #ifdef ORIG
      int lli, llj, i, j;
      float llat, llon;
      float dx = 25.4e3, dy = 25.4e3, slat = 60.0, slon = -10.0, sgn = 1.;
      float rearth = 6378.160e3 , eccen2 = 0.006694604 ;
      float xorig =  (-38.*5* dx ), yorig = (-46.*5* dy );
      float outdx = 0.5, outdy = 0.5;
    #endif
    for ( ijloc.i = 0; ijloc.i < outmap.xpoints(); ijloc.i++ ) {
      outloc = outmap.locate(ijloc); // Location on the output grid 
      #ifdef ORIG
        llat = 90. - (ijloc.j +1. - 0.5)*outdy ;   
        llon = (ijloc.i + 1 - 0.5) * outdx ;  
        mapll(llat, llon, &i, &j, xorig, yorig, eccen2,
          slat, slon, rearth, dx, dy, 1.0);  
      #endif
      if (count[ijloc] != 0 ) { 
        outmap[ijloc] = outmap[ijloc] / count [ijloc] ;
      }
      else {
        if (outloc.lat > 0.) {
          tloc = nmap.locate(outloc);
          ijloc2 = tloc;
          #ifdef ORIG
            if (ijloc2.j != j || ijloc2.i != i || outloc.lat != llat || outloc.lon != llon) {
              printf("old %3d %3d %f %f new %3d %3d %f %f\n",i, j, llat, llon, ijloc2.i, ijloc2.j, outloc.lat, outloc.lon);
            }
          #endif
          if (nmap.in(ijloc2) ) {
            tmp = nmap[ijloc2];
            //NEW if (tmp <= (unsigned char) MAX_ICE) {
            if (tmp < 129) {
              outmap[ijloc] = min(tmp, (unsigned char) 100);
            }
            #ifdef ORIG
            else if (tmp == (unsigned char) WEATHER) {
              outmap[ijloc] = 0;
            }
            #endif
            else {
              outmap[ijloc] = tmp;
            }
          } /* End of if on north grid */
        } /* End of if in northern hemisphere */
        
  
        else {
          tloc = smap.locate(outloc);
          ijloc2 = tloc;
          if (smap.in(ijloc2) ) {
            tmp = smap[ijloc2] ;
            if (tmp <= (unsigned char) MAX_ICE) {
              outmap[ijloc] = min(tmp, (unsigned char) 100);
            }
            #ifdef ORIG
            else if (tmp == (unsigned char) WEATHER) {
              outmap[ijloc] = 0;
            }
            #endif
            else {
              outmap[ijloc] = tmp;
            }
          } /* end of if on southern map */
        } /* End of hemisphere selection */
      } /* End of filling in at a missing point */
   
    } /* End of for ijloc.i */
  } /* End of for ijloc.j */


/* Begin SST filtering: Note that while the gribbed fields are in K, the
   operational OISST is in C.
   Only change if there is a fair degreeof confidence in the SST,
   confidence being ranged from 0 to 1 with 0 being best.  */
        for (ijloc.j = 0; ijloc.j < outmap.ypoints(); ijloc.j++) {
          for (ijloc.i = 0; ijloc.i < outmap.xpoints() ; ijloc.i++) {
            outloc = outmap.locate(ijloc);
            tloc = sst.locate(outloc);
            ijloc2 = tloc;
            if (sst[ijloc2] >= filt_temp && 
                outmap[ijloc] > 0.0 ) {
                #ifdef VERBOSE
                  printf("zeroing %d %d %f %f\n" ,ijloc.i, ijloc.j, sst[ijloc2],
                                                        outmap[ijloc] );
                #endif
              outmap[ijloc] = (unsigned char) 0; 
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

// Now compare to reference map
     fin = fopen("latlon.990908","r");
     refmap.binin(fin);
     fclose (fin);
     for (ijloc.j = 0; ijloc.j < cout2.ypoints() ; ijloc.j++) {
     for (ijloc.i = 0; ijloc.i < cout2.xpoints() ; ijloc.i++) {
       if ( (fabs(cout2[ijloc] - refmap[ijloc]) > 1 ) && 
              gmap[ijloc] != (unsigned char) LAND) {
        printf("%3d %3d old %3d new %3d land %3d count %2.0f\n", ijloc.i, ijloc.j,
          refmap[ijloc], cout2[ijloc], gmap[ijloc], count[ijloc] );
       }
     }
     }
     ijloc.i = nmap.xpoints() / 2;
     for (ijloc.j = 0; ijloc.j < nmap.ypoints() ; ijloc.j++) {
       printf("%3d %d\n",ijloc.j, nmap[ijloc]);
     }
     
/* Place sst filtering of polar stereo grids here */

     return 0;

}
