#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "icessmi.h"
#include "old.h"

#undef OPERATIONS
#ifdef OPERATIONS
  extern int DEGRIB(int *nx, int *ny, float *sst);
#endif
void mapxy(float *alat, float *along, int i, int j, const float xorig,
            const float yorig, const float dx, const float dy,
            const float slat, const float slon, const float sgn,
            const float rearth, const float eccen2);
unsigned char min(unsigned char x, unsigned char y) ;

/* Filter the global ice concentration grid according to the SST and
    to the land mask */
/* Note that hooks are in place for filtering the hemispheric ice grids,
    though this isn't done currently. */
/* Robert Grumbine 4 June 1997 */

  const int nnx = NX_NORTH;
  const int nny = NY_NORTH;
  const int snx = NX_SOUTH;
  const int sny = NY_SOUTH;

  const float slat   =   slat_NORTH;
  const float npolei   = polei_NORTH;
  const float npolej   = polej_NORTH;
  const float spolei   = polei_SOUTH;
  const float spolej   = polej_SOUTH;
  const float nslon    = slon_NORTH;
  const float sslon    = slon_SOUTH;

const float indx = 1.0;
const float indy = 1.0;
const float outdx = 0.5;
const float outdy = 0.5;
#define innx 360
#define inny 180
#define outnx 720
#define outny 360

int main(int argc, char *argv[])
{
  FILE *in10, *in11, *in12, *out13, *out14, *out15, *in16;

  float count[outny][outnx], outmap[outny][outnx];
  float sst[inny][innx], errs[inny][innx];
  int xrat, yrat;
  unsigned char nmap[NY_NORTH][NX_NORTH], smap[NY_SOUTH][NX_SOUTH];
  unsigned char cout2[outny][outnx];
  unsigned char gmap[outny][outnx];

  int i, j, yy, mm, dd, k;
  int tmp, lli, llj;
  float llat, llon, xcor, ycor, xorig, yorig, filt_temp;

  xrat = indx / outdx;
  yrat = indy / outdy;

  for (llj = 0; llj < outny; llj++ ) {
    for (lli = 0; lli < outnx; lli++ ) {
       outmap[llj][lli] = 0.0;
       count[llj][lli] = 0.0;
    }
  }

  in10 = fopen(argv[2],"r");
  in11 = fopen(argv[3],"r");
  in16 = fopen(argv[7],"r");
  if (in10 == NULL || in11 == NULL || in16 == NULL) {
    printf("Failed to open a required input file in filt\n");
    if (in10 == NULL) { printf(" - northern hemisphere file\n");}
    if (in11 == NULL) { printf(" - southern hemisphere file\n");}
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

  fread(nmap, sizeof(unsigned char), nnx*nny, in10);
  fread(smap, sizeof(unsigned char), snx*sny, in11);
#ifdef OPERATIONS
  j = DEGRIB(&innx, &inny, &sst[0][0]);
  if (j != 0) { fprintf(stderr,"Failed to degrib sst file \n"); return j; }
#else
  in12 = fopen(argv[1],"r");
  if (in12 == NULL) { printf("failed to open - global sst file\n");}
  fread(sst,  sizeof(float),         innx*inny,   in12);
/*  fread(errs, sizeof(float),         innx*inny,   in12);
    Remove for working with reanalysis archive */
#endif
  fread(gmap, sizeof(unsigned char), outnx*outny, in16);

/* Now average from polar stereographic on to the lat-long grid */
  for (j = 0; j < nny; j++) {
    for (i = 0; i < nnx; i++) {
       mapxy(&llat, &llon, i, j, -npolei*dx, -npolej*dy, dx, dy, 
              slat, nslon, 1.0, rearth, eccen2); 

       lli = llon / outdx + 0.5; 
       if (lli < 0. ) lli += 360./outdx;
       llj = (90. - llat) / outdy  - 0.5;
       if ( nmap[j][i] < 126 ) {
         count[llj][lli] += 1;
         outmap[llj][lli] += min(100, nmap[j][i]);
       }
       if ( nmap[j][i] == WEATHER ) {
         count[llj][lli] += 1;
         outmap[llj][lli] += 0;
       }
     }
   }

  for (j = 0; j < sny; j++) {
    for (i = 0; i < snx; i++) {
       mapxy(&llat, &llon, i, j, -spolei*dx, -spolej*dy, dx, dy, 
              slat, sslon, -1.0, rearth, eccen2); 

       lli = llon / outdx + 0.5; 
       if (lli < 0. ) lli += 360./outdx;
       llj = (90. - llat) / outdy  - 0.5;
       if ( smap[j][i] < 126 ) {
         count[llj][lli] += 1;
         outmap[llj][lli] += min(100, smap[j][i]);
       }
       if ( smap[j][i] == WEATHER ) {
         count[llj][lli] += 1;
         outmap[llj][lli] += 0;
       }
     }
   }
 

/* Verify that all points have a concentration (they won't, due to
   the remapping ps - ll) */
  for (llj = 0; llj < outny; llj++ ) {

    llat = 90. - (llj +1. - 0.5)*outdy ; 
      
    for ( lli = 0; lli < outnx; lli++ ) {
 
      llon = (lli + 1 - 0.5) * outdx ; 

      if (count[llj][lli] != 0 ) { 
        outmap[llj][lli] = outmap[llj][lli] / count [llj][lli] ;
      }
      else {
        if (llat > 0.) {
          xorig = -npolei*dx;
          yorig = -npolej*dy;
          mapll(llat, llon, &i, &j, xorig, yorig, eccen2, 
            slat, nslon, rearth, dx, dy, 1.0);
          if (i > 0 && i < nnx && j > 0 && j < nny) {
            tmp = nmap[j][i];
            if (tmp < 129) {
              outmap[llj][lli] = min(tmp, 100);
            }
            else if (tmp == WEATHER) {
              outmap[llj][lli] = 0;
            }
            else {
              outmap[llj][lli] = tmp;
            }
          } /* End of if on north grid */
        } /* End of if in northern hemisphere */
        
  
        else {
          xorig = -spolei*dx;
          yorig = -spolej*dy;
          mapll(llat, llon, &i, &j, xorig, yorig, eccen2, slat, sslon, 
                  rearth, dx, dy, -1.0);
          if (i > 0 && i < snx && j > 0 && j < sny) {
            tmp = (smap[j][i] );
            if (tmp < 129) {
              outmap[llj][lli] = min(tmp, 100);
            }
            else if (tmp == WEATHER) {
              outmap[llj][lli] = 0;
            }
            else {
              outmap[llj][lli] = tmp;
            }
          } /* end of if on southern map */
        } /* End of hemisphere selection */
      } /* End of filling in at a missing point */
   
    } /* End of for lli */
  } /* End of for llj */


/* Begin SST filtering: Note that while the gribbed fields are in K, the
   operational OISST is in C.
   Only change if there is a fair degreeof confidence in the SST,
   confidence being ranged from 0 to 1 with 0 being best.  */
        for (llj = 0; llj < outny; llj++) {
          for (lli = 0; lli < outnx ; lli++) {
            if (sst[llj/yrat][lli/xrat] >= filt_temp && 
/*                errs[llj/yrat][lli/xrat] <= 0.5 && Remove for reanalysis*/
                outmap[llj][lli] > 0.0 ) {
/*              printf("zeroing %d %d %f %f\n" ,lli, llj, sst[llj][lli], 
                                                        outmap[llj][lli] ); */ 
              outmap[llj][lli] = 0.0; 
            }
            cout2[llj][lli] = outmap[llj][lli]; 
          }
        }
/* end SST filtering */

/* Now do land mask filtering */
      for (llj = 0; llj < outny; llj++ ) {
         for (lli = 0; lli < outnx; lli++ ) {
            if (gmap[llj][lli] == LAND) cout2[llj][lli] = (unsigned char) 0;
         }
      }

/* Write out the filtered global grid */
     fwrite(cout2, sizeof(unsigned char), outnx*outny, out13);

/* Place sst filtering of polar stereo grids here */

     return 0;

}
#include <math.h> 


void fmapll(const float lat, const float lon, float *i, float *j, const float xorig,
           const float yorig, const float eccen2, const float slat, 
           const float slon, const float rearth, const float dx, 
           const float dy, const float sgn)
{
/* Convert from geodetic latitude and longitude to polar stereographic
   grid coordinates.  Follows mapll by V. J. Troisi.         */
/* Conventions include that slat and lat must be absolute values */
/* The hemispheres are controlled by the sgn parameter */
/* Robert Grumbine 15 April 1994. */
/* Last Modified 9 May 1994. */
/* Error noted in NSIDC bulletin fixed 11 December 1996 */
/* Change over to using tlat and tslat locally to ensure that absolute 
     values are used  4 June 1997 */

   const float pi        = 3.141592654;
   float cdr, alat, along, e, e2;
   float tlat, tslat;

   float t, x, y, rho, sl, tc, mc;

   tlat  = fabs(lat);
   tslat = fabs(slat);
   cdr   = 180./pi;
   alat  = tlat/cdr;
   along = lon/cdr;
   e2 = eccen2;
   e  = sqrt(eccen2);

   if ( tlat > 90.)  {
	 *i = -1;
	 *j = -1;
	 return;
   }
   else {
	 t = tan(pi/4. - alat/2.) /
	   pow( (1.-e*sin(alat))/(1.+e*sin(alat)) , e/2.);

	 if ( fabs(90. - tslat) < 1.E-3) {
	   rho = 2.*rearth*t/
		 pow( pow(1.+e,1.+e) * pow(1.-e,1.-e) , 1./2.);
	 }
	 else {
	   sl = tslat/cdr;
	   tc = tan(pi/4.-sl/2.) /
		 pow( (1.-e*sin(sl))/(1.+e*sin(sl)), (e/2.) );
	   mc = cos(sl)/ sqrt(1.-e2*sin(sl)*sin(sl) );
	   rho = rearth * mc*t/tc;
	 }

	 x = rho*sgn*cos(sgn*(along+slon/cdr));
	 y = rho*sgn*sin(sgn*(along+slon/cdr));

	 *i = ((x - xorig)/dx);
	 *j = ((y - yorig)/dy);

	 return;
   }

}
void mapll(const float lat, const float lon, int *i, int *j, const float xorig,
           const float yorig, const float eccen2, const float slat, 
           const float slon, const float rearth, const float dx, 
           const float dy, const float sgn)
{
/* Convert from geodetic latitude and longitude to polar stereographic
   grid coordinates.  Follows mapll by V. J. Troisi.         */
/* Conventions include that slat and lat must be absolute values */
/* The hemispheres are controlled by the sgn parameter */
/* Robert Grumbine 15 April 1994. */
/* Last Modified 9 May 1994. */

   const float pi        = 3.141592654;
   float cdr, alat, along, e, e2;
   float tlat, tslat;

   float t, x, y, rho, sl, tc, mc;

   tlat  = fabs(lat);
   tslat = fabs(slat);
   cdr   = 180./pi;
   alat  = tlat/cdr;
   along = lon/cdr;
   e2 = eccen2;
   e  = sqrt(eccen2);

   if ( tlat > 90.)  {
	 *i = -1;
	 *j = -1;
	 return;
   }
   else {
	 t = tan(pi/4. - alat/2.) /
	   pow( (1.-e*sin(alat))/(1.+e*sin(alat)) , e/2.);

	 if ( fabs(90. - tslat) < 1.E-3) {
	   rho = 2.*rearth*t/
/*		 pow( pow(1.+e,1.+e) * pow(1.-e,1.-e) , e/2.); 
            Bug fix 11 December 1996 */
		 pow( pow(1.+e,1.+e) * pow(1.-e,1.-e) , 1./2.);
	 }
	 else {
	   sl = tslat/cdr;
	   tc = tan(pi/4.-sl/2.) /
		 pow( (1.-e*sin(sl))/(1.+e*sin(sl)), (e/2.) );
	   mc = cos(sl)/ sqrt(1.-e2*sin(sl)*sin(sl) );
	   rho = rearth * mc*t/tc;
	 }

	 x = rho*sgn*cos(sgn*(along+slon/cdr));
	 y = rho*sgn*sin(sgn*(along+slon/cdr));

	 *i = (int) ((x - xorig)/dx+0.5);
	 *j = (int) ((y - yorig)/dy+0.5);

	 return;
   }

}
#include <math.h>

void mapxy(float *alat, float *along, int i, int j, const float xorig,
            const float yorig, const float dx, const float dy,
            const float slat, const float slon, const float sgn,
            const float rearth, const float eccen2)
{
/* After V. Troisi program.  Given grid coordinates, compute latitude
  and longitude on a polar stereographic grid */
/* Bob Grumbine */
/* Last Modified 3 June 1994. */


  const float cdr = 57.29577951;
  const float pi  = 3.141592654;
  float e, e2;

  float x, y, sl, rho, cm, t, chi;

/*  printf("%3d %3d %8.1f %8.1f %8.1f %8.1f %5.1f %5.1f %4.1f %f %f \n",
    i, j, xorig, yorig, dx, dy, slat, slon, sgn, rearth, eccen2); */

  x = i * dx + xorig;
  y = j * dy + yorig;

  sl = slat / cdr;
  rho = sqrt(x*x+y*y);
  e   = sqrt(eccen2);
  e2  = eccen2;
  
  if (rho <= 0.1) {
	*alat  = 90. * sgn;
	*along = 0.0;
	return;
  }

  cm = cos(sl)/ sqrt(1.0-e2*sin(sl)*sin(sl) );

  t  = tan(pi/4. - sl/2.) /
	   pow(  ((1.0 - e*sin(sl))/(1.0+e*sin(sl))), e/2.);

  if ( fabs(slat - 90) < 1.e-5 ) {
	t = rho*sqrt( pow(1.+e, 1.+e) * pow(1.-e, 1.-e) ) / 2. / rearth;
  }
  else {
	t = rho * t / (rearth * cm);
  }
  
  chi = (pi/2.) - 2.*atan(t);
  *alat = chi +
		 ( e2/2. + 5.*e2*e2/24. + e2*e2*e2/12.) * sin(2.*chi) +
		 ( 7.*e2*e2/48. + 29.*e2*e2*e2/240.)    * sin(4.*chi) +
		 ( 7.*e2*e2*e2/120.                )    * sin(6.*chi) ;

  *alat = sgn* (*alat) *cdr;
  *along = sgn*atan2(sgn*y, sgn*x)*cdr - slon;

  return;
}

unsigned char min(unsigned char x, unsigned char y) {
  if (x < y) {
    return x;
  }
  else {
    return y;
  }
  return x;
}
