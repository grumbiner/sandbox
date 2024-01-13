#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "ssmi.h"
#include "icessmi.h"

/* Perform extended weather filtering (per OMB Tech Note 120) on
     SSMI data for sea ice use */
/* Robert Grumbine 4 June 1997 */
 
float gr37(const ssmi *map, const int i, const int j, 
           const int nx, const int ny, const int range);
float gr22(const ssmi *map, const int i, const int j, 
           const int nx, const int ny, const int range);

float gr37(const ssmi *map, const int i, const int j, 
                            const int nx, const int ny, const int range)
{
   int index, ti, tj, count;
   float t19v, t37v, tempor;

   t19v = 0.0;
   t37v = 0.0;
   count = 0;
 
   if (range != 0) {
     for (tj = j-range ; tj < j+range ; tj++) {
       for (ti = i - range ; ti < i+range; ti++) {
         index = ti + tj*nx;
         if (index < 0 || index >= nx*ny) continue; 
         if (map[index].t19v != 0 && map[index].t37v != 0) {
           count += 1;
           t19v += map[index].t19v;
           t37v += map[index].t37v;
         }
       }
     }
  
     t37v = t37v / count;
     t19v = t19v / count;
   }
   else {
     index = i + j*nx;
     t19v = map[index].t19v;
     t37v = map[index].t37v;
   }

   if (t19v != 0.0 && t37v != 0.0 ) {
     tempor = (t37v - t19v) / (t37v + t19v);
   }
   else {tempor = 0.0;}
   
   return tempor;
}


float gr22(const ssmi *map, const int i, const int j, 
                            const int nx, const int ny, const int range)
{
   float t19v, t22v, tempor;
   int index, ti, tj, count;

   t19v = 0.0;
   t22v = 0.0;
   count = 0;
 
   for (tj = j-range ; tj < j+range ; tj++) {
     for (ti = i - range ; ti < i+range; ti++) {
        count += 1;
        index = i + j*nx;
        if (index < 0 || index >= nx*ny) continue; 
        t19v += map[index].t19v;
        t22v += map[index].t22v;
     }
   }
   t19v = t19v / count;
   t22v = t22v / count;

   tempor = (t22v - t19v) / (t22v + t19v);
   
   return tempor;
}
#include <stdio.h>
#include <math.h>
#include "ssmi.h"
#include "icessmi.h"
#include "icegrids.h"

int getfld(ssmi *ice, int npts, unsigned char *cfld, float *ffld, int sel)
{
/* Extract a desired field (specified by sel) from a full ssmi map
   (ice) and copy to both a character array (cfld) and a floating
   point array (ffdl) for some other routine to use.
   Tb Floats are scaled into degrees Kelvin, and have a 0.01 degree precision.
   Tb Chars are linearly rescaled according to o = (i-50)/2, where i is 
     the floating number input, and o is the output, with 2 degree precision
     starting from 50 Kelvin.
   Ice concentrations are 1% precision, floating or character.
   Robert Grumbine 11 October 1994.
*/

  int i, limit;

  limit = npts;

  switch (sel) {
  case T19V :
    for (i = 0; i < limit; i++)
    {
      ffld[i] = (float) ice[i].t19v/100.;
      cfld[i] = (unsigned char) (ffld[i] - 50.) / 2. ;
    }
    break;
  case T19H :
    for (i = 0; i < limit; i++)
    {
      ffld[i] = (float) ice[i].t19h/100.;
      cfld[i] = (unsigned char) (ffld[i] - 50.) / 2. ;
    }
    break;
  case T22V :
    for (i = 0; i < limit; i++)
    {
      ffld[i] = (float) ice[i].t22v/100.;
      cfld[i] = (unsigned char) (ffld[i] - 50.) / 2. ;
    }
    break;
  case T37V :
    for (i = 0; i < limit; i++)
    {
      ffld[i] = (float) ice[i].t37v/100.;
      cfld[i] = (unsigned char) (ffld[i] - 50.) / 2. ;
    }
    break;
  case T37H :
    for (i = 0; i < limit; i++)
    {
      ffld[i] = (float) ice[i].t37h/100.;
      cfld[i] = (unsigned char) (ffld[i] - 50.) / 2. ;
    }
    break;
  case T85V :
    for (i = 0; i < limit; i++)
    {
      ffld[i] = (float) ice[i].t85v/100.;
      cfld[i] = (unsigned char) (ffld[i] - 50.) / 2. ;
    }
    break;
  case T85H :
    for (i = 0; i < limit; i++)
    {
      ffld[i] = (float) ice[i].t85h/100.;
      cfld[i] = (unsigned char) (ffld[i] - 50.) / 2. ;
    }
    break;
  case CONC_BAR :
    for (i = 0; i < limit; i++)
    {
      ffld[i] = (float) ice[i].conc_bar/100.;
      cfld[i] = (unsigned char) ice[i].conc_bar ;
    }
    break;
  case BAR_CONC :
    for (i = 0; i < limit; i++)
    {
      ffld[i] = (float) ice[i].bar_conc/100.;
      cfld[i] = (unsigned char) ice[i].bar_conc ;
    }
    break;
  default :
    return -1;
  }

  return 0;

}
void antenna(float *t19v, float *t19h, float *t22v, float *t37v, float *t37h,
             float *t85v, float *t85h);

void abdalati(float t19v, float t19h, float t22v, float t37v, float t37h,
              float t85v, float t85h,
              float *nt19v, float *nt19h, float *nt22v, float *nt37v, float *nt37h,
              float *nt85v, float *nt85h);

float nasa_team(const float t19v, const float t19h, const float t22v, 
                const float t37v, const float t37h, 
                const float t85v, const float t85h, 
                const char pole, const int ant, const int regress)
{
/* Implementation of the NASA Team algorithm with 22 GHz weather
     filter.  C version by Robert Grumbine, based on Fortran code by
     M. Martino.  3/23/94.

C                                                                
C   THIS PROGRAM USES THE TEAM ALGORITHM TO CALCULATE THE TOTAL ICE
C   CONCENTRATION AND THE MULTIYEAR ICE CONCENTRATION.  INPUT ARE 
C   19 VERT, 19 HORZ AND 37 VERT, 37 HORZ BRIGHTNESS TEMPERATURES.
C                                                                 
C   COMPLETELY RE-WRITTEN FOR THE PC 16MAY91 M.G.MARTINO, STX CORP.
C   22V-19V GR WEATHER FILTER ADDED 22 JULY 93 BY MGM
C                                                                 
C     GR FILTER IS .05                        
C    SSMI TIE POINTS  (1=19H, 4=37V)
C                              
C   Revised to use the Abdalati regreession coefficients for F-11, 
C     with F-8 tie points.  95/06/22.
C   Revised to make antenna to brightness temperature correction 95/06/22.
*/
/* NORTH POLE TIE POINTS 08 MAR 91 F-8 */                           
      float t0n[7] = {100.8, 177.1, 00.0, 201.7, 0., 0., 0.};
      float tfn[7] = {242.8, 258.2, 00.0, 252.8, 0., 0., 0.};
      float tmn[7] = {203.9, 223.2, 00.0, 186.3, 0., 0., 0.};
/* North Pole tie points Jan 13, 1992 F-11*/
/*      float t0n[7] = { 99.8, 177.3, 00.0, 202.5, 0., 0., 0.};  */                 
/*      float tfn[7] = {239.5, 249.7, 00.0, 244.2, 0., 0., 0.}; */
/*      float tmn[7] = {202.3, 221.3, 00.0, 184.6, 0., 0., 0.}; */

/* SOUTH POLE TIE POINTS 12 FEB 91 F-8 */
      float t0s[7] = {100.3, 176.6, 00.0, 200.5, 0., 0., 0.};
      float tfs[7] = {237.8, 249.8, 00.0, 243.3, 0., 0., 0.};
      float tms[7] = {193.7, 221.6, 00.0, 190.3, 0., 0., 0.};

/* South pole tie points 22 Nov 1994, approved 14 Oct 1994 F-11 */
/*      float t0s[7] = {100.9, 177.1, 00.0, 200.4, 0., 0., 0.}; */ /* open */
/*      float tfs[7] = {236.9, 249.0, 00.0, 242.8, 0., 0., 0.}; */ /* first */
/*      float tms[7] = {193.2, 221.3, 00.0, 190.3, 0., 0., 0.}; */ /* multi */

/* tie points within function */
      float *T0, *TF, *TM;
      float DW[2], DF[2], DM[2];
      float SW[2], SF[2], SM[2];

/* Local variables */
      float gr37, gr22, polar;
      float nf19, nm19, dd19, fy, my, total;
      float a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1 ;
      int i;
      float nt19v, nt19h, nt22v, nt37v, nt37h, nt85v, nt85h;

/* Preliminary check on whether the data are useable */
       if ( (t22v + t19v) == 0. || (t19v + t19h) == 0. || (t37v + t19v)==0. )
       {
         return (float) BAD_DATA;
       }

/*
C   CALCULATE PARAMETERS FOR ICE CONCENTRATION ALGORITHM          
*/                                                                 
                           
      if (pole == 'n') {
        T0 = &t0n[0];
        TF = &tfn[0];
        TM = &tmn[0];
      }
      else if (pole == 's') {
        T0 = &t0s[0];
        TF = &tfs[0];
        TM = &tms[0];
      }
      else {
        printf("specified a pole that doesn't exist!!\n");
        return -1.;
      }  

       DW[0]=T0[1]-T0[0];
       DF[0]=TF[1]-TF[0];
       DM[0]=TM[1]-TM[0];

       SW[0]=T0[1]+T0[0];
       SF[0]=TF[1]+TF[0];
       SM[0]=TM[1]+TM[0];

       DW[1]=T0[3]-T0[1];
       DF[1]=TF[3]-TF[1];
       DM[1]=TM[3]-TM[1];

       SW[1]=T0[3]+T0[1];
       SF[1]=TF[3]+TF[1];
       SM[1]=TM[3]+TM[1];

       a1=DM[0]*DW[1]-DM[1]*DW[0];
       b1=DM[1]*SW[0]-DW[1]*SM[0];
       c1=DW[0]*SM[1]-DM[0]*SW[1];
       d1=SM[0]*SW[1]-SM[1]*SW[0]; 
/*       d1=SM[0]*SW[0]-SM[1]*SW[0]; */

       i1=DF[1]*DW[0]-DF[0]*DW[1];
       j1=DW[1]*SF[0]-DF[1]*SW[0];
       k1=SW[1]*DF[0]-DW[0]*SF[1];
       l1=SF[1]*SW[0]-SF[0]*SW[1];

       e1=DF[0]*(DM[1]-DW[1])+DW[0]*(DF[1]-DM[1])+DM[0]*(DW[1]-DF[1]); 
       f1=DF[1]*(SM[0]-SW[0])+DW[1]*(SF[0]-SM[0])+DM[1]*(SW[0]-SF[0]); 
       g1=DF[0]*(SW[1]-SM[1])+DW[0]*(SM[1]-SF[1])+DM[0]*(SF[1]-SW[1]); 
/*       g1=DF[0]*(SW[0]-SM[1])+DW[0]*(SM[1]-SF[1])+DM[0]*(SF[1]-SW[1]); */
       h1=SF[1]*(SW[0]-SM[0])+SW[1]*(SM[0]-SF[0])+SM[1]*(SF[0]-SW[0]);

/* Recompute the brightness temperatures, if needed, via the Abdalati, 
   1995 calibration of F-8 versus F-11 */
       if (regress == 1) {
         abdalati(t19v, t19h, t22v, t37v, t37h, t85v, t85h,
                  &nt19v, &nt19h, &nt22v, &nt37v, &nt37h, &nt85v, &nt85h); 
       }

/* Correct from antenna temperatures to brightness temperatures, if
   needed, following Katz codes of 3/6/95 */
       if (ant == 1) {
         antenna(&nt19v, &nt19h, &nt22v, &nt37v, &nt37h, &nt85v, &nt85h);
       }

/* Now, finally, compute the ice concentration */
       gr22  =  (nt22v - nt19v) / (nt22v + nt19v);
       polar =  (nt19v - nt19h) / (nt19v + nt19h);
       gr37  =  (nt37v - nt19v) / (nt37v + nt19v);

       total = (float) NO_DATA;
       if ( (gr37 <= (float) GR37LIM) && (gr22 <= (float) GR22LIM) ) {
           nf19=a1+b1*polar+c1*gr37+d1*polar*gr37;
           nm19=i1+j1*polar+k1*gr37+l1*polar*gr37;
           dd19=e1+f1*polar+g1*gr37+h1*polar*gr37;
           if (dd19 == 0.) {  printf("zero divisor for concentrations\n");
                              return (float) BAD_DATA; }
           fy = nf19/dd19;
           my = nm19/dd19;
           total = (my + fy)*100.;
           if (total < -20.0 ) return (float) BAD_DATA;
           if (total <   0.0 ) total = 0.; 
       }
       else {
        total = (float) WEATHER; /* Set weather filtered points to weather flag */
       }
      
      return total;
}
void antenna(float *TB19V, float *TB19H, float *TB22V, 
             float *TB37V, float *TB37H, float *TB85V, float *TB85H)
{
      float YES19V, YES19H, YES22V, YES37V, YES37H, YES85V, YES85H;

      float AP19V = 0.969, AP19H = .969, AP22V = .974;
      float AP37V = .986, AP37H = .986,  AP85V = .988, AP85H = .988;
      float BP19V = .00473, BP19H = .00415, BP22V = .0107; 
      float BP37V = .0217, BP37H = .02612, BP85V = .01383, BP85H = .01947;

      float C19V, C19H, C22V, C37V, C37H, C85V, C85H;
      float D19V, D19H, D22V, D37V, D37H, D85V, D85H;

      C19V = 1/(AP19V*(1-BP19V));
      C19H = 1/(AP19H*(1-BP19H));
      C22V = 1/(AP22V*(1-BP22V));
      C37V = 1/(AP37V*(1-BP37V));
      C37H = 1/(AP37H*(1-BP37H));
      C85V = 1/(AP85V*(1-BP85V));
      C85H = 1/(AP85H*(1-BP85H));
      D19V = C19V*BP19V;
      D19H = C19H*BP19H;
      D22V = C22V*BP22V;
      D37V = C37V*BP37V;
      D37H = C37H*BP37H;
      D85V = C85V*BP85V;
      D85H = C85H*BP85H;

        YES19V = C19V * (*TB19V) - D19V * (*TB19H);
        YES19H = C19H * (*TB19H) - D19H * (*TB19V);
        YES22V = C22V * (*TB22V) - D22V * (0.653 * (*TB19H) + 96.6);
        YES37V = C37V * (*TB37V) - D37V * (*TB37H);
        YES37H = C37H * (*TB37H) - D37H * (*TB37V);
        YES85V = C85V * (*TB85V) - D85V * (*TB85H);
        YES85H = C85H * (*TB85H) - D85H * (*TB85V);

     *TB19V = YES19V;
     *TB19H = YES19H;
     *TB22V = YES22V;
     *TB37V = YES37V;
     *TB37H = YES37H;
     *TB85V = YES85V;
     *TB85H = YES85H;

     return;
}     
void abdalati(float t19v, float t19h, float t22v, float t37v, float t37h,
              float t85v, float t85h,
              float *nt19v, float *nt19h, float *nt22v, float *nt37v, float *nt37h,
              float *nt85v, float *nt85h) 
{
/* Recompute the brightness temperatures via the Abdalati, 1995 calibration
   of F-8 versus F-11 */
       *nt19h = 1.013 * t19h + (-1.89);
       *nt19v = 1.013 * t19v + (-2.51);
       *nt22v = 1.014 * t22v + (-2.73);
       *nt37h = 1.024 * t37h + (-4.22);
       *nt37v = 1.000 * t37v + ( 0.052);
       *nt85v = t85v;
       *nt85h = t85h;

       return;
}
#include <stdio.h>
#include <stdlib.h>
#include "ssmi.h"
#include "icessmi.h"

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
