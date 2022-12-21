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
