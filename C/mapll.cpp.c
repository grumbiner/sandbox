#include <math.h> 


void newmapll(const float lat, const float lon, float *i, float *j, const float xorig,
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

   const float pi        = 3.141592654;
   float cdr, alat, along, e, e2;

   float t, x, y, rho, sl, tc, mc;

   cdr   = 180./pi;
   alat  = lat/cdr;
   along = lon/cdr;
   e2 = eccen2;
   e  = sqrt(eccen2);

   if ( fabs(lat) > 90.)  {
	 *i = -1;
	 *j = -1;
	 return;
   }
   else {
	 t = tan(pi/4. - alat/2.) /
	   pow( (1.-e*sin(alat))/(1.+e*sin(alat)) , e/2.);

	 if ( fabs(90. - slat) < 1.E-3) {
	   rho = 2.*rearth*t/
		 pow( pow(1.+e,1.+e) * pow(1.-e,1.-e) , 1./2.);
	 }
	 else {
	   sl = slat/cdr;
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
/* Bob Grumbine 15 April 1994. */
/* Last Modified 9 May 1994. */

   const float pi        = 3.141592654;
   float cdr, alat, along, e, e2;

   float t, x, y, rho, sl, tc, mc;

   cdr   = 180./pi;
   alat  = lat/cdr;
   along = lon/cdr;
   e2 = eccen2;
   e  = sqrt(eccen2);

   if ( fabs(lat) > 90.)  {
	 *i = -1;
	 *j = -1;
	 return;
   }
   else {
	 t = tan(pi/4. - alat/2.) /
	   pow( (1.-e*sin(alat))/(1.+e*sin(alat)) , e/2.);

	 if ( fabs(90. - slat) < 1.E-3) {
	   rho = 2.*rearth*t/
		 pow( pow(1.+e,1.+e) * pow(1.-e,1.-e) , e/2.);
	 }
	 else {
	   sl = slat/cdr;
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
