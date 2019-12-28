#include <stdio.h>
#include <math.h>
#include "bgnh.h"
#include "nesdisnh.h"

void mapll(float lat, float lon, int *i, int *j);

void mapxy(float *lat, float *lon, int i, int j);

int main(int argc, char *argv[])
{
/* BG program to remap NESDIS ssmi information onto the bg grid */
/* Northern Hemisphere version */
/* Last Modified 2 February 1994 */
   FILE *input, *output;
   float lat, lon, con;
   int i, j, n, iloop, jloop, jnes;
   ssmi map[NES_NY][NES_NX];
   float conc[NY+1][NX+1];
   int count[NY+1][NX+1];

/* Input is name of the ssmi data file, then name of bg
	 output file */

   if (argc < 3) { printf("Need names of the arctic data file and the bg \
		output file \n");
		return -1;
   }
   input = fopen(argv[1], "r");
   output = fopen(argv[2], "w");
   if (input == NULL || output == NULL) {
	 printf("Failed to open one of the files!!\n");
	 return -1;
   }

   for (j = 0; j <= NY; j++)
   {  for (i = 0; i <= NX; i++)
	  {  count[j][i] = 0;
             conc [j][i] = 0.0;
	  }
   }


   n = fread(map, sizeof(ssmi), NES_NX*NES_NY, input);
   if (n != NES_NX*NES_NY) {
	 printf("Failure to read in the data!!\n");
	 return -1;
   }

   for (jloop = 0; jloop < NES_NY; jloop++)
   {  for ( iloop = 0; iloop < NES_NX; iloop++)
        {
/*          Must flip the J index as NESDIS uses 1 = top, rather than bottom */
            jnes = NES_NY-jloop-1; 
	    mapxy(&lat, &lon, iloop, jnes);
	    mapll(lat, lon, &i, &j);
/*          printf("%d %d lat, lon, i, j %f %f %d %d\n",
              iloop, jloop, lat, lon, i, j); */
	    if (i >= 0 && i <= NX && j >= 0 && j <= NY) {
	     con = (float) map[jloop][iloop].nasa;
	     conc[j][i] += con;
	     count[j][i] += 1;
	    }
        }
   }

   for (j = 0; j <= NY; j++)
   {  for (i = 0; i <= NX; i++)
	  {  if (count[j][i] != 0)  conc[j][i] = conc[j][i]/count[j][i];
	  }
   }

   n = fwrite(conc, sizeof(float), (NX+1)*(NY+1), output);
   if (n != (NX+1)*(NY+1) ) {
	 printf("Failed to write to output file!!\n");
	 return -1;
   }

   return 0;


}

void mapll(float lat, float lon, int *i, int *j)
{
/* Convert from geodetic latitude and longitude to polar stereographic
   grid coordinates.  Follows mapll by V. J. Troisi.         */

   const float cdr = 57.29577951;
   const pi        = 3.141592654;
   float alat, along, e;

   float t, x, y, rho, sl, tc, mc;

   alat = lat/cdr;
   along = lon/cdr;
   e = sqrt(e2);

   if ( fabs(alat) > pi/2.)  {
	 *i = -1;
	 *j = -1;
	 return;
   }
   else {
	 t = tan(pi/4. - alat/2.) /
	   pow( (1.-e*sin(alat))/(1.+e*sin(alat)) , e/2.);

	 if ( fabs(90. - slat) < 1.E-5) {
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

	 *i = (int) ((x + xorig)/dx+0.5);
	 *j = (int) ((y + yorig)/dy+0.5);

	 return;
   }

}

void mapxy(float *alat, float *along, int i, int j)
{
/* After V. Troisi program.  Given grid coordinates, compute latitude
  and longitude on a polar stereographic grid */

  const float cdr = 57.29577951;
  const float pi  = 3.141592654;
  float e;

  float x, y, sl, rho, cm, t, chi;

  x = i * nes_dx - nes_xorig;
  y = j * nes_dy - nes_yorig;

  sl = slat / cdr;
  rho = sqrt(x*x+y*y);
  e   = sqrt(e2);

  if (rho <= 0.1) {
	*alat  = 90. * nes_sgn;
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

  *alat = nes_sgn* (*alat) *cdr;
  *along = nes_sgn*atan2(sgn*y, sgn*x)*cdr + nes_slon;

  return;
}
