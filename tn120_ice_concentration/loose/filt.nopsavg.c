#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "icegrids.h"

void mapll(const float lat, const float lon, int *i, int *j, const float xorig,
           const float yorig, const float ee2, const float slat, 
           const float slon, const float r, const float tdx, 
           const float tdy, const float sgn);

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

#define indx  1.00
#define indy  1.00
#define outdx 0.50
#define outdy 0.50
#define innx  360
#define inny  180
#define outnx 720
#define outny 360

int main(int argc, char *argv[])
{
  FILE *in10, *in11, *in12, *out13, *out14, *out15;

  float outmap[outny][outnx], sst[inny][innx], errs[inny][innx];
  int xrat, yrat;
  unsigned char nmap[nny][nnx], smap[sny][snx];
  unsigned char coutmap[outny][outnx], cout2[outny][outnx];

  int i, j, yy, mm, dd, k;
  int tmp, lli, llj;
  float llat, llon, xcor, ycor, xorig, yorig, filt_temp;

  xrat = indx / outdx;
  yrat = indy / outdy;

  for (llj = 0; llj < outny; llj++ ) {
    for (lli = 0; lli < outnx; lli++ ) {
       outmap[llj][lli] = 0.0;
    }
  }

  in10 = fopen(argv[2],"r");
  in11 = fopen(argv[3],"r");
  in12 = fopen(argv[1],"r");
  if (in10 == NULL || in11 == NULL || in12 == NULL ) {
    printf("Failed to open a required input file\n");
    return -1;
  }
  out13 = fopen(argv[4],"w");
  out14 = fopen(argv[5],"w");
  out15 = fopen(argv[6],"w");

  filt_temp = (float) atof(argv[7]);

  fread(nmap, sizeof(unsigned char), nnx*nny, in10);
  fread(smap, sizeof(unsigned char), snx*sny, in11);
  fread(sst,  sizeof(float),         innx*inny,   in12);
  fread(errs, sizeof(float),         innx*inny,   in12);


/* Construct concentrations on lat-long grid */
  for (llj = 0; llj < outny; llj++ ) {

    llat = 90. - (llj +1. - 0.5)*outdy ;
      
    for ( lli = 0; lli < outnx; lli++ ) {
 
      llon = (lli + 1 - 0.5) * outdx ;

      if (llat > 0.) {
        xorig = -npolei*dx;
        yorig = -npolej*dy;
        mapll(llat, llon, &i, &j, xorig, yorig, eccen2, slat, nslon, rearth, dx, dy, 1.0);
        if (i > 0 && i < nnx && j > 0 && j < nny) {
          tmp = nmap[j][i];
          if (tmp < 129) {
            outmap[llj][lli] = min(tmp, 100);
          }
          else {
            outmap[llj][lli] = tmp;
          }
        } /* End of if on north grid */
      } /* End of if in northern hemisphere */
      

      else {
        xorig = -spolei*dx;
        yorig = -spolej*dy;
        mapll(fabs(llat), llon, &i, &j, xorig, yorig, eccen2, slat, sslon, rearth, dx, dy, -1.0);
        if (i > 0 && i < snx && j > 0 && j < sny) {
          tmp = (smap[j][i] );
          if (tmp < 129) {
            outmap[llj][lli] = min(tmp, 100);
          }
          else {
            outmap[llj][lli] = tmp;
          }
        } /* end of if on southern map */
      } /* End of hemisphere selection */


   
    } /* End of for lli */
  } /* End of for llj */


/* Begin SST filtering: Note that while the gribbed fields are in K, the
   operational OISST is in C */
        for (llj = 0; llj < outny; llj++) {
          for (lli = 0; lli < outnx ; lli++) {
            coutmap[llj][lli] = outmap[llj][lli];
            if (sst[llj/yrat][lli/xrat] >= filt_temp && 
                outmap[llj][lli] > 0.0 ) {
/*              printf("zeroing %d %d %f %f\n" ,lli, llj, sst[llj][lli], 
                                                        outmap[llj][lli] ); */ 
              outmap[llj][lli] = 0.0; 
            }
            cout2[llj][lli] = outmap[llj][lli]; 
          }
        }
/* end SST filtering */

     fwrite(cout2, sizeof(unsigned char), outnx*outny, out13);

/* Place sst filtering of polar stereo grids here */

     return 0;

}

/* Add mapxy here */

void mapll(const float lat, const float lon, int *i, int *j, const float xorig,
           const float yorig, const float ee2, const float slat, 
           const float slon, const float r, const float tdx, 
           const float tdy, const float sgn)
{
/* Convert from geodetic latitude and longitude to polar stereographic
   grid coordinates.  Follows mapll by V. J. Troisi.         */
/* Conventions include that slat and lat must be absolute values */
/* The hemispheres are controlled by the sgn parameter */
/* Bob Grumbine 15 April 1994. */

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
