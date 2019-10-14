#include <stdio.h>
#include "icegrids.h"
#include <math.h>

#define RECLAND    -1
#define RECSEA      0 
#define LAND 1
#define SEA  0

typedef struct {
  float latspac;
  float lonspac;
  float tagres; /* km */
  short recno[648];
  char spare[1892];
} dirrec ;
typedef struct {
  unsigned char chars[3200];
} block;


int unblock(short int *unblocked, block *tag); 
int blocksea( short int *unblocked); 
int blockland( short int *unblocked); 
void mapll(const float lat, const float lon, int *i, int *j, const float xorig,
           const float yorig, const float feccen2, const float slat, 
           const float slon, const float frearth, const float fdx, 
           const float fdy, const float sgn);


int main(void)
{
  FILE *fin, *fout;
  dirrec header;
  block  tags;
  short int unblocked[160][160];
  short int nland[NY_NORTH][NX_NORTH], sland[NY_SOUTH][NX_SOUTH];
  short int nsea[NY_NORTH][NX_NORTH], ssea[NY_SOUTH][NX_SOUTH];

  int i, k, l;
  float lat, lon, latb, lonb;
  int mi, mj;

/* Initialize the ssmi grid counters */
  for (k = 0; k < NY_NORTH; k++) {
    for (l = 0; l < NX_NORTH; l++) {
      nland[k][l] = 0;
      nsea[k][l]  = 0; 
    }
  }
  for (k = 0; k < NY_SOUTH; k++) {
    for (l = 0; l < NX_SOUTH; l++) {
      sland[k][l] = 0;
      ssea[k][l]  = 0; 
    }
  }
  
/* Now get hold of the nesdis data */
  fin = fopen("temphigh","r");
  fread(&header, sizeof(dirrec), 1, fin);

  for (i = 0; i < 648; i++) { 
/* Decode a tag block */
    if (header.recno[i] == RECSEA) {
      blocksea(&unblocked[0][0]);    
    }
    else if (header.recno[i] == RECLAND) {
      blockland(&unblocked[0][0]);    
    }
    else {
      fseek(fin, (header.recno[i]-1)*3200, SEEK_SET);
      fread(&tags, sizeof(block), 1, fin);
      unblock(&unblocked[0][0], &tags);
    }

/* Remap over to the ssmi grid */
    latb = -90.0 + 10.0*( (int) (i/36) );
    lonb = -180. + 10.0*( i % 36 );
    printf ("block lat, lon %f %f\n",latb, lonb);

    for (k = 0; k < 160; k++) {
      lat = latb + ((float)k)/16. ;
      if (lat > -20. && lat < 15.) continue;

      for (l = 0; l < 160; l++) {
        lon = lonb + ((float)l)/16. ;
        if (lat >= 15.) {
          mapll(lat, lon, &mi, &mj, xorig_NORTH, yorig_NORTH, eccen2, slat_NORTH,
                slon_NORTH, rearth, dx, dy, sgn_NORTH);
          if (mi >= 0 && mi < NX_NORTH && mj >= 0 && mj < NY_NORTH) {
            if (unblocked[k][l] == LAND) {
              nland[mj][mi] += 1;
            }
            else if (unblocked[k][l] == SEA) {
              nsea[mj][mi] += 1;
            }
            else {
              printf("unblock value out of range %d %d %d\n",k,l,unblocked[k][l]);
              return -1;
            }
          } /* end of filling in tags */
        } /* end of northern hemisphere if */


        if (lat < -20.) {
          mapll(-lat, lon, &mi, &mj, xorig_SOUTH, yorig_SOUTH, eccen2, 
                     slat_SOUTH, slon_SOUTH, rearth, dx, dy, sgn_SOUTH);
          if (mi >= 0 && mi < NX_SOUTH && mj >= 0 && mj < NY_SOUTH) {
            if (unblocked[k][l] == LAND) {
              sland[mj][mi] += 1;
            }
            else if (unblocked[k][l] == SEA) {
              ssea[mj][mi] += 1;
            }
            else {
              printf("unblock value out of range %d %d %d\n",k,l,unblocked[k][l]);
              return -1;
            }
          } /* end of filling in tags */
        } /* end of southern hemisphere if */


      } /* end of looping across longitude */
    } /* end of looping across latitude within block */
  } /* end of looping across blocks */

  fout=fopen("north","w");
  fwrite(&nland[0][0], sizeof(short int), NX_NORTH*NY_NORTH, fout);
  fwrite(&nsea [0][0], sizeof(short int), NX_NORTH*NY_NORTH, fout);

  fout=fopen("south","w");
  fwrite(&sland[0][0], sizeof(short int), NX_SOUTH*NY_SOUTH, fout);
  fwrite(&ssea [0][0], sizeof(short int), NX_SOUTH*NY_SOUTH, fout);

  return 0;
}

/* Given a tag block, unblock it in to a short in array of 1s and 0s */
int unblock(short int *unblocked, block *tag) 
{
  int i, j;
  unsigned char m1=128;
  unsigned char m2= 64;
  unsigned char m3= 32;
  unsigned char m4= 16;
  unsigned char m5=  8;
  unsigned char m6=  4;
  unsigned char m7=  2;
  unsigned char m8=  1;
  int index ;

  for (j = 0; j < 160; j++) {
    for (i = 0; i < 10; i++) {  /* Note range, the 160 is achieved through 
                                   decoding bit by bit through the char */
      index = j*160 + 16*i;

      unblocked[ index   ] = (tag->chars[ index / 8 + 1 ] & m1) != 0 ;
      unblocked[ index+1 ] = (tag->chars[ index / 8  + 1] & m2) != 0 ;
      unblocked[ index+2 ] = (tag->chars[ index / 8  + 1] & m3) != 0 ;
      unblocked[ index+3 ] = (tag->chars[ index / 8  + 1] & m4) != 0 ;
      unblocked[ index+4 ] = (tag->chars[ index / 8  + 1] & m5) != 0 ;
      unblocked[ index+5 ] = (tag->chars[ index / 8  + 1] & m6) != 0 ;
      unblocked[ index+6 ] = (tag->chars[ index / 8  + 1] & m7) != 0 ;
      unblocked[ index+7 ] = (tag->chars[ index / 8  + 1] & m8) != 0 ;
      unblocked[ index+8 ] = (tag->chars[ index / 8 ] & m1) != 0 ;
      unblocked[ index+9 ] = (tag->chars[ index / 8 ] & m2) != 0 ;
      unblocked[ index+10 ] = (tag->chars[ index / 8 ] & m3) != 0 ;
      unblocked[ index+11 ] = (tag->chars[ index / 8 ] & m4) != 0 ;
      unblocked[ index+12 ] = (tag->chars[ index / 8 ] & m5) != 0 ;
      unblocked[ index+13 ] = (tag->chars[ index / 8 ] & m6) != 0 ;
      unblocked[ index+14 ] = (tag->chars[ index / 8 ] & m7) != 0 ;
      unblocked[ index+15 ] = (tag->chars[ index / 8 ] & m8) != 0 ;
    }
  }

  return 0;
}
int blocksea( short int *unblocked) 
{
  int j;
  for (j = 0; j < 160*160; j++) {
    unblocked[j] = SEA ;
  } 

  return 0;
}
int blockland( short int *unblocked) 
{
  int j;
  for (j = 0; j < 160*160; j++) {
    unblocked[j] = LAND ;
  } 

  return 0;
}

void mapll(const float lat, const float lon, int *i, int *j, const float xorig,
           const float yorig, const float feccen2, const float slat, 
           const float slon, const float frearth, const float fdx, 
           const float fdy, const float sgn)
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
