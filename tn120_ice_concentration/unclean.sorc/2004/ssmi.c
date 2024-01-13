#include <stdio.h>
#include <stdlib.h>
#include <math.h>
/*
#ifdef CRAY
  #include <macros.h>
#endif
#ifdef LINUX
  #include <f2c.h>
#endif
  #include <macros.h>
*/

#include "ssmi.h"
#include "icessmi.h"
#include "icegrids.h"

/*C$$$  MAIN PROGRAM DOCUMENTATION BLOCK                            */
/*C                                                                 */
/*C MAIN PROGRAM:  SSMI      CONSTRUCT SEA ICE CONCENTRATION GRIDS  */
/*C   PRGMMR: ROBERT GRUMBINE  ORG: W/NP21    DATE: 97-06-25        */
/*C                                                                 */
/*C ABSTRACT: READ IN SSMI DATA AND CONSTRUCT SEA ICE CONCENTRATION */
/*C    GRIDS USING THE NASA TEAM ALGORITHM.                         */
/*C                                                                 */
/*C PROGRAM HISTORY LOG:                                            */
/*C    97-06-25  ROBERT GRUMBINE                                    */
/*C                                                                 */
/*C USAGE:                                                          */
/*C   FILES ARE HANDLED BY ORDER IN THE INVOKING COMMAND LINE       */
/*C   INPUT FILES:                                                  */
/*C      FILE 1  - INPUT SSMI DATA.  UNFORMATTED BINARY GENERATED   */
/*C              -   BY SSMI.BUFR.X                                 */
/*C      FILE 2  - NORTHERN HEMISPHERE LAND MASK                    */
/*C      FILE 3  - SOUTHERN HEMISPHERE LAND MASK                    */
/*C   OUTPUT FILES:                                                 */
/*C      FILE 4  - NORTHERN HEMISPHERE GRIDDED SSMI DATA            */
/*C      FILE 5  - SOUTHERN HEMISPHERE GRIDDED SSMI DATA            */
/*C      FILE 6  - MASKED NORTHERN HEMISPHERE ICE CONCENTRATIONS    */
/*C      FILE 7  - MASKED SOUTHERN HEMISPHERE ICE CONCENTRATIONS    */
/*C      FILE 8  - UNMASKED NORTHERN HEMISPHERE ICE CONCENTRATIONS  */
/*C      FILE 9  - UNMASKED SOUTHERN HEMISPHERE ICE CONCENTRATIONS  */
/*C                                                                 */
/*C SUBPROGRAMS CALLED:                                             */
/*C      UNIQUE: PROCESS_BUFR, PROCESS_SHORT_BUFR, CHECK_BUFR,      */
/*C              CHECK_SHORT_BUFR, ZERO_BUFR, NEWFILT, POLE_FILL    */
/*C              GETFLD, ICE_ZERO, ICE_ADD_DATA, ICE_ADD_BUFR,      */
/*C              ICE_AVG_DATA, NASA_TEAM                            */
/*C      LIBRARY:                                                   */
/*C        POLAR: MAPLL, MAPXY                                      */
/*C                                                                 */
/*C EXIT STATES:                                                    */
/*C    COND =  0  - SUCCESSFUL RUN                                  */
/*C         = -1  - ERROR OCCURRED.  MESSAGE WRITTED TO STDOUT      */
/*C                                                                 */
/*C REMARKS:                                                        */
/*C                                                                 */
/*C ATTRIBUTES:                                                     */
/*C   LANGUAGE: ANSI STANDARD C                                     */
/*C   MACHINE:  ANY                                                 */
/*C                                                                 */
/*C$$$                                                              */
#define TRUE ( 1 == 1 )
#define FALSE ( 1 == 0 )

/* Main program for decoding the ssmi orbital records. */
/* Read in (process_ and check_ procedures) SDR data. */
/* Remap to sea ice grid -- 25.4 km, true at 60, 80 W (North hemisphere)
    100 E (Southern hemisphere) -- ice_ procedures */
/* Compute sea ice concentrations -- nasa_team algorithm */
/* Robert Grumbine 18 December 1994 */
/* revised to handle flat files derived from bufr 6/97 */


float hires(ssmi *map, int nx, int ny, int polei, int polej, unsigned char *mask) ;

int main(int argc, char *argv[])
{
  bufr_line  a_bufr_line;
  FILE *fin, *input, *outn, *outs, *nland, *sland;
  FILE *nraw, *sraw;
  char fname[80];

  ssmi *south, *north;
  ssmi_tmp *north_tmp, *south_tmp;
  float  rfld_n[NY_NORTH][NX_NORTH];
  float  rfld_s[NY_SOUTH][NX_SOUTH];
  unsigned char  fld_n[NY_NORTH][NX_NORTH], nland_mask[NY_NORTH][NX_NORTH];
  unsigned char  fld_s[NY_SOUTH][NX_SOUTH], sland_mask[NY_SOUTH][NX_SOUTH];

  int north_pts, south_pts;
  int i, j, nerrs, nproc;
  int jday, satno;
  int leap, ref_year, day_yest;

/* Mallocate the ssmi arrays */
  north_tmp = malloc( sizeof(ssmi_tmp) * NX_NORTH*NY_NORTH);
  south_tmp = malloc( sizeof(ssmi_tmp) * NX_SOUTH*NY_SOUTH);
  north     = malloc( sizeof(ssmi) *NX_NORTH*NY_NORTH);
  south     = malloc( sizeof(ssmi) *NX_SOUTH*NY_SOUTH);
  if (north == NULL || south == NULL ||
      south_tmp == NULL || north_tmp == NULL) {
    printf("Failed to mallocate one of the ssmi fields!\n");
    return -1;
  }

/* Open data files */
  fin = fopen(argv[1], "r");
  nland = fopen(argv[2], "r");
  sland = fopen(argv[3], "r");
  outn  = fopen(argv[4], "w");
  outs  = fopen(argv[5], "w");
  nraw  = fopen(argv[6], "w");
  sraw  = fopen(argv[7], "w");
  if (fin == (FILE*) NULL || outn == (FILE*) NULL || outs == (FILE*) NULL ||
      nraw == (FILE*) NULL || sraw == (FILE*) NULL ||
      nland == (FILE*) NULL || sland == (FILE*) NULL)
  {
    printf("Failed to open one of the seven files!\n");
    if (fin == NULL ) {printf(" - input file\n");}
    if (nland == NULL ) {printf(" - north land in \n");}
    if (sland == NULL ) {printf(" - south land in \n");}
    if (outn == NULL ) {printf(" - north SSMI out \n");}
    if (outs == NULL ) {printf(" - south SSMI out \n");}
    if (nraw == NULL ) {printf(" - north char out \n");}
    if (sraw == NULL ) {printf(" - south char out \n");}
    return -1;
  }

  jday   = atoi(argv[8]);
  printf("Julian day to get data for %d\n",jday);
  ref_year = atoi(argv[9]);
  satno    = atoi(argv[10]);

  if ( ref_year % 4 != 0) { leap = FALSE ; }
  else {
    if ( ref_year % 400 != 0 ) { leap = FALSE ; }
    else { leap = TRUE ; }
  }

  if ( jday == 1 ) {
    if ( leap == TRUE ) { day_yest = 366 ; }
    else { day_yest = 365 ; }
  }
  else {
    day_yest = jday - 1;
  }


/* Set internal constants */
  nproc = NORBITS;
  north_pts = (NX_NORTH)*(NY_NORTH);
  south_pts = (NX_SOUTH)*(NY_SOUTH);
  ice_zero(north_tmp, south_tmp, north_pts, south_pts);

  while ( !feof(fin) ) {
/* Read in name of next file to process */
    fscanf(fin, "%s\n", fname);
    printf("Opened %s\n", fname);
    input = fopen(fname, "r");
    if ( input == NULL ) {
       printf("Failed to open %s\n", fname);
       break;
    }


/* Begin stripping out header */
    fseek(input, (long)(0) , SEEK_SET);

/* Begin stripping out data */
    for (i = 0; i < nproc ; i++)
    while (!feof(input) )
    {

      /* Add data only if data is within window */
      /* Warning: Does not handle year end and leap years properly */
        j = 0;
        while (!feof(input) )
        {
          #ifdef VERBOSE2
            printf("Process record number %d\n",j); fflush(stdout);
          #endif
          j++;
          fread(&a_bufr_line, sizeof(bufr_line), 1, input);

          #ifdef VERBOSE2
            printf("%d Satno = %d\n",j, a_bufr_line.satno);
          #endif
          if (a_bufr_line.satno == satno) {
            nerrs = process_bufr(&a_bufr_line);
            #ifdef VERBOSE2
            if (nerrs != 0) {
              printf("%2d errors in record %6d of orbit %2d\n",nerrs, j, i);
              fflush(stdout);
            }
            #endif
            ice_add_bufr(north_tmp, south_tmp, &a_bufr_line);
          }
        }

    } /* end processing the orbits */

    fclose(input);
  } /* End while - reading files */

  /* Compute averaged tb and concentrations */
  ice_avg_data(north_tmp, south_tmp, north, south, north_pts, south_pts);

/* Apply newer filtering techniques 10/29/95 BG */
  newfilt(north, south);
  #ifdef VERBOSE
  printf("returned from newfilt\n"); fflush(stdout);
  #endif

/* Apply the high resolution algorithm */
  fread(nland_mask, sizeof(unsigned char), north_pts, nland);
  fread(sland_mask, sizeof(unsigned char), south_pts, sland);
  #ifdef VERBOSE
    printf("read in the land mask files\n"); fflush(stdout);
  #endif

  #ifdef HIRES
    hires(north, NX_NORTH, NY_NORTH, polei_NORTH, polej_NORTH, &nland_mask[0][0]);
    #ifdef VERBOSE
      printf("back from north hires\n");
    #endif
    hires(south, NX_SOUTH, NY_SOUTH, polei_SOUTH, polej_SOUTH, &sland_mask[0][0]);
    #ifdef VERBOSE
      printf("back from hires\n");
    #endif
  #endif 
  

/* Fill in pole for output purposes */
  #ifdef HIRES
    getfld(north, north_pts, &fld_n[0][0], &rfld_n[0][0], HIRES_CONC);
    getfld(south, south_pts, &fld_s[0][0], &rfld_s[0][0], HIRES_CONC);
  #else
    getfld(north, north_pts, &fld_n[0][0], &rfld_n[0][0], BAR_CONC);
    getfld(south, south_pts, &fld_s[0][0], &rfld_s[0][0], BAR_CONC);
  #endif
  pole_fill(&fld_n[0][0], 1);
  pole_fill(&fld_s[0][0], 2);
  fwrite(fld_n, sizeof(unsigned char), north_pts, nraw);
  fwrite(fld_s, sizeof(unsigned char), south_pts, sraw);

/* Mask out the land now */
  ice_mask(north, south, north_pts, south_pts, 
         &nland_mask[0][0], &sland_mask[0][0]);

/* Write out full ssmi data file */
  fwrite(north, sizeof(ssmi), north_pts, outn);
  fwrite(south, sizeof(ssmi), south_pts, outs);
  fclose(outn);
  fclose(outs);

/* Expt: Write out the data count maps */
  outn = fopen("count.n", "w");
  outs = fopen("count.s", "w");
  getfld(north, north_pts, &fld_n[0][0], &rfld_n[0][0], COUNT);
  getfld(south, south_pts, &fld_s[0][0], &rfld_s[0][0], COUNT);
  fwrite(fld_n, sizeof(unsigned char), north_pts, outn);
  fwrite(fld_s, sizeof(unsigned char), south_pts, outs);
  fclose(outn);
  fclose(outs);

  return 0;

}

int ice_add_bufr(ssmi_tmp *north_tmp, ssmi_tmp  *south_tmp,
                 bufr_line *a)
{
/* Version of the SDR ice_add_data suited to work with BUFR input */

  int j, k;
  float tlat, tlon;
  float t19v, t19h, t22v, t37v, t37h, t85v, t85h;
  float nasa;
  int ilat, jlon, stype, index;
  #ifdef HIRES
    int KMAX = 4;
  #else
    int KMAX = 1;
  #endif

/* Peel off data from the record.  Transfer to physical units. */
  for (j = 0; j < NSCANS; j++) {
    t19v = ( (float)a->full[j].t19v );
    t19h = ( (float)a->full[j].t19h );
    t22v = ( (float)a->full[j].t22v );
    t37v = ( (float)a->full[j].t37v );
    t37h = ( (float)a->full[j].t37h );
    stype = a->full[j].surface_type;
    for (k = 0; k < KMAX; k++) {
      if (k > 0) {
        #ifdef HIRES
        tlat =  ( a->full[j].hires[k-1].latitude);
        tlon = ( (float)a->full[j].hires[k-1].longitude);
        t85v = ( (float)a->full[j].hires[k-1].t85v );
        t85h = ( (float)a->full[j].hires[k-1].t85h );
        #endif
      }
      else {
        tlat =  ( a->full[j].latitude);
        tlon = ( (float)a->full[j].longitude);
        t85v = ( (float)a->full[j].t85v );
        t85h = ( (float)a->full[j].t85h );
      }

      if (tlat > -25. && tlat < 20. ) continue;

    /* Note that we are now doubly-nested in j, k though indentation is not*/
    nasa = 0.;

    if ((t19v == 0.) && (tlat == 0.) && (tlon == 0.) ) {
      printf("no good tb\n");
      continue;
    } /* done handling the bad data case */

/* Now check for which hemisphere the data are in. */
    if (tlat > 0.) {
      mapll(tlat, tlon, &ilat, &jlon, xorig_NORTH, yorig_NORTH,
            eccen2, slat_NORTH, slon_NORTH, rearth, dx, dy, sgn_NORTH);
      if ( (ilat >= 0) && (ilat <  (NX_NORTH) ) &&
           (jlon >= 0) && (jlon <  (NY_NORTH) )    ) {
        index = ilat+jlon*(NX_NORTH);
        nasa = (int) (0.5 + nasa_team(t19v, t19h, t22v,
                              t37v, t37h, t85v, t85h, 'n', 1, 1)   );
        if (nasa != NO_DATA && nasa != BAD_DATA && nasa != WEATHER) {
          north_tmp[index].count += 1;
          north_tmp[index].t19v += t19v*100.;
          north_tmp[index].t19h += t19h*100.;
          north_tmp[index].t22v += t22v*100.;
          north_tmp[index].t37v += t37v*100.;
          north_tmp[index].t37h += t37h*100.;
          north_tmp[index].t85v += t85v*100.;
          north_tmp[index].t85h += t85h*100.;
          if ( north_tmp[index].conc_bar == NO_DATA ||
               north_tmp[index].conc_bar == BAD_DATA ||
               north_tmp[index].conc_bar == WEATHER  ) {
            north_tmp[index].conc_bar = (int) (0.5+nasa);
            north_tmp[index].count = 1;
          }
          else {
            north_tmp[index].conc_bar +=   (int) (0.5 + nasa);
          }
        } /* end of adding data if data not equal bad or land */
        else {
          if ( nasa == BAD_DATA && north_tmp[index].count == 0 ) {
            north_tmp[index].conc_bar = BAD_DATA;
          }
          if ( nasa == WEATHER && north_tmp[index].count == 0 ) {
            north_tmp[index].conc_bar = WEATHER;
          }
        }

      } /* end of lat-long range test */
    } /* end of north hemisphere work */

    else {
/* South branch : BEWARE!! must have positive latitudes, take -tlat.*/
/* Mapll fixed 4 June 1997 to accept physical latitudes */
      mapll(tlat, tlon, &ilat, &jlon, xorig_SOUTH, yorig_SOUTH,
            eccen2, slat_SOUTH, slon_SOUTH, rearth, dx, dy, sgn_SOUTH);
      if (ilat >= 0 && ilat < NX_SOUTH && jlon >= 0 && jlon < NY_SOUTH) {
        index = ilat + jlon*(NX_SOUTH);
        nasa = (int) (0.5 +  nasa_team(t19v, t19h, t22v,
                                t37v, t37h, t85v, t85h, 's', 1, 1) ) ;
        if (nasa != NO_DATA && nasa != BAD_DATA && nasa != WEATHER ) {
          south_tmp[index].count += 1;
          south_tmp[index].t19v += t19v*100.;
          south_tmp[index].t19h += t19h*100.;
          south_tmp[index].t22v += t22v*100.;
          south_tmp[index].t37v += t37v*100.;
          south_tmp[index].t37h += t37h*100.;
          south_tmp[index].t85v += t85v*100.;
          south_tmp[index].t85h += t85h*100.;
          if ( south_tmp[index].conc_bar == NO_DATA ||
               south_tmp[index].conc_bar == BAD_DATA ||
               south_tmp[index].conc_bar == WEATHER   ) {
            south_tmp[index].conc_bar = (int) (0.5 + nasa) ;
            south_tmp[index].count = 1;
          }
          else {
            south_tmp[index].conc_bar += (int) (0.5 + nasa);
          }
        } /* end of adding data if data not equal bad or land */
        else {
         if ( nasa == BAD_DATA && south_tmp[index].count == 0 ) {
           south_tmp[index].conc_bar = BAD_DATA;
         }
         if ( nasa == WEATHER && south_tmp[index].count == 0 ) {
           south_tmp[index].conc_bar = WEATHER;
         }
        }


      } /* end of adding data */

    } /* end north-south splitting */


  } /* end for k loop -- checking full resolution parameters*/
  } /* end for j loop -- spot values */

  return 0;

}

int process_bufr(bufr_line *b)
/* Process the bufr data records, which will eventually include the short
    data as well.
   Only processing is to check for qc purposes.
*/
{
  return check_bufr(b);
}


/* Perform some bounds checking for the long data */
/* Order of checking is related to the frequency with which
   a given field is erroneous */
/* Note that typically if one number is wrong, so are several others */
/* Robert Grumbine 10 February 1994 */
/* High resolution processing added 11 October 2001 */

int check_bufr(bufr_line *b)
{
  int nerr = 0;
  int i, k;

  for (i = 0; i < NSCANS; i++) {
    if ((int) b->full[i].surface_type > 8 ) {
      nerr += 1;
      zero_bufr(b, i);
    }

    if ( b->full[i].t19h > 295.0 ||  b->full[i].t19h <  75.0) {
      nerr += 1;
      zero_bufr(b, i);
    }
    if ( b->full[i].t19v > 295.0 ||  b->full[i].t19v < 150.0) {
      nerr += 1;
      zero_bufr(b, i);
    }

    if ( b->full[i].t22v > 295.0 ||  b->full[i].t22v < 150.0) {
      nerr += 1;
      zero_bufr(b, i);
    }

    if ( b->full[i].t37h > 295.0 ||  b->full[i].t37h < 100.0) {
      nerr += 1;
      zero_bufr(b, i);
    }
    if ( b->full[i].t37v > 295.0 ||  b->full[i].t37v < 150.0) {
      nerr += 1;
      zero_bufr(b, i);
    }

    if ( b->full[i].t85h > 295.0 ||  b->full[i].t85h < 125.0) {
      nerr += 1;
      zero_bufr(b, i);
    }
    if ( b->full[i].t85v > 295.0 ||  b->full[i].t85v < 150.0) {
      nerr += 1;
      zero_bufr(b, i);
    }

    if ( b->full[i].latitude > 180.) {
       nerr += 1;
       zero_bufr(b, i);
    }
    if ( b->full[i].longitude > 360.) {
       nerr += 1;
       zero_bufr(b, i);
    }

    if ( b->full[i].t19h > b->full[i].t19v) {
      printf("failed v > h 19 test\n");
      nerr += 1;
      zero_bufr(b,i);
    }
    if ( b->full[i].t37h > b->full[i].t37v) {
      printf("failed v > h 37 test\n");
      nerr += 1;
      zero_bufr(b,i);
    }
    if ( b->full[i].t85h > b->full[i].t85v) {
      printf("failed v > h 85 test\n");
      nerr += 1;
      zero_bufr(b,i);
    }

    #ifdef HIRES
    for (k = 0; k < 3; k++) {
      if (check_short_bufr(&(b->full[i].hires[k]) ) != 0) {
        nerr += 1;
        zero_bufr(b, i);
      }
    }
    #endif
       

  } /* end checking */

  return nerr;

}

void zero_bufr(bufr_line *b, int i)
{
   int k;

   b->full[i].scan_counter = 0;
   b->full[i].latitude     = 0;
   b->full[i].longitude    = 0;
   b->full[i].t19v         = 0;
   b->full[i].t19h         = 0;
   b->full[i].t22v         = 0;
   b->full[i].t37v         = 0;
   b->full[i].t37h         = 0;
   b->full[i].t85v         = 0;
   b->full[i].t85h         = 0;
   b->full[i].surface_type = 0;
   b->full[i].position_num = 0;
   #ifdef HIRES
   for (k = 0; k < 3; k++) {
     b->full[i].hires[k].t85v = 0;
     b->full[i].hires[k].t85h = 0;
     b->full[i].hires[k].latitude = 0;
     b->full[i].hires[k].longitude = 0;
   }
   #endif

  return;
}

/* Bounds checking on a short data record */
/* Robert Grumbine 1 March 1995 */

int check_short_bufr(short_bufr *c)
{
  int nerr = 0;

  if (c->latitude > 180.) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
  }
  if (c->longitude > 360.) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
  }
  if ( c->t85v > 295.0 ||  c->t85h > 295.0) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
  }
  if ( c->t85v < 150.0 ) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
  }
  if ( c->t85h < 125.0 ) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
  }
  if ( c->t85h > c->t85v ) {
    nerr += 1;
    c->latitude = 0;
    c->longitude = 0;
    c->t85v = 0;
    c->t85h = 0;
  }

  return nerr;

}

#include "icessmi.h"

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


/* Routine to fill in the unobserved polar gap */
/* Use a laplacean fill on the principle that the true field
   there is the smoothest (gradient sense) possible */
/* Robert Grumbine 4 June 1997 */

int pole_fill(unsigned char *map, const int pole)
{
    int i, j;
    float *a0, *a1, *a2, *delta;
    int nx, ny, polei, polej;
    int index, indexori, holex, holey;
    int iters, itmax = 15, jp, jm, ip, im;
    float delmax;

    if (dx != dy) {
      printf("Cannot use this polefill routine, it assumes that dx = dy.\n");
      printf(" dx = %f dy = %f\n",dx, dy);
      return -1;
    }

    if (pole == 1) {
      nx = NX_NORTH;
      ny = NY_NORTH;
      polei = polei_NORTH;
      polej = polej_NORTH;
    }
    else {
      nx = NX_SOUTH;
      ny = NY_SOUTH;
      polei = polei_SOUTH;
      polej = polej_SOUTH;
    }

    holex = 2*( 0.5 + (90. - MAX_LATITUDE) * 111.e3 / dx ) + 3;
    holey = holex;

    printf("holex, holey = %d %d \n",holex, holey);

    a0 = malloc(sizeof(float)*holex*holey);
    a1 = malloc(sizeof(float)*holex*holey);
    a2 = malloc(sizeof(float)*holex*holey);
    delta = malloc(sizeof(float)*holex*holey);

/* Make copy for local use */
    for (j = 0; j < holey; j++)
    { for (i = 0; i < holex; i++)
      {
        index = i+j*holex;
        indexori = (polei - holex/2 + i) + (polej - holey/2 + j)*nx;
        a0[index] = (float) map[indexori];
      }
    }

/* Fill in the 'no data' or 'bad data' points */
   for (j = 0; j < holey  ; j++)
   { for (i = 0; i < holex  ; i++)
     {
       index = i + j*holex;
       jp =  i      + (holey-1)*holex;
       jm =  i      +   0*holex;
       ip = holex-1 + j*holex;
       im =  0      + j*holex;
       if (a0[index] == NO_DATA || a0[index] == BAD_DATA || a0[index] == 0) {
         if ( a0[im] == NO_DATA || a0[ip] == NO_DATA ||
              a0[im] == BAD_DATA || a0[ip] == BAD_DATA ||
              a0[jm] == NO_DATA || a0[jp] == NO_DATA ||
              a0[jm] == BAD_DATA || a0[jp] == BAD_DATA ) {
           printf("Data problem with pole filling, no or bad data on bndy\n");
           return -1;
         }
         a1[index] = 0.5 *(a0[im] + (float)(i)/(float)(holex) *
                                     (a0[ip] - a0[im]) )
                   + 0.5 *(a0[jm] + (float)(j)/(float)(holey) *
                                     (a0[jp] - a0[jm]) );
       }
       else {
         a1[index] = a0[index];
       }
     }
   }

/* Now start some sort of an iteration scheme on the interior values */
   iters = 0;
   do {
     delmax = 0.;
     iters += 1;

     for (j = 1; j < holey-1; j++)
     { for ( i = 1; i < holex-1; i++)
       {
         index = i + j*holex;
         im =  i-1 + j*holex;
         ip =  i+1 + j*holex;
         jm =  i + (j-1)*holex;
         jp =  i + (j+1)*holex;

         delta[index] = -a1[index] + (a1[im] + a1[ip] + a1[jm] + a1[jp])/4.;
         a2[index] = a1[index]+delta[index];

         delmax = max(fabs(delta[index]), delmax);
       }
     }
     printf("delmax = %f\n",delmax);
     for (j = 1; j < holey-1; j++)
     { for ( i = 1; i < holex-1; i++)
     {  index = i + j*holex;
        a1[index] = a2[index];
     }
     }

   }
   while ( fabs(delmax) > 0.5 && iters < itmax );

    for (j = 0; j < holey; j++)
    { for (i = 0; i < holex; i++)
      {
        index = i+j*holex;
        indexori = (polei - holex/2 + i) + (polej - holey/2 + j)*nx;
        if ( a0[index] == BAD_DATA || a0[index] == NO_DATA || a0[index] == 0) {
          map[indexori] = (int) (0.5 + a1[index]) ;
        }
      }
    }

   free(a0);
   free(a1);
   free(a2);
   free(delta);

   return 0;

}



#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#include "icessmi.h"

#define FALSE (1==0)
#define TRUE (1==1)
#define MAX_CONC 128

/* Elements specific to the high resolution SSMI algorithm and processing */
/* Note that almost the entire program is identical to the low-res version */
/* This is necessarily the case as the goal is to produce something which */
/*   is maximally correspondant to the old processing, just higher resolution */

void landuse(int *use, unsigned char *mask, ssmi *obs, int nx, int ny,
             int polei, int polej, int latrange) ;
void regress(float *test, float *conc, int count, float *a, float *b, 
                                                float *r) ;
/* int min(int x, int y) ; */
/* int max(int x, int y) ; */

float hires(ssmi *map, int nx, int ny, int polei, int polej, unsigned char *mask) {
  float *conc, *test, a, b, r;
  float c1, c2, c3, *q;
  int *use;
  float fconc;
  int i, count = 0, tconc;
  int latrange = 8*30;  
  float th, tv;

/* find the points which are usable -- good tb's, not too close to land */
  use  = malloc(nx*ny*sizeof(int));
  conc = malloc(nx*ny*sizeof(float));
  test = malloc(nx*ny*sizeof(float));

  landuse(use, mask, map, nx, ny, polei, polej, latrange);

  #ifdef VERBOSE
  printf("Entered hires and completed malloc and landuse\n"); fflush(stdout);
  #endif

  for (i = 0; i < nx*ny; i++) {
    if (use[i]) {
      /* th = (float) map[i].t37h; */
      /* tv = (float) map[i].t37v; */
      th = (float) map[i].t85h;
      tv = (float) map[i].t85v;

      conc[count] = map[i].conc_bar;
      test[count] = sqrt( fabs(tv*tv - th*th) );
      count += 1;
    }
  }
  printf("hires count = %d\n", count); fflush(stdout);

/* Perform the linear regression */
  regress(test, conc, count, &a, &b, &r);
  printf("regression, %f %f  %f\n",a,b,r); fflush(stdout);

  /* Starting a quadratic regression */
/*  q = malloc(count*sizeof(float) ); */
/*  for (i = 0; i < count; i++) { */
/*    th = (float) map[i].t85h; */
/*    tv = (float) map[i].t85v; */
/*    q[i] = a + b * sqrt(fabs(tv*tv - th*th) ); */
/*    test[i] = q[i]*test[i]; */
/*  } */
/*  c2 = a; c1 = b; */
/*  regress(test, conc, count, &a, &b, &r); */
/*  printf("regression 2, %f %f  %f\n",a,b,r); fflush(stdout); */
/*  c1 *= b; */
/*  c2 *= b; */
/*  c3 = a; */
/*  printf("quadratic regression %e %e %f\n", c1, c2, c3); fflush(stdout); */


  for (i = 0; i < nx*ny; i++) {
    tconc = (int) map[i].conc_bar;
    if (tconc != WEATHER && tconc != BAD_DATA && tconc != NO_DATA) {
      /* th = (float) map[i].t37h; */
      /* tv = (float) map[i].t37v; */
      th = (float) map[i].t85h;
      tv = (float) map[i].t85v;
      fconc = a + b * sqrt(fabs(tv*tv - th*th) ); 
      /* fconc = c1*(tv*tv-th*th) + c2*sqrt(fabs(tv*tv - th*th) ) + c3; */
      if (fconc < MIN_CONC) fconc = 0;
      if (fconc > MAX_CONC) fconc = MAX_CONC;
      map[i].hires_conc = (unsigned char) (0.5 + fconc);
    }
    else {
      map[i].hires_conc = (unsigned char) tconc;
      if (map[i].hires_conc != map[i].conc_bar) {
         printf("wild failure in hires %d vs. %d \n",(int) map[i].hires_conc, 
              (int) map[i].conc_bar );
      }
    }
  }

  free(use);
  free(conc);
  free(test);
  /* Return the correlation as a quality parameter */
  return r;
}

void landuse(int *use, unsigned char *mask, ssmi *obs, int nx, int ny,
             int polei, int polej, int latrange) {
  int i, j;
  int k, l, index, tindex, tconc;
  int range = 5;

  #ifdef VERBOSE
  printf("Entered landuse\n"); fflush(stdout);
  #endif

  for (j = 0; j < ny; j++) {
  for (i = 0; i < nx; i++) {
    #ifdef VERBOSE2
    printf("landuse %d %d\n",i,j); fflush(stdout);
    #endif
    index = i + j*nx;
    use[index] = TRUE;
    tconc = (int) obs[index].conc_bar;
    if (tconc == WEATHER || tconc == BAD_DATA || tconc == NO_DATA ||
        mask[index] == LAND || mask[index] == COAST ) {
      use[index] = FALSE;
    }
    else {
      for (l = max(0,j - range); l < min(ny-1,j + range); l++) {
      for (k = max(0,i - range); k < min(nx-1,i + range); k++) {
        tindex = k + l*nx;
        if (mask[tindex] == LAND || mask[tindex] == COAST) {
          use[index] = FALSE;
          continue;
        }
      }
      }
    }
/* Now ensure that only high latitude values are used -- exclude Gulf
   Stream storms, etc. */
    if ( use[index] &&
        ((i-polei)*(i-polei)+(j-polej)*(j-polej)) < latrange*latrange) {
      use[index] = FALSE;
    }

  }
  }    

  #ifdef VERBOSE
  printf("leaving landuse\n"); fflush(stdout);
  #endif
  return;
}
void regress(float *test, float *conc, int count, float *a, float *b, 
             float *r) {
   float sx = 0., sy = 0., sxy = 0., sxx = 0., syy = 0., det;
   float tconc;
   int i;
 
   for (i = 0; i < count; i++) {
      sx += test[i];
      sy += conc[i];
      sxy += test[i]*conc[i];
      sxx += test[i]*test[i];
      syy += conc[i]*conc[i];
   }
   det = count * sxx - sx*sx;
   if (det == 0.) {
     printf("Singular in regress, cannot construct regression.\n");
     a = 0;
     b = 0;
     r = 0;
     return;
   }

   *b = (count *sxy - sx*sy)/det;
   *a = (sy - *b*sx)/count;
   *r = (count *sxy - sx*sy) / sqrt(det) / sqrt(count*syy - sy*sy) ;
   printf("In regress, a, b, r = %f %f %f\n",*a, *b, *r); fflush(stdout);

   sx = 0.;
   sxx = 0.;
   for (i = 0; i < count; i++) {
     sx  +=  *a + (*b)*test[i] - conc[i];
     sxx += (*a + (*b)*test[i] - conc[i])*(*a + (*b)*test[i]-conc[i]);
   }
   printf("sample bias, rms %f %f\n", sx/count, sqrt(sxx/count) ); 
   fflush(stdout); 

   for (i = 0; i < count; i++) {
     tconc = *a + (*b)*test[i];
     if (fabs(tconc - conc[i]) > 3.*sqrt(sxx/count) ) {
        printf("%f %f %f  %5.1f %5.1f %6.1f\n",*a, *b, test[i], conc[i], tconc, tconc - conc[i]);
        fflush(stdout);
     }
   }



   return;
}

/*

int min(int x, int y) {
  if (x < y) { return x; }
  else { return y;}
  return y;
}
int max(int x, int y) {
  if (x > y) { return x; }
  else { return y;}
  return y;
}
*/
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
  case COUNT :
    for (i = 0; i < limit; i++)
    {
      ffld[i] = (float) ice[i].count;
      cfld[i] = (unsigned char) ice[i].count;
    }
    break;
  case HIRES_CONC :
    for (i = 0; i < limit; i++)
    {
      ffld[i] = (float) ice[i].hires_conc/100.;
      cfld[i] = (unsigned char) ice[i].hires_conc ;
    }
    break;
  default :
    return -1;
  }

  return 0;

}

/* Average (as needed) the brightness temperatures and ice concentrations
     on the sea ice grid. */
/* Compute the ice concentration with the averaged ice temperatures (a1
     files) */
/* Robert Grumbine 10 February 1995 */
/* Apply the minimum concentration cutoff  Robert Grumbine 8 June 2001 */

int ice_avg_data(ssmi_tmp *north_tmp, ssmi_tmp *south_tmp,
                 ssmi *north, ssmi *south,
                 const int north_pts, const int south_pts)
{
  int i, nlim, slim;

  nlim = north_pts;
  slim = south_pts;

  for (i = 0; i < nlim ; i++)
  {
    if (north_tmp[i].count == 0) {
      north[i].conc_bar = north_tmp[i].conc_bar;
      north[i].bar_conc = north_tmp[i].conc_bar;
      north[i].t19v = 0;
      north[i].t19h = 0;
      north[i].t22v = 0;
      north[i].t37v = 0;
      north[i].t37h = 0;
      north[i].t85v = 0;
      north[i].t85h = 0;
      north[i].count = 0;
    }
    else {
      north[i].t19v = (int) (0.5 +  north_tmp[i].t19v / \
                                        (float) north_tmp[i].count);
      north[i].t19h = (int) (0.5 +  north_tmp[i].t19h / \
                                        (float) north_tmp[i].count);
      north[i].t22v = (int) (0.5 +  north_tmp[i].t22v / \
                                        (float) north_tmp[i].count);
      north[i].t37v = (int) (0.5 +  north_tmp[i].t37v / \
                                        (float) north_tmp[i].count);
      north[i].t37h = (int) (0.5 +  north_tmp[i].t37h / \
                                        (float) north_tmp[i].count);
      north[i].t85v = (int) (0.5 +  north_tmp[i].t85v / \
                                        (float) north_tmp[i].count);
      north[i].t85h = (int) (0.5 +  north_tmp[i].t85h / \
                                        (float) north_tmp[i].count);
      north[i].conc_bar = (int) (0.5 + (float) north_tmp[i].conc_bar /
                                       (float) north_tmp[i].count);
      north[i].count    = north_tmp[i].count;
      north[i].bar_conc = 0.5 + nasa_team(
        (float)north[i].t19v / 100.,
        (float)north[i].t19h / 100.,
        (float)north[i].t22v / 100.,
        (float)north[i].t37v / 100.,
        (float)north[i].t37h / 100.,
        (float)north[i].t85v / 100.,
        (float)north[i].t85h / 100., 'n', 1, 1
      );
      #ifdef VERBOSE2
        printf("averaged data, %4d, %5d %5d %5d %5d %3d %3d %5d %3d\n",
        i, north[i].t19v, north[i].t19h, north[i].t22v, north[i].t37v,
        north[i].conc_bar, north[i].bar_conc,
        north[i].conc_bar - north[i].bar_conc, north[i].count);
      #endif

    }
  }

  for (i = 0; i < slim ; i++)
  {
    if (south_tmp[i].count == 0) {
      south[i].conc_bar = south_tmp[i].conc_bar;
      south[i].bar_conc = south_tmp[i].conc_bar;
      south[i].t19v = 0;
      south[i].t19h = 0;
      south[i].t22v = 0;
      south[i].t37v = 0;
      south[i].t37h = 0;
      south[i].t85v = 0;
      south[i].t85h = 0;
      south[i].count = 0;
    }
    else {
      south[i].t19v = (int) (0.5 +  south_tmp[i].t19v /
                                        (float) south_tmp[i].count);
      south[i].t19h = (int) (0.5 +  south_tmp[i].t19h /
                                        (float)  south_tmp[i].count);
      south[i].t22v = (int) (0.5 +  south_tmp[i].t22v /
                                        (float) south_tmp[i].count);
      south[i].t37v = (int) (0.5 +  south_tmp[i].t37v /
                                        (float) south_tmp[i].count);
      south[i].t37h = (int) (0.5 +  south_tmp[i].t37h /
                                        (float) south_tmp[i].count);
      south[i].t85v = (int) (0.5 +  south_tmp[i].t85v /
                                        (float) south_tmp[i].count);
      south[i].t85h = (int) (0.5 +  south_tmp[i].t85h /
                                        (float) south_tmp[i].count);
      south[i].conc_bar = (int) (0.5 + (float)south_tmp[i].conc_bar /
                                       (float)south_tmp[i].count);
      south[i].count    = south_tmp[i].count;
      south[i].bar_conc = 0.5 + nasa_team(
        (float)south[i].t19v / 100. ,
        (float)south[i].t19h  / 100.,
        (float)south[i].t22v  / 100.,
        (float)south[i].t37v  / 100.,
        (float)south[i].t37h  / 100.,
        (float)south[i].t85v  / 100.,
        (float)south[i].t85h  / 100., 's', 1, 1
       );
    }
  }

/* Minimum concentration cutoffs  8 June 2001 */
  for (i = 0; i < nlim; i++) {
     if (north[i].conc_bar < MIN_CONC) north[i].conc_bar = 0;
     if (north[i].bar_conc < MIN_CONC) north[i].bar_conc = 0;
  }
  for (i = 0; i < slim; i++) {
     if (south[i].conc_bar < MIN_CONC) south[i].conc_bar = 0;
     if (south[i].bar_conc < MIN_CONC) south[i].bar_conc = 0;
  }

  #ifdef VERBOSE
    printf("About to leave ice_avg_data\n");
    fflush(stdout);
  #endif

  return 0;

}

/* Apply a land mask to the concentrations (but not the brightness
     temperatures ) */
/* Robert Grumbine 28 March 1995 */

int ice_mask( ssmi *north, ssmi *south,
              const int north_pts, const int south_pts,
              unsigned char *nmap, unsigned char *smap                     )
{
  int i, nlim, slim;

  nlim = north_pts;
  slim = south_pts;

  for (i = 0; i < nlim ; i++)
  {
    if ( ((int) nmap[i]) == ((int) LAND) ) {
      north[i].conc_bar = LAND ;
      north[i].bar_conc = LAND ;
    }
    if ( ((int) nmap[i]) == ((int) COAST) ) {
      north[i].conc_bar = COAST ;
      north[i].bar_conc = COAST ;
    }
  }

  for (i = 0; i < slim ; i++)
  {
    if (smap[i] == LAND) {
      south[i].conc_bar = LAND ;
      south[i].bar_conc = LAND ;
    }
    if ( ((int) smap[i]) == ((int) COAST) ) {
      south[i].conc_bar = COAST ;
      south[i].bar_conc = COAST ;
    }
  }

  return 0;

}

/* Zero out the information in the temporary data files for ssmi
      average computation */
/* Robert Grumbine 15 December 1994 */

int ice_zero(ssmi_tmp *north_tmp, ssmi_tmp *south_tmp,
              const int north_pts, const int south_pts)
{
  int i, j;
  ssmi_tmp blank;

  blank.t19v = 0;
  blank.t19h = 0;
  blank.t22v = 0;
  blank.t37v = 0;
  blank.t37h = 0;
  blank.t85v = 0;
  blank.t85h = 0;
  blank.conc_bar = NO_DATA;
  blank.count    = 0;

  j = north_pts;
  for (i = 0; i < j; i++)
  {
    north_tmp[i] = blank;
  }

  j = south_pts;
  for (i = 0; i < j; i++)
  {
    south_tmp[i] = blank;
  }

  return 0;

}



/* Perform extended weather filtering (per OMB Tech Note 120) on
     SSMI data for sea ice use */
/* Robert Grumbine 4 June 1997 */

float gr37(const ssmi *map, const int i, const int j,
           const int nx, const int ny, const int range);
float gr22(const ssmi *map, const int i, const int j,
           const int nx, const int ny, const int range);

int newfilt(ssmi *nmap, ssmi *smap) {
  int i, j;
  float *g37;
  int debug;
  unsigned char *nconc, *sconc;
  int index, indexip1, indexim1, indexjp1, indexjm1;

  debug = (0 == 1);
  #ifdef VERBOSE
  printf("entered newfilt\n"); fflush(stdout);
  #endif

  nconc = malloc(sizeof(unsigned char)*NX_NORTH*NY_NORTH);
  sconc = malloc(sizeof(unsigned char)*NX_SOUTH*NY_SOUTH);
  g37 = malloc(sizeof(float)*NX_NORTH*NY_NORTH);

  getfld(nmap, NX_NORTH*NY_NORTH, nconc, g37, BAR_CONC);
  getfld(smap, NX_SOUTH*NY_SOUTH, sconc, g37, BAR_CONC);
  #ifdef VERBOSE
    printf("in newfilt returned from getfld\n"); fflush(stdout);
  #endif


/* Find the northern hemisphere gradient ratio */
  for (j = 0; j < NY_NORTH  ; j++) {
    for (i = 0; i < NX_NORTH  ; i++) {
      index = i+j*NX_NORTH;
      #ifdef VERBOSE2
      printf("index = %d\n", index); fflush(stdout);
      #endif
      g37[index] = gr37(nmap, i, j, NX_NORTH, NY_NORTH, 0);
    }
  }
/* Loop over all points.  If, in any case, the 2 pt averaged gradient ratio
     for 37-19 v is greater than the cut off, then filter out the ice
     concentration */

  for (j = 1; j < NY_NORTH - 1 ; j++) {
    for (i = 1; i < NX_NORTH - 1 ; i++) {
      index = i+j*NX_NORTH;
      indexip1 = index+1;
      indexim1 = index-1;
      indexjp1 = index+NX_NORTH;
      indexjm1 = index-NX_NORTH;

      if (nconc[index] != 0 && nconc[index] != BAD_DATA) {

        if (nconc[indexjp1] != BAD_DATA) {
          if (g37[indexjp1] + g37[index] > 2*GR37LIM ) {
            nconc[index] = (float) WEATHER;
            if (debug) printf("1 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (nconc[indexjm1] != BAD_DATA) {
          if (g37[indexjm1] + g37[index] > 2*GR37LIM ) {
            nconc[index] = (float) WEATHER;
            if (debug) printf("2 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (nconc[indexim1] != BAD_DATA) {
          if (g37[indexim1] + g37[index] > 2*GR37LIM ) {
            nconc[index] = (float) WEATHER;
            if (debug) printf("3 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (nconc[indexip1] != BAD_DATA) {
          if (g37[indexip1] + g37[index] > 2*GR37LIM ) {
            nconc[index] = (float) WEATHER;
            if (debug) printf("4 resetting %3d %3d \n",i,j);
            continue;
          }
        }

      } /* End of filtration testing */

    }
  }

  for (index = 0; index < NY_NORTH*NX_NORTH; index++) {
     nmap[index].bar_conc = nconc[index];
  }

/* Need to put southern filtering in here */
  for (j = 0; j < NY_SOUTH  ; j++) {
    for (i = 0; i < NX_SOUTH  ; i++) {
      index = i+j*NX_SOUTH;
      g37[index] = gr37(smap, i, j, NX_SOUTH, NY_SOUTH, 0);
    }
  }
/* Note above that we've used the same array for both north and south
   gradients */
  for (j = 1; j < NY_SOUTH - 1; j++) {
    for (i = 1; i < NX_SOUTH - 1; i++) {
      index = i+j*NX_SOUTH;
      indexip1 = index+1;
      indexim1 = index-1;
      indexjp1 = index+NX_SOUTH;
      indexjm1 = index-NX_SOUTH;
      #ifdef VERBOSE2
        printf("south, i, j, index = %d %d %d\n",i,j,index); fflush(stdout);
      #endif

      if (sconc[index] != 0 && sconc[index] != BAD_DATA) {

        if (sconc[indexjp1] != BAD_DATA) {
          if (g37[indexjp1] + g37[index] > 2*GR37LIM ) {
            sconc[index] = (float) WEATHER;
            if (debug) printf("1 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (sconc[indexjm1] != BAD_DATA) {
          if (g37[indexjm1] + g37[index] > 2*GR37LIM ) {
            sconc[index] = (float) WEATHER;
            if (debug) printf("2 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (sconc[indexim1] != BAD_DATA) {
          if (g37[indexim1] + g37[index] > 2*GR37LIM ) {
            sconc[index] = (float) WEATHER;
            if (debug) printf("3 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (sconc[indexip1] != BAD_DATA) {
          if (g37[indexip1] + g37[index] > 2*GR37LIM ) {
            sconc[index] = (float) WEATHER;
            if (debug) printf("4 resetting %3d %3d \n",i,j);
            continue;
          }
        }

      } /* End of filtration testing */

    }
  }
  #ifdef VERBOSE
  printf("finished filtration loop\n"); fflush(stdout);
  #endif

  for (j = 0; j < NY_SOUTH*NX_SOUTH; j++) {
     smap[j].bar_conc = sconc[j];
  }
  #ifdef VERBOSE
  printf("newfilt finished smap loop\n"); fflush(stdout);
  #endif

  free(sconc);
  free(nconc);
  free(g37);
  #ifdef VERBOSE
  printf("about to return from newfilt\n"); fflush(stdout);
  #endif
  return 0;
}

float gr37(const ssmi *map, const int i, const int j,
                            const int nx, const int ny, const int range)
{
   int index, ti, tj, count;
   float t19v, t37v, tempor;

   #ifdef VERBOSE2
   printf("in gr37, i, j, nx, ny, range = %d %d %d %d  %d\n",i,j,nx,ny,range);
   fflush(stdout);
   #endif

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

