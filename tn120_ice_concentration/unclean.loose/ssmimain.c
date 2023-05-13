#include <stdio.h>
#include <stdlib.h>
#include <math.h> 
#ifdef CRAY
  #include <macros.h>
#elif LINUX
  #include <f2c.h>
#elif IBM
  #include <stdlib.h>
  #include <macros.h>
#endif
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
#ifndef IBM
  #define TRUE ( 1 == 1 ) 
  #define FALSE ( 1 == 0 ) 
#endif

/* Main program for decoding the ssmi orbital records. */
/* Read in (process_ and check_ procedures) SDR data. */
/* Remap to sea ice grid -- 25.4 km, true at 60, 80 W (North hemisphere)
    100 E (Southern hemisphere) -- ice_ procedures */
/* Compute sea ice concentrations -- nasa_team algorithm */
/* Robert Grumbine 18 December 1994 */
/* revised to handle flat files derived from bufr 6/97 */


int main(int argc, char *argv[])
{
  bufr_line  a_bufr_line;
  FILE *fin, *input, *outn, *outs, *outnc1, *nland, *outsc1, *sland;
  FILE *nraw, *sraw;
  char fname[80];

  ssmi *south, *north;
  ssmi_tmp *north_tmp, *south_tmp;
  float  rfld_n[NY_NORTH][NX_NORTH];
  float  rfld_s[NY_SOUTH][NX_SOUTH];
  unsigned char  fld_n[NY_NORTH][NX_NORTH];
  unsigned char  fld_s[NY_SOUTH][NX_SOUTH];

  int north_pts, south_pts;
  int i, j, nerrs, nproc;
  int jday, satno;
  int leap, ref_year, day_yest;

/* Mallocate the ssmi arrays */
  #ifdef VERBOSE
    printf("Size of ssmi_tmp, ssmi %d %d\n",sizeof(ssmi_tmp), sizeof(ssmi) );
  #endif
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
  outnc1 = fopen(argv[8], "w");
  outsc1 = fopen(argv[9], "w");
  if (fin == NULL || outn == NULL || outs == NULL || 
      outnc1 == NULL || nland == NULL || outsc1 == NULL || sland == NULL)
  {
    printf("Failed to open one of the seven files!\n");
    if (fin == NULL ) {printf(" - input file\n");}
    if (outn == NULL ) {printf(" - north out \n");}
    if (outs == NULL ) {printf(" - south out \n");}
    if (outnc1 == NULL ) {printf(" - north char out \n");}
    if (outsc1 == NULL ) {printf(" - south char out \n");}
    if (nland == NULL ) {printf(" - north land in \n");}
    if (sland == NULL ) {printf(" - south land in \n");}
    return -1;
  }

  jday   = atoi(argv[10]);
  printf("Julian day to get data for %d\n",jday);
  ref_year = atoi(argv[11]);
  satno    = atoi(argv[12]);

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
    printf("%s\n", fname);
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
        j = 0;
        while (!feof(input) )
        {  
          #ifdef VERBOSE2
            printf("Process record number %d\n",j); fflush(stdout); 
          #endif
          j++;
          fread(&a_bufr_line, sizeof(bufr_line), 1, input);
          if (a_bufr_line.satno == satno) {
            nerrs = process_bufr(&a_bufr_line);
            if (nerrs != 0) {
              printf("%2d errors in record %6d of orbit %2d\n",nerrs, j, i);
              fflush(stdout);
            }
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

/* Fill in pole for output purposes */
  getfld(north, north_pts, &fld_n[0][0], &rfld_n[0][0], BAR_CONC);
  getfld(south, south_pts, &fld_s[0][0], &rfld_s[0][0], BAR_CONC);
  pole_fill(&fld_n[0][0], dx, dy, NX_NORTH, NY_NORTH, polei_NORTH, polej_NORTH, MAX_LATITUDE);
  pole_fill(&fld_s[0][0], dx, dy, NX_SOUTH, NY_SOUTH, polei_SOUTH, polej_SOUTH, MAX_LATITUDE);
  fwrite(fld_n, sizeof(unsigned char), north_pts, nraw);
  fwrite(fld_s, sizeof(unsigned char), south_pts, sraw);

/* Mask out the land now */
  fread(fld_n, sizeof(unsigned char), north_pts, nland);
  fread(fld_s, sizeof(unsigned char), south_pts, sland);
  ice_mask(north, south, north_pts, south_pts, &fld_n[0][0], &fld_s[0][0]);

/* Write out full ssmi and concentration data files */
  fwrite(north, sizeof(ssmi), north_pts, outn);
  fwrite(south, sizeof(ssmi), south_pts, outs);
  fclose(outn);
  fclose(outs);

  getfld(north, north_pts, &fld_n[0][0], &rfld_n[0][0], BAR_CONC);
  getfld(south, south_pts, &fld_s[0][0], &rfld_s[0][0], BAR_CONC);
  fwrite(fld_n, sizeof(unsigned char), north_pts, outnc1);
  fwrite(fld_s, sizeof(unsigned char), south_pts, outsc1); 
  fclose(outnc1);
  fclose(outsc1);

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
