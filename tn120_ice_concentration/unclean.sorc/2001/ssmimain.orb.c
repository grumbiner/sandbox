#include <stdio.h>
#include <stdlib.h>
#include "ssmi.h"
#include "icessmi.h"
#include "icegrids.h"

#define TRUE ( 1 == 1 )
#define FALSE ( 1 == 0 )

/* Main program for decoding the ssmi orbital records. */
/* Read in (process_ and check_ procedures) SDR data. */
/* Remap to sea ice grid -- 25.4 km, true at 60, 80 W (North hemisphere)
    100 E (Southern hemisphere) -- ice_ procedures */
/* Compute sea ice concentrations -- nasa_team algorithm */
/* Bob Grumbine 18 December 1994 */

int main(int argc, char *argv[])
{
  struct data_record  a;
  char buffer[REC_LENGTH];
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
  int jday, dayin, hrin, minin, secin;
  int leap, ref_year, day_yest;
  char date[10];

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
  outnc1 = fopen(argv[8], "w");
  outsc1 = fopen(argv[9], "w");
  if (fin == NULL || outn == NULL || outs == NULL || 
      outnc1 == NULL || nland == NULL || outsc1 == NULL || sland == NULL)
  {
    printf("Failed to open one of the seven files!\n");
    return -1;
  }
  jday   = atoi(argv[10]);
  printf("Julian day to get data for %d\n",jday);
  ref_year = atoi(argv[11]);

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
    fscanf(fin, "%s", fname);
    printf("%s\n", fname);
    input = fopen(fname, "r");
    if ( input == NULL ) {
       break;
    }
  

/* Begin stripping out header */
    fseek(input, (long)(0) , SEEK_SET);

/* Begin stripping out data */
    for (i = 0; i < nproc ; i++)
    while (!feof(input) )
    {

      fread(buffer, sizeof(char), REC_LENGTH, input);
      process_header(buffer, date);
      printf("Orbit data starts %s\n",date);
      sscanf(date,"%03d%02d%02d%02d\n",&dayin, &hrin, &minin, &secin); 
      printf("%3d %2d %2d %2d\n",dayin, hrin, minin, secin);

      /* Add data only if data is within window */
      /* Warning: Does not handle year end and leap years properly */
      if ( (dayin == day_yest && hrin >= 24 - DATA_WINDOW - 1) ||
           (dayin == jday     && hrin <= DATA_WINDOW)    ) {
        printf("Orbit used: %s\n",date);
        j = 1;
/*        while (!feof(input) && j <= RECS_PER_ORBIT ) */
        while (!feof(input) )
        {  
          j++;
          fflush(stdout);
          fread(buffer, sizeof(char), REC_LENGTH, input);
          nerrs = process_data(&buffer[0], &a);
          if (nerrs != 0) {
            printf("%2d errors in record %4d of orbit %2d\n",nerrs, j, i);
            fflush(stdout);
          }
          ice_add_data(north_tmp, south_tmp, &a); 
        }
      }
      else {
        printf("Orbit not used: %s\n",date);
            fflush(stdout);
/*        for (j = 1; j < RECS_PER_ORBIT; j++) 
        {
          fread(buffer, sizeof(char), REC_LENGTH, input);
        }
*/

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
  pole_fill(&fld_n[0][0], 1);
  pole_fill(&fld_s[0][0], 2);
  fwrite(fld_n, sizeof(unsigned char), north_pts, nraw);
  fwrite(fld_s, sizeof(unsigned char), south_pts, sraw);

/* Mask out the land now */
  fread(fld_n, sizeof(unsigned char), north_pts, nland);
  fread(fld_s, sizeof(unsigned char), south_pts, sland);
  ice_mask(north, south, north_pts, south_pts, &fld_n[0][0], &fld_s[0][0]);

/* Write out full ssmi and concentration data files */
  fwrite(north, sizeof(ssmi), north_pts, outn);
  fwrite(south, sizeof(ssmi), south_pts, outs);

  getfld(north, north_pts, &fld_n[0][0], &rfld_n[0][0], BAR_CONC);
  getfld(south, south_pts, &fld_s[0][0], &rfld_s[0][0], BAR_CONC);
  fwrite(fld_n, sizeof(unsigned char), north_pts, outnc1);
  fwrite(fld_s, sizeof(unsigned char), south_pts, outsc1); 

  return 0;

}
