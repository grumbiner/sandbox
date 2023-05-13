#include <stdio.h>
#include <stdlib.h>
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

int main(int argc, char *argv[])
{
  bufr_line  a_bufr_line;
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
  int i, j, nerrs, nproc, nrecs;
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
      /* Warning: Does not handle year end and leap years properly */
/*      if ( (dayin == day_yest && hrin >= 24 - DATA_WINDOW - 1) ||
           (dayin == jday     && hrin <= DATA_WINDOW)    ) {
        printf("Orbit used: %s\n",date);
*/
        j = 0;
/*        while (!feof(input) && j <= RECS_PER_ORBIT ) */
        while (!feof(input) )
        {  
/*          printf("Process record number %d\n",j); fflush(stdout); */
          j++;
/*          fread(buffer, sizeof(char), REC_LENGTH, input);
          nerrs = process_data(&buffer[0], &a);
*/
          fread(&a_bufr_line, sizeof(bufr_line), 1, input);
          nerrs = process_bufr(&a_bufr_line);
          if (nerrs != 0) {
            printf("%2d errors in record %4d of orbit %2d\n",nerrs, j, i);
            fflush(stdout);
          }
/*          ice_add_data(north_tmp, south_tmp, &a_bufr_line); */ 
          ice_add_bufr(north_tmp, south_tmp, &a_bufr_line); 
        }
/*      } */       /* End of if clause on date */
/*
      else {
        printf("Orbit not used: %s %d recs\n",date, nrecs); fflush(stdout);
        for (j = 0; j < nrecs; j++ ) {
          fread(buffer, sizeof(char), REC_LENGTH, input);
        }
      }
*/  /* End of else case on date check */
      
  
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
#include <stdio.h>
#include <math.h>
#include "icessmi.h"
#include "icegrids.h"

/* Perform extended weather filtering (per OMB Tech Note 120) on
     SSMI data for sea ice use */
/* Robert Grumbine 4 June 1997 */
 
float gr37(const ssmi *map, const int i, const int j, 
           const int nx, const int ny, const int range);
float gr22(const ssmi *map, const int i, const int j, 
           const int nx, const int ny, const int range);


int newfilt(ssmi *nmap, ssmi *smap)
{
  int i, j;
  float t19v, t19h, t22v, t37v, t37h, t85v, t85h;
  float g37[NY_NORTH][NX_NORTH];
  int debug;
  unsigned char nconc[NY_NORTH][NX_NORTH], sconc[NY_SOUTH][NX_SOUTH];
  
  debug = (0 == 1);

  getfld(nmap, NX_NORTH*NY_NORTH, &nconc[0][0], &g37[0][0], BAR_CONC);
  getfld(smap, NX_SOUTH*NY_SOUTH, &sconc[0][0], &g37[0][0], BAR_CONC);

/* Find the northern hemisphere gradient ratio */
  for (j = 0; j < NY_NORTH  ; j++) {
    for (i = 0; i < NX_NORTH  ; i++) {
      g37[j][i] = gr37(nmap, i, j, NX_NORTH, NY_NORTH, 0);
    }
  }
/* Loop over all points.  If, in any case, the 2 pt averaged gradient ratio 
     for 37-19 v is greater than the cut off, then filter out the ice 
     concentration */

  for (j = 1; j < NY_NORTH - 1 ; j++) {
    for (i = 1; i < NX_NORTH - 1 ; i++) {
      if (nconc[j][i] != 0 && nconc[j][i] != BAD_DATA) {

        if (nconc[j+1][i] != BAD_DATA) {
          if (g37[j+1][i] + g37[j][i] > 2*GR37LIM ) {
            nconc[j][i] = (float) WEATHER;
            if (debug) printf("1 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (nconc[j-1][i] != BAD_DATA) {
          if (g37[j-1][i] + g37[j][i] > 2*GR37LIM ) {
            nconc[j][i] = (float) WEATHER;
            if (debug) printf("2 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (nconc[j][i-1] != BAD_DATA) {
          if (g37[j][i-1] + g37[j][i] > 2*GR37LIM ) {
            nconc[j][i] = (float) WEATHER;
            if (debug) printf("3 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (nconc[j][i+1] != BAD_DATA) {
          if (g37[j][i+1] + g37[j][i] > 2*GR37LIM ) {
            nconc[j][i] = (float) WEATHER;
            if (debug) printf("4 resetting %3d %3d \n",i,j);
            continue;
          }
        }
      
      } /* End of filtration testing */

    }
  }

  for (j = 0; j < NY_NORTH; j++) {
    for (i = 0; i < NX_NORTH; i++) {
       nmap[j*NX_NORTH +i].bar_conc = nconc[j][i];
    }
  }

/* Need to put southern filtering in here */
  for (j = 0; j < NY_SOUTH  ; j++) {
    for (i = 0; i < NX_SOUTH  ; i++) {
      g37[j][i] = gr37(smap, i, j, NX_SOUTH, NY_SOUTH, 0);
    }
  }
/* Note above that we've used the same array for both north and south
   gradients */
  for (j = 1; j < NY_SOUTH - 1; j++) {
    for (i = 1; i < NX_SOUTH - 1; i++) {
      if (sconc[j][i] != 0 && sconc[j][i] != BAD_DATA) {

        if (sconc[j+1][i] != BAD_DATA) {
          if (g37[j+1][i] + g37[j][i] > 2*GR37LIM ) {
            sconc[j][i] = (float) WEATHER;
            if (debug) printf("1 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (sconc[j-1][i] != BAD_DATA) {
          if (g37[j-1][i] + g37[j][i] > 2*GR37LIM ) {
            sconc[j][i] = (float) WEATHER;
            if (debug) printf("2 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (sconc[j][i-1] != BAD_DATA) {
          if (g37[j][i-1] + g37[j][i] > 2*GR37LIM ) {
            sconc[j][i] = (float) WEATHER;
            if (debug) printf("3 resetting %3d %3d \n",i,j);
            continue;
          }
        }
        if (sconc[j][i+1] != BAD_DATA) {
          if (g37[j][i+1] + g37[j][i] > 2*GR37LIM ) {
            sconc[j][i] = (float) WEATHER;
            if (debug) printf("4 resetting %3d %3d \n",i,j);
            continue;
          }
        }
      
      } /* End of filtration testing */

    }
  }

  for (j = 0; j < NY_SOUTH; j++) {
    for (i = 0; i < NX_SOUTH; i++) {
       smap[j*NX_SOUTH + i].bar_conc = sconc[j][i];
    }
  }

  return 0;
}

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

/* Routine to fill in the unobserved polar gap */
/* Use a laplacean fill on the principle that the true field
   there is the smoothest (gradient sense) possible */
/* Robert Grumbine 4 June 1997 */

int pole_fill(unsigned char *map, const int pole)
{
    int iter, i, j;
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
#include "icessmi.h"

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
#include <stdio.h>
#include "ssmi.h"
#include "icessmi.h"
#include "icegrids.h"

int ice_add_bufr(ssmi_tmp *north_tmp, ssmi_tmp  *south_tmp, 
                 bufr_line *a)
{
/* Version of the SDR ice_add_data suited to work with BUFR input */ 

  int j, n_north, n_south;
  float tlat, tlon;
  float t19v, t19h, t22v, t37v, t37h, t85v, t85h;
  float nasa;
  int ilat, jlon, stype, index;

/* Peel off data from the record.  Transfer to physical units. */
  for (j = 0; j < NSCANS; j++) {
    tlat =  ( a->full[j].latitude);
    if (tlat > -25. && tlat < 20. ) continue;

    tlon = ( (float)a->full[j].longitude);
    t19v = ( (float)a->full[j].t19v );
    t19h = ( (float)a->full[j].t19h );
    t22v = ( (float)a->full[j].t22v );
    t37v = ( (float)a->full[j].t37v );
    t37h = ( (float)a->full[j].t37h );
    t85v = ( (float)a->full[j].t85v );
    t85h = ( (float)a->full[j].t85h );
    stype = a->full[j].surface_type;
    nasa = 0.;


    if ((t19v == 0.) && (tlat == 0.) && (tlon == 0.) ) {
      printf("no good tb\n"); 
      if (tlat > 0.) {
        mapll(tlat, tlon, &ilat, &jlon, xorig_NORTH, yorig_NORTH,
              eccen2, slat_NORTH, slon_NORTH, rearth, dx, dy, sgn_NORTH);
        if ( (ilat >= 0) && (ilat <  (NX_NORTH) ) && 
             (jlon >= 0) && (jlon <  (NY_NORTH) ) ) {
          index = ilat + jlon*(NX_NORTH);
          north_tmp[index].count = 0;
          north_tmp[index].conc_bar = BAD_DATA;
        }
      }
      else {
        mapll(tlat, tlon, &ilat, &jlon, xorig_SOUTH, yorig_SOUTH,
              eccen2, slat_SOUTH, slon_SOUTH, rearth, dx, dy, sgn_SOUTH);
        if (ilat >= 0 && ilat < NX_SOUTH && jlon >= 0 && jlon < NY_SOUTH) {
          index = ilat + jlon*(NX_SOUTH);
          south_tmp[index].count = 0;
          south_tmp[index].conc_bar = BAD_DATA;
        }
      }
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
          north_tmp[index].t19v += t19v; 
          north_tmp[index].t19h += t19h; 
          north_tmp[index].t22v += t22v; 
          north_tmp[index].t37v += t37v; 
          north_tmp[index].t37h += t37h; 
          north_tmp[index].t85v += t85v; 
          north_tmp[index].t85h += t85h; 
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
          south_tmp[index].t19v += t19v; 
          south_tmp[index].t19h += t19h; 
          south_tmp[index].t22v += t22v; 
          south_tmp[index].t37v += t37v; 
          south_tmp[index].t37h += t37h; 
          south_tmp[index].t85v += t85v; 
          south_tmp[index].t85h += t85h; 
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


  } /* end for loop */

  return 0;

}

int ice_add_data(ssmi_tmp *north_tmp, ssmi_tmp  *south_tmp, 
                 struct data_record *a)
{

  int j, n_north, n_south;
  float tlat, tlon;
  float t19v, t19h, t22v, t37v, t37h, t85v, t85h;
  float nasa;
  int ilat, jlon, stype, index;

/* Peel off data from the record.  Transfer to physical units. */
  for (j = 0; j < NSCANS; j++) {
    tlat = ( (float)a->data.full[j].latitude);
    if (tlat > -25. && tlat < 20. ) continue;

    tlon = ( (float)a->data.full[j].longitude);
    t19v = ( (float)a->data.full[j].t19v );
    t19h = ( (float)a->data.full[j].t19h );
    t22v = ( (float)a->data.full[j].t22v );
    t37v = ( (float)a->data.full[j].t37v );
    t37h = ( (float)a->data.full[j].t37h );
    t85v = ( (float)a->data.full[j].t85v );
    t85h = ( (float)a->data.full[j].t85h );
    stype = a->data.full[j].surface_type;
    nasa = 0.;


    if ((t19v == 0.) && (tlat == 0.) && (tlon == 0.) ) {
      printf("no good tb\n"); 
      if (tlat > 0.) {
        mapll(tlat, tlon, &ilat, &jlon, xorig_NORTH, yorig_NORTH,
              eccen2, slat_NORTH, slon_NORTH, rearth, dx, dy, sgn_NORTH);
        if ( (ilat >= 0) && (ilat <  (NX_NORTH) ) && 
             (jlon >= 0) && (jlon <  (NY_NORTH) ) ) {
          index = ilat + jlon*(NX_NORTH);
          north_tmp[index].count = 0;
          north_tmp[index].conc_bar = BAD_DATA;
        }
      }
      else {
        mapll(tlat, tlon, &ilat, &jlon, xorig_SOUTH, yorig_SOUTH,
              eccen2, slat_SOUTH, slon_SOUTH, rearth, dx, dy, sgn_SOUTH);
        if (ilat >= 0 && ilat < NX_SOUTH && jlon >= 0 && jlon < NY_SOUTH) {
          index = ilat + jlon*(NX_SOUTH);
          south_tmp[index].count = 0;
          south_tmp[index].conc_bar = BAD_DATA;
        }
      }
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
          north_tmp[index].t19v += (int) (t19v*100 + 0.5); 
          north_tmp[index].t19h += (int) (t19h*100 + 0.5); 
          north_tmp[index].t22v += (int) (t22v*100 + 0.5); 
          north_tmp[index].t37v += (int) (t37v*100 + 0.5); 
          north_tmp[index].t37h += (int) (t37h*100 + 0.5); 
          north_tmp[index].t85v += (int) (t85v*100 + 0.5); 
          north_tmp[index].t85h += (int) (t85h*100 + 0.5); 
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
          south_tmp[index].t19v += (int) (t19v*100 + 0.5); 
          south_tmp[index].t19h += (int) (t19h*100 + 0.5); 
          south_tmp[index].t22v += (int) (t22v*100 + 0.5); 
          south_tmp[index].t37v += (int) (t37v*100 + 0.5); 
          south_tmp[index].t37h += (int) (t37h*100 + 0.5); 
          south_tmp[index].t85v += (int) (t85v*100 + 0.5); 
          south_tmp[index].t85h += (int) (t85h*100 + 0.5); 
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


  } /* end for loop */

  return 0;

}

/* Average (as needed) the brightness temperatures and ice concentrations
     on the sea ice grid. */
/* Compute the ice concentration with the averaged ice temperatures (a1 
     files) */
/* Robert Grumbine 10 February 1995 */

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
    }
    else {
      north[i].t19v = (int) (0.5 + 100.*(float) north_tmp[i].t19v / \
                                        (float) north_tmp[i].count);
      north[i].t19h = (int) (0.5 + 100.*(float) north_tmp[i].t19h / \
                                        (float) north_tmp[i].count);
      north[i].t22v = (int) (0.5 + 100.*(float) north_tmp[i].t22v / \
                                        (float) north_tmp[i].count);
      north[i].t37v = (int) (0.5 + 100.*(float) north_tmp[i].t37v / \
                                        (float) north_tmp[i].count);
      north[i].t37h = (int) (0.5 + 100.*(float) north_tmp[i].t37h / \
                                        (float) north_tmp[i].count);
      north[i].t85v = (int) (0.5 + 100.*(float) north_tmp[i].t85v / \
                                        (float) north_tmp[i].count);
      north[i].t85h = (int) (0.5 + 100.*(float) north_tmp[i].t85h / \
                                        (float) north_tmp[i].count);
      north[i].conc_bar = (int) (0.5 + (float) north_tmp[i].conc_bar / 
                                       (float) north_tmp[i].count);
      north[i].bar_conc = 0.5 + nasa_team(
        (float)north[i].t19v / 100.,
        (float)north[i].t19h / 100.,
        (float)north[i].t22v / 100.,
        (float)north[i].t37v / 100.,
        (float)north[i].t37h / 100.,
        (float)north[i].t85v / 100.,
        (float)north[i].t85h / 100., 'n', 1, 1
      ); 
/*      printf("averaged data, %4d, %5d %5d %5d %5d %3d %3d %5d\n",
        i, north[i].t19v, north[i].t19h, north[i].t22v, north[i].t37v,
        north[i].conc_bar, north[i].bar_conc, 
        north[i].conc_bar - north[i].bar_conc);
*/
      
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
    }
    else {
      south[i].t19v = (int) (0.5 + 100.*(float) south_tmp[i].t19v / 
                                        (float) south_tmp[i].count);
      south[i].t19h = (int) (0.5 + 100.*(float) south_tmp[i].t19h /
                                        (float)  south_tmp[i].count);
      south[i].t22v = (int) (0.5 + 100.*(float) south_tmp[i].t22v / 
                                        (float) south_tmp[i].count);
      south[i].t37v = (int) (0.5 + 100.*(float) south_tmp[i].t37v / 
                                        (float) south_tmp[i].count);
      south[i].t37h = (int) (0.5 + 100.*(float) south_tmp[i].t37h / 
                                        (float) south_tmp[i].count);
      south[i].t85v = (int) (0.5 + 100.*(float) south_tmp[i].t85v / 
                                        (float) south_tmp[i].count);
      south[i].t85h = (int) (0.5 + 100.*(float) south_tmp[i].t85h / 
                                        (float) south_tmp[i].count);
      south[i].conc_bar = (int) (0.5 + (float)south_tmp[i].conc_bar / 
                                       (float)south_tmp[i].count);
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

int process_bufr(bufr_line *b)
/* Process the bufr data records, which will eventually include the short 
    data as well. 
   Only processing is to check for qc purposes.
*/
{
  return check_bufr(b);
}

int process_short_bufr(short_bufr *c)
/* Process the short data record */
{
  return check_short_bufr(c);
}


/* Perform some bounds checking for the long data */
/* Order of checking is related to the frequency with which
   a given field is erroneous */
/* Note that typically if one number is wrong, so are several others */
/* Robert Grumbine 10 February 1994 */

int check_bufr(bufr_line *b)
{
  int nerr = 0;
  int i;

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
  
  } /* end checking */

  return nerr;
  
}

void zero_bufr(bufr_line *b, int i)
{
   if ( ! (
     ((int) b->full[i].scan_counter == 0 ) &&
     ( b->full[i].latitude     == 0 ) &&
     ( b->full[i].longitude    == 0 ) &&
     ( b->full[i].t19v         == 0 ) &&
     ( b->full[i].t19h         == 0 ) &&
     ( b->full[i].t22v         == 0 ) &&
     ( b->full[i].t37v         == 0 ) &&
     ( b->full[i].t37h         == 0 ) &&
     ( b->full[i].t85v         == 0 ) &&
     ( b->full[i].t85h         == 0 ) &&
     ( b->full[i].surface_type == 0 ) &&
     ( b->full[i].position_num == 0 )   ) )
   {
/*     printf("Bad = ");
     printf("%5d %5d %5d %5d %5d %5d %5d %5d %5d %5d %3d %3d\n",
     b->full[i].scan_counter ,
     b->full[i].latitude     ,
     b->full[i].longitude    ,
     b->full[i].t19v         ,
     b->full[i].t19h         ,
     b->full[i].t22v         ,
     b->full[i].t37v         ,
     b->full[i].t37h         ,
     b->full[i].t85v         ,
     b->full[i].t85h         ,
     b->full[i].surface_type ,
     b->full[i].position_num  );
*/
   }


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
      c->position_num = 0;
  }
  if (c->longitude > 360.) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
      c->position_num = 0;
  }
  if ( c->t85v > 295.0 ||  c->t85h > 295.0) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
      c->position_num = 0;
  }
  if ( c->t85v < 150.0 ) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
      c->position_num = 0;
  }
  if ( c->t85h < 125.0 ) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
      c->position_num = 0;
  }

  return nerr;
  
}
#include <stdio.h>
#include <stdlib.h>
#include "ssmi.h"
#include "icessmi.h"

void zero_long(struct long_data *b);

int process_data(const char *buffer, struct data_record *a)
/* Procedure to take the REC_LENGTH bytes from the buffer and
     transfer them into the data record pointed to by a.  The 
     indirection is used so that the main program can decide
     how to deal with the multiple data records which will 
     exist. */

{
  int locate, j, nerrs;
  struct long_data b;
  struct short_data c;

  a->header.block_length = 256*buffer[0]+ buffer[1];
  a->header.mode         = buffer[2];
  a->header.submode      = buffer[3];
  a->header.scan_counter = 256*buffer[4]+buffer[5];
  a->header.b_scan_start = ((256*buffer[6]+buffer[7])*256+buffer[8])*256+
                                    buffer[9];
  a->header.checksum     = 256*buffer[10]+buffer[11];

  a->data.block_length  = 256*buffer[12] + buffer[13];
  a->data.mode          = buffer[14];
  a->data.submode       = buffer[15];

  locate = 16;
  nerrs = 0;

  for(j = 0; j < NSCANS; j++)
  {
     nerrs += process_long(buffer, &locate, &b);
     a->data.full[j] = b;
  }
  
/* If any errors were found on the scan line, zero out the entire set */
  if (nerrs != 0) {
    zero_long(&b);
    for (j = 0; j < NSCANS; j++)
    {
      a->data.full[j] = b;
    }
  }

 return nerrs;

}


int process_header(const char *buffer, char *date, int *nrecs)
{
  struct sdr_scan_header a;

/*  FILE *tmp;                                     */
/*  tmp = fopen("ssmitmp","w");                    */
/*  fwrite(buffer, sizeof(char), REC_LENGTH, tmp); */
/*  fread(&a, sizeof(a), 1, tmp);                  */ 
/*  printf("size of scan_header %d\n",sizeof(a));  */
/*  The above will not work because the sizes assigned by some compilers
    for sub-elements of the struct are not equal to the byte counts in
    the structs.  Will need to manually equate elements 
    Robert Grumbine 10 December 1994 */
/* Wish: return the filled structure for later consideration */
/* Wish: do a check on the header data */

/* Begin the equating */
  a.rev_header2.julian_begin = 256*buffer[660] + buffer[661];
  a.rev_header2.hour_begin   = buffer[662];
  a.rev_header2.min_begin    = buffer[663];
  a.rev_header2.sec_begin    = buffer[664];
  a.rev_header2.julian_end = 256*buffer[665] + buffer[666];
  a.rev_header2.hour_end   = buffer[667];
  a.rev_header2.min_end    = buffer[668];
  a.rev_header2.sec_end    = buffer[669];
  a.rev_header2.julian_ascend = 256*buffer[670] + buffer[671];
  a.rev_header2.hour_ascend   = buffer[672];
  a.rev_header2.min_ascend    = buffer[673];
  a.rev_header2.sec_ascend    = buffer[674];


  printf("\nInformation from rev_header_2\n");

  printf("Begin orbit on %4d %2d %2d %2d\n",a.rev_header2.julian_begin,
                                            a.rev_header2.hour_begin, 
                                            a.rev_header2.min_begin, 
                                            a.rev_header2.sec_begin);
  printf("End orbit on   %4d %2d %2d %2d\n",a.rev_header2.julian_end,
                                            a.rev_header2.hour_end, 
                                            a.rev_header2.min_end, 
                                            a.rev_header2.sec_end);
  printf("Ascending node %4d %2d %2d %2d\n",a.rev_header2.julian_ascend,
                                            a.rev_header2.hour_ascend, 
                                            a.rev_header2.min_ascend, 
                                            a.rev_header2.sec_ascend);


  printf("Number of data blocks (sdr dsb) %d\n", 256*buffer[42]+buffer[43]);
  *nrecs = 256*buffer[42]+buffer[43];

  printf("Info from low bytes %4d %2d %2d %2d %2d  %4d\n",
          256*buffer[20]+buffer[21], buffer[22], buffer[23], buffer[24],
          buffer[25], *nrecs);

  sprintf(date,"%03d%02d%02d%02d\0",a.rev_header2.julian_begin,
                                            a.rev_header2.hour_begin, 
                                            a.rev_header2.min_begin, 
                                            a.rev_header2.sec_begin);
  return 1;

}

int process_long(const char *buffer, int *locate, struct long_data *b)
/* Process the long data records, which include the short data as 
     well. 
   Locate points to the first byte of the long record.
*/
{
  struct short_data c;
  int posit;

  posit = *locate;

  b->scan_counter = 256*buffer[posit   ] + buffer[posit+1];
  posit += 2;

  b->latitude     = 256*buffer[posit   ] + buffer[posit+1];
  b->longitude    = 256*buffer[posit +2] + buffer[posit+3];
  b->t19v         = 256*buffer[posit +4] + buffer[posit+5];
  b->t19h         = 256*buffer[posit +6] + buffer[posit+7];
  b->t22v         = 256*buffer[posit +8] + buffer[posit+9];
  b->t37v         = 256*buffer[posit+10] + buffer[posit+11];
  b->t37h         = 256*buffer[posit+12] + buffer[posit+13];
  b->t85v         = 256*buffer[posit+14] + buffer[posit+15];
  b->t85h         = 256*buffer[posit+16] + buffer[posit+17];
  b->surface_type = buffer[posit+18];
  b->position_num = buffer[posit+19];

  posit  += 20;
  process_short(buffer, &posit, &c);
  b->short_rec[0] = c;
  process_short(buffer, &posit, &c);
  b->short_rec[1] = c;
  process_short(buffer, &posit, &c);
  b->short_rec[2] = c;

  *locate = posit;

  return check_long(b);
}

int process_short(const char *buffer, int *locate, struct short_data *c)
/* Process the short data record */
{

  int posit;

  posit = *locate;

  c->latitude     = 256*buffer[posit  ] + buffer[posit+1];
  c->longitude    = 256*buffer[posit+2] + buffer[posit+3];
  c->t85v         = 256*buffer[posit+4] + buffer[posit+5];
  c->t85h         = 256*buffer[posit+6] + buffer[posit+7];
  c->surface_type = buffer[posit+8];
  c->position_num = buffer[posit+9];

  *locate += 10;

  return check_short(c);
}
/* Perform some bounds checking for the long data */
/* Order of checking is related to the frequency with which
   a given field is erroneous */
/* Note that typically if one number is wrong, so are several others */
/* Robert Grumbine 10 February 1994 */

int check_long(struct long_data *b)
{
  int nerr = 0;

  if ((int) b->surface_type > 8 ) {
    nerr += 1;
    zero_long(b);
    return nerr;
  }

  if ((int) b->t19h > 295*100 || (int) b->t19h <  75*100) {
    nerr += 1;
    zero_long(b);
    return nerr;
  } 
  if ((int) b->t19v > 295*100 || (int) b->t19v < 150*100) {
    nerr += 1;
    zero_long(b);
    return nerr;
  } 

  if ((int) b->t22v > 295*100 || (int) b->t22v < 150*100) {
    nerr += 1;
    zero_long(b);
    return nerr;
  } 

  if ((int) b->t37h > 295*100 || (int) b->t37h < 100*100) {
    nerr += 1;
    zero_long(b);
    return nerr;
  } 
  if ((int) b->t37v > 295*100 || (int) b->t37v < 150*100) {
    nerr += 1;
    zero_long(b);
    return nerr;
  } 

  if ((int) b->t85h > 295*100 || (int) b->t85h < 125*100) {
    nerr += 1;
    zero_long(b);
    return nerr;
  } 
  if ((int) b->t85v > 295*100 || (int) b->t85v < 150*100) {
    nerr += 1;
    zero_long(b);
    return nerr;
  } 

  if ((int) b->latitude > 18000) {
     nerr += 1;
     zero_long(b);
     return nerr;
  }
  if ((int) b->longitude > 36000) {
     nerr += 1;
     zero_long(b);
     return nerr;
  }

  return nerr;

}

void zero_long(struct long_data *b)
{
   if ( ! (
     ((int) b->scan_counter == 0 ) &&
     ((int) b->latitude     == 0 ) &&
     ((int) b->longitude    == 0 ) &&
     ((int) b->t19v         == 0 ) &&
     ((int) b->t19h         == 0 ) &&
     ((int) b->t22v         == 0 ) &&
     ((int) b->t37v         == 0 ) &&
     ((int) b->t37h         == 0 ) &&
     ((int) b->t85v         == 0 ) &&
     ((int) b->t85h         == 0 ) &&
     ((int) b->surface_type == 0 ) &&
     ((int) b->position_num == 0 )   ) )
   {
/*     printf("Bad = ");
     printf("%5d %5d %5d %5d %5d %5d %5d %5d %5d %5d %3d %3d\n",
     b->scan_counter ,
     b->latitude     ,
     b->longitude    ,
     b->t19v         ,
     b->t19h         ,
     b->t22v         ,
     b->t37v         ,
     b->t37h         ,
     b->t85v         ,
     b->t85h         ,
     b->surface_type ,
     b->position_num  );
*/
   }


   b->scan_counter = 0;
   b->latitude     = 0;
   b->longitude    = 0;
   b->t19v         = 0;
   b->t19h         = 0;
   b->t22v         = 0;
   b->t37v         = 0;
   b->t37h         = 0;
   b->t85v         = 0;
   b->t85h         = 0;
   b->surface_type = 0;
   b->position_num = 0;
  
  return;
}

/* Bounds checking on a short data record */
/* Robert Grumbine 1 March 1995 */

int check_short(struct short_data *c)
{
  int nerr = 0;

  if (c->latitude > 18000) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
      c->position_num = 0;
  }
  if (c->longitude > 36000) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
      c->position_num = 0;
  }
  if ((int) c->t85v > 295*100 || (int) c->t85h > 295*100) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
      c->position_num = 0;
  }
  if ((int) c->t85v < 150*100 ) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
      c->position_num = 0;
  }
  if ((int) c->t85h < 125*100 ) {
      nerr += 1;
      c->latitude     = 0;
      c->longitude    = 0;
      c->t85v         = 0;
      c->t85h         = 0;
      c->position_num = 0;
  }

  return nerr;
  
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
