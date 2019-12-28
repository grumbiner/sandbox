/* Program designed for the NIC to decode the raster file of SSMI ice
     concentration into an ASCII format with latitude, longitude, and
     ice concentration, one to a line.
   SSMI ice concentration as produced experimentally by Bob Grumbine
     using the experimental NASA-Team algorithm on F-11.
     This program will cap values of ice concentration at 100%.  It
     will not affect the land, no data, or bad data flags (157, 166, 224
     respectively).
   Mapping done using the Hughes algorithm as supplied by the NSIDC,
     written by V. Troisi, and translated to C by Bob Grumbine.
   Bob Grumbine 22 April 1994.

   Due to run-time memory requirements, this program may not execute on 
     an IBM compatible PC.  It will run without problem on a workstation
     or on a Macintosh.

   Usage: nictrim north_file_name south_file_name north_out south_out
     where the first name is the command name, the next pair are the
     input file names, and the last pair are the output file names. 

   Usage note: The output file size will be 16 times the input file size.
     For the current (22 April 1994) files, this means 4.8 Megabytes
     will be required for the two ASCII files (combined).
*/
/* Extensions 11 October 1994:
     Permit, through the file nic.h, the specification of a number of
       sub-regions.  Output will be sectioned along each of the regions.
     OTH-Gold format will be supported here.
     Output file names are created by the program, rather than on input
       due to the number of files (16 currently) involved.
*/

#include <stdio.h>
#include <math.h>
#include "ssmi.h"
#include "icessmi.h"
#include "icegrids.h"
#include "nic.h"

int oth_data(FILE **north, FILE **south, 
             const region *northern, const region *southern, 
             float lat, float longit, int conc, int area); 
int oth_header(FILE **north, FILE **south, 
               const region *northern, const region *southern);
int nic_region(float *lat, float *longit, 
               const region *northern, const region *southern);

int main(int argc, char *argv[])
{
  unsigned char  fld_n[NY_NORTH][NX_NORTH];
  unsigned char  fld_s[NY_SOUTH][NX_SOUTH];
  int i, j;
  int x;
  FILE *nin, *sin, *nout[NORTH_REGIONS], *sout[SOUTH_REGIONS];
  float lat, lon;
  int area, ret;
 
/* Open data files */
  nin   = fopen(argv[1], "r");
  sin   = fopen(argv[2], "r");
  if (nin != NULL && sin != NULL ) {
    printf("Have opened the input files\n");
  }
  else {
    printf("Error opening one or more of the input files\n");
    return -1;
  }

  for ( i = 0 ; i < NORTH_REGIONS; i++)
  {
    nout[i]  = fopen(northern[i].name, "w");
    if (nout[i] == NULL) {
      printf("Failed to open northern output file %s\n",northern[i].name);
      return -1;
    }
  }

  for ( i = 0 ; i < SOUTH_REGIONS; i++)
  {
    sout[i]  = fopen(southern[i].name, "w");
    if (sout[i] == NULL) {
      printf("Failed to open southern output file %s\n",southern[i].name);
      return -1;
    }
  }

/* Set up the OTH header information */
  oth_header(nout, sout, northern, southern);



/* Read in the northern hemisphere data */
  i = fread(fld_n, sizeof(unsigned char), (size_t) (NX_NORTH)*(NY_NORTH), nin);
  if (i != (NX_NORTH)*(NY_NORTH)) {
    printf("Error reading in data, only %d bytes read, when %d are expected\n",
      i, (NX_NORTH)*(NY_NORTH) );
  }
  else {
    for (j = 0; j < NY_NORTH; j++)
    {
      for (i = 0; i < NX_NORTH; i++)
      {
        x = (int) fld_n[j][i];
        if (x > 100 && x < LAND) x = 100;
        mapxy(&lat, &lon, i, j, xorig_NORTH, yorig_NORTH, dx, dy,
               slat_NORTH, slon_NORTH, sgn_NORTH, rearth, eccen2);
        area = nic_region(&lat, &lon, northern, southern);
        oth_data(nout, sout, northern, southern, lat, lon, x, area);
      }
    }
  } /*end of northern hemisphere*/
      

/* Read in the southern hemisphere data */
  i = fread(fld_s, sizeof(unsigned char), (NX_SOUTH)*(NY_SOUTH), sin);
  if (i != (NX_SOUTH)*(NY_SOUTH)) {
    printf("Error reading in south data, only %d bytes read, when %d are expected\n",
      i, (NX_SOUTH)*(NY_SOUTH) );
  }
  else {
    for (j = 0; j < NY_SOUTH; j++)
    {
      for (i = 0; i < NX_SOUTH; i++)
      {
        x = (int) fld_s[j][i];
        if (x > 100 && x < LAND) x = 100;
        mapxy(&lat, &lon, i, j, xorig_SOUTH, yorig_SOUTH, dx, dy,
               slat_SOUTH, slon_SOUTH, sgn_SOUTH, rearth, eccen2);
        area = nic_region(&lat, &lon, northern, southern);
        ret = oth_data(nout, sout, northern, southern, lat, lon, x, area);
/*        if (ret != 0) {
          printf("error in oth_data, ret = %d ",ret);
          printf("lat, lon, x, area = %f %f %d %d\n",lat, lon, x, area);
        }
*/
      }
    }
  } /*end of hemisphere*/
      
  return -1;

}

int oth_header(FILE **north, FILE **south, 
               const region *northern, const region *southern)
{
/* Print the oth gold header information */
  int i;
  
  for (i = 0 ; i < NORTH_REGIONS; i++)
  {
    fprintf(north[i], "UNCLAS //N03140//\n");
    fprintf(north[i], "MSGID//GOLD/001/JUN\n");
    fprintf(north[i], "CTC/T0000/////////MSG\n");
    fprintf(north[i], "OVLY/SSMI NORTH/16/JUN/1OF1/SSMI DATA NORTHERN HEMI\n");
  }

  for (i = 0 ; i < SOUTH_REGIONS; i++)
  {
    fprintf(south[i], "UNCLAS //N03140//\n");
    fprintf(south[i], "MSGID//GOLD/001/JUN\n");
    fprintf(south[i], "CTC/T0000/////////MSG\n");
    fprintf(south[i], "OVLY/SSMI SOUTH/16/JUN/1OF1/SSMI DATA SOUTHERN HEMI\n");
  }

  return 0;

}

int oth_data(FILE **north, FILE **south, 
             const region *northern, const region *southern, 
             float lat, float longit, int conc, int area)
{
  int i;
  int lat_degs, lat_mins,  lat_secs;
  int long_degs, long_mins, long_secs;
  float x, llat, llon;
  char pole, hemi;
  
  if (area < 0) return -1;

  llon = longit;
  llat = lat;
  if (llon >= 0. && llon <= 180.) {
    hemi='E';
  }
  else {
    hemi='W';
    llon = -llon;
  }

/* Convert latitude and longitude to degrees, minutes, seconds */
  x = llon;
  long_degs = (int) x;
  x = 60. * (x - long_degs);
  long_mins = (int) x;
  x = 60. * (x - long_mins);
  long_secs = (int) ( x + 0.5); 

  if (llat > 0.) {
    x = llat;
    lat_degs = (int) x;
    x = 60. * (x - lat_degs);
    lat_mins = (int) x;
    x = 60. * (x - lat_mins);
    lat_secs = (int) ( x + 0.5); 
  
    pole = 'N';
    fprintf(north[area], 
            "TEXT/10/1/D/%02d%02d%02d%c0/%03d%02d%02d%c0/%d\n",
            lat_degs, lat_mins, lat_secs, pole, 
            long_degs, long_mins, long_secs, hemi,
            conc );
  }
  else { 
    x = -llat;
    lat_degs = (int) x;
    x = 60. * (x - lat_degs);
    lat_mins = (int) x;
    x = 60. * (x - lat_mins);
    lat_secs = (int) ( x + 0.5); 
    pole = 'S';
    fprintf(south[area], 
            "TEXT/10/1/D/%02d%02d%02d%c0/%03d%02d%02d%c0/%d\n",
            lat_degs, lat_mins, lat_secs, pole, 
            long_degs, long_mins, long_secs, hemi,
            conc );
  }

  return 0;

}



int nic_region(float *lat, float *longit, 
               const region *northern, const region *southern)
{
/* Find which region, if any, the point is inside.  Use brain-dead
   algorithm to start with */
/* Bob Grumbine 11 October 1994 */

  int i, area;
  float llat, llon;
 
  area = -1;

  if (*lat > 90.) return -1;
  if (*lat < -90.) return -1;
  llat = *lat;
  llon = *longit;
  
  if (llon > 180.) {
    llon = llon - 360.;
  }
  if (llon < -180.) {
    llon = llon + 360.;
  }
  if (llon != *longit) {
/*    printf ("fixing longitude, longit = %f, llon = %f\n",*longit, llon); */
    *longit = llon;
  }

  if (llat > 0 ) {
   for (i = 0 ; i < NORTH_REGIONS; i++)
   {
     if ( llat >= northern[i].latmin && llat <= northern[i].latmax
      &&  llon >=northern[i].longmin && llon <= northern[i].longmax ) 
     { area = i ; 
     }
   }
  }

  else {
    for (i = 0 ; i < SOUTH_REGIONS; i++)
    {
      if ( llat >= southern[i].latmin && llat <= southern[i].latmax
       &&  llon >=southern[i].longmin && llon <= southern[i].longmax ) 
      { area = i ; 
      }
    }
  }

  return area;  

}
