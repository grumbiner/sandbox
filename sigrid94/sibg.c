#include <stdio.h>
#include <math.h>
#include "bgnh.h"
#include "sigrid.h"

void mapll(const float lat,  const float lon, int *i, int *j,
           const float xnot, const float ynot);

int sibg4main(int argc, char *argv[])
{
/* BG program to remap the sigrid information onto the bg grid */
/* Northern Hemisphere version */
   FILE *east, *west, *output;
   float lat, lon, con;
   int i, j, n, jloop;
   sigrid_point point ;
/*   sigrid_point point[18] = { {90,  90, 100}, 
                              {60,   0, 100},
                              {60,  90, 100},
                              {60, 180, 100},
                              {60, 270, 100},
                              {80,   0, 100},
                              {80,  90, 100},
                              {80, 180, 100},
                              {80, 270, 100},
                              {45, 280, 100},
                              {50, 280, 100},
                              {55, 280, 100},
                              {60, 280, 100},
                              {65, 280, 100},
                              {70, 280, 100},
                              {75, 280, 100},
                              {80, 280, 100},
                              {85, 280, 100} }; */

/*   const int npts = 18; */
   float conc[NY+1][NX+1];
   int count[NY+1][NX+1];

/* Input is name of the east then west arctic data files, then name of bg
	 output file */

   east   = fopen(argv[1], "r");
   if (east == NULL) {
	 printf("Failed to open the east file !!\n");
	 return -1;
   }
   west   = fopen(argv[2], "r");
   if (west == NULL) {
	 printf("Failed to open the west file !!\n");
	 return -1;
   }
   output = fopen(argv[3], "w");
   if (output == NULL) {
	 printf("Failed to open the output file !!\n");
	 return -1;
   }

   for (j = 0; j <= NY; j++)
   {  for (i = 0; i <= NX; i++)
	  {  count[j][i] = 0;
	     conc [j][i] = 0.0;
	  }
   }

/* Read in, point by point, from th east arctic file */
   fread(&point, sizeof(sigrid_point), 1, east);
   while ( ! feof(east) ) {
	 lat = (float) point.lat;
	 lon = (float) point.lon;
	 con = (float) point.con;
         lat = (lat + (float) lat_min)*0.25;
         lon = (lon)*0.25;
	 mapll(lat, lon, &i, &j, xorig, yorig);
/*         printf("lat, long, concentration %5.2f %6.2f %4.0f %3d %3d\n",
                 lat, lon, con, i, j); */
	 if (i >= 0 && i <= NX && j >= 0 && j <= NY) {
	   conc[j][i] += con;
	   count[j][i] += 1;
	 }
     fread(&point, sizeof(sigrid_point), 1, east);
   }

/* Read in from the west arctic file */
   fread(&point, sizeof(sigrid_point), 1, west);
   while ( ! feof(west) ) {
	 lat = (float) point.lat;
	 lon = (float) point.lon;
	 con = (float) point.con;
         lat = (lat + (float) lat_min)*0.25;
         lon = (lon)*0.25;
	 mapll(lat, lon, &i, &j, xorig, yorig);
         printf("lat, long, concentration %5.2f %6.2f %4.0f %3d %3d\n",
                 lat, lon, con, i, j);
	 if (i >= 0 && i <= NX && j >= 0 && j <= NY) {
	   conc[j][i] += con;
	   count[j][i] += 1;
	 }
     fread(&point, sizeof(sigrid_point), 1, west);
   }

   for (j = 0; j <= NY; j++)
   {  for (i = 0; i <= NX; i++)
	  {  if (count[j][i] != 0)  conc[j][i] = conc[j][i]/count[j][i];
	  }
   }

   conc[1][1] = 100;
   conc[NY][NX] = 100;
   n = fwrite(conc, sizeof(float), (NX+1)*(NY+1), output);
   if (n != (NX+1)*(NY+1) ) {
	 printf("Failed to write to output file!!\n");
	 return -1;
   }

   return 0;

}
