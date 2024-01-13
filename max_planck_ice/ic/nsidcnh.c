#include <stdio.h>
#include <math.h>
#include "nsidc.h"
#include "foregrids.h"
#include "icegrids.h"
#include "icessmi.h"


int main(int argc, char *argv[])
{
/* BG program to remap NASA smmr/ssmi information onto the bg grid */
/* Northern Hemisphere version */
   FILE *input, *output;
   float lat, lon, con;
   int i, j, n, iloop, jloop, jnes;
   char map[NSIDC_NORTH_NY][NSIDC_NORTH_NX];
   float conc[FORE_NORTH_NY][FORE_NORTH_NX];
   int count[FORE_NORTH_NY][FORE_NORTH_NX];

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

   for (j = 0; j <= FORE_NORTH_NY; j++)
   {  for (i = 0; i <= FORE_NORTH_NX; i++)
	  {  count[j][i] = 0;
             conc [j][i] = 0.0;
	  }
   }


   n = fread(map, sizeof(char), NSIDC_NORTH_NX*NSIDC_NORTH_NY, input);
   if (n != NSIDC_NORTH_NX*NSIDC_NORTH_NY) {
	 printf("Failure to read in the data!!\n");
	 return -1;
   }

   for (jloop = 0; jloop < NSIDC_NORTH_NY; jloop++)
   {  for ( iloop = 0; iloop < NSIDC_NORTH_NX; iloop++)
        {

            jnes = NSIDC_NORTH_NY - 1 - jloop;

	    mapxy(&lat, &lon, iloop, jnes,
              nsidc_NORTH_xorig,  nsidc_NORTH_yorig,
              nsidc_NORTH_dx,     nsidc_NORTH_dy,
              nsidc_NORTH_slat,   nsidc_NORTH_slon, nsidc_NORTH_sgn,
              nsidc_NORTH_rearth, nsidc_NORTH_eccen2);

	    mapll(lat, lon, &i, &j, FORE_NORTH_xorig, FORE_NORTH_yorig, 
              FORE_NORTH_eccen2, FORE_NORTH_slat, FORE_NORTH_slon, 
              FORE_NORTH_rearth, FORE_NORTH_dx, FORE_NORTH_dy, FORE_NORTH_sgn);

/*            if (jloop >= 463) {
            printf("lat, lon, iloop, jloop, i, j %f %f %4d %4d %3d %3d\n",
              lat, lon, iloop, jloop, i, j);
            }
*/

	    if (i >= 0 && i < FORE_NORTH_NX && j >= 0 && j < FORE_NORTH_NY) {
	     con = (float) map[jloop][iloop] ;
             if ( (int) con != LAND && (int) con != BAD_DATA && 
                  (int) con != NO_DATA) {
               conc[j][i] += con;
               count[j][i] += 1;
             }
	    } /* End of filling in data where appropriate */
        } /* End of iloop */
   } /* End of jloop */

   for (j = 0; j < FORE_NORTH_NY; j++)
   {  for (i = 0; i < FORE_NORTH_NX; i++)
	  {  if (count[j][i] != 0)  conc[j][i] = conc[j][i]/count[j][i]/100.;
	  }
   }

   n = fwrite(conc, sizeof(float), (FORE_NORTH_NX)*(FORE_NORTH_NY), output);
   if (n != (FORE_NORTH_NX)*(FORE_NORTH_NY) ) {
	 printf("Failed to write to output file!!\n");
	 return -1;
   }

   return 0;

}
