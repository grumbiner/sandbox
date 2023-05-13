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
   char map[NSIDC_SOUTH_NY][NSIDC_SOUTH_NX];
   float conc[FORE_SOUTH_NY][FORE_SOUTH_NX];
   int count[FORE_SOUTH_NY][FORE_SOUTH_NX];

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

   for (j = 0; j <= FORE_SOUTH_NY; j++)
   {  for (i = 0; i <= FORE_SOUTH_NX; i++)
	  {  count[j][i] = 0;
             conc [j][i] = 0.0;
	  }
   }


   n = fread(map, sizeof(char), NSIDC_SOUTH_NX*NSIDC_SOUTH_NY, input);
   if (n != NSIDC_SOUTH_NX*NSIDC_SOUTH_NY) {
	 printf("Failure to read in the data!!\n");
	 return -1;
   }

   for (jloop = 0; jloop < NSIDC_SOUTH_NY; jloop++)
   {  for ( iloop = 0; iloop < NSIDC_SOUTH_NX; iloop++)
        {

            jnes = NSIDC_SOUTH_NY - 1 - jloop;

	    mapxy(&lat, &lon, iloop, jnes,
              nsidc_SOUTH_xorig,  nsidc_SOUTH_yorig,
              nsidc_SOUTH_dx,     nsidc_SOUTH_dy,
              nsidc_SOUTH_slat,   nsidc_SOUTH_slon, nsidc_SOUTH_sgn,
              nsidc_SOUTH_rearth, nsidc_SOUTH_eccen2);
/* Must have absolute value of lat */
	    mapll(-lat, lon, &i, &j, FORE_SOUTH_xorig, FORE_SOUTH_yorig, 
              FORE_SOUTH_eccen2, FORE_SOUTH_slat, FORE_SOUTH_slon, 
              FORE_SOUTH_rearth, FORE_SOUTH_dx, FORE_SOUTH_dy, FORE_SOUTH_sgn);

/*            if (jloop >= 463) {
            printf("lat, lon, iloop, jloop, i, j %f %f %4d %4d %3d %3d\n",
              lat, lon, iloop, jloop, i, j);
            }
*/

	    if (i >= 0 && i < FORE_SOUTH_NX && j >= 0 && j < FORE_SOUTH_NY) {
	     con = (float) map[jloop][iloop] ;
             if ( (int) con != LAND && (int) con != BAD_DATA && 
                  (int) con != NO_DATA) {
               conc[j][i] += con;
               count[j][i] += 1;
             }
	    } /* End of filling in data where appropriate */
        } /* End of iloop */
   } /* End of jloop */

   for (j = 0; j < FORE_SOUTH_NY; j++)
   {  for (i = 0; i < FORE_SOUTH_NX; i++)
	  {  if (count[j][i] != 0)  conc[j][i] = conc[j][i]/count[j][i]/100.;
	  }
   }

   n = fwrite(conc, sizeof(float), (FORE_SOUTH_NX)*(FORE_SOUTH_NY), output);
   if (n != (FORE_SOUTH_NX)*(FORE_SOUTH_NY) ) {
	 printf("Failed to write to output file!!\n");
	 return -1;
   }

   return 0;

}
