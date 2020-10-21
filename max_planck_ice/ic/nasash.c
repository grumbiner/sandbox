#include <stdio.h>
#include <math.h>
#include "nassh.h"
#include "bgsh.h"

void mapll(float lat, float lon, int *i, int *j, const float xnot, 
           const float ynot);

void mapxy(float *lat, float *lon, int i, int j, const float xnot, 
           const float ynot);

int main(int argc, char *argv[])
{
/* BG program to remap NASA smmr/ssmi information onto the bg grid */
/* Southern Hemisphere version - only change is the include */
   FILE *input, *output;
   float lat, lon, con;
   int i, j, n, iloop, jloop, jnes;
   char map[NES_NY][NES_NX];
   float conc[NY+1][NX+1];
   int count[NY+1][NX+1];
   float xnot, ynot;

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

   for (j = 0; j <= NY; j++)
   {  for (i = 0; i <= NX; i++)
	  {  count[j][i] = 0;
             conc [j][i] = 0.0;
	  }
   }


   n = fread(map, sizeof(char), NES_NX*NES_NY, input);
   if (n != NES_NX*NES_NY) {
	 printf("Failure to read in the data!!\n");
	 return -1;
   }

   for (jloop = 0; jloop < NES_NY; jloop++)
   {  for ( iloop = 0; iloop < NES_NX; iloop++)
        {
/*          Must flip the J index */
            jnes = NES_NY-jloop-1; 

            xnot = nes_xorig;
            ynot = nes_yorig;
/*          printf("%d %d lat, lon, i, j %f %f %d %d\n",
              iloop, jloop, lat, lon, i, j); */ 
	    mapxy(&lat, &lon, iloop, jnes, xnot, ynot);

            xnot = xorig;
            ynot = yorig;
/*          printf("%d %d lat, lon, i, j %f %f %d %d\n",
              iloop, jloop, lat, lon, i, j); */ 
	    mapll(lat, lon, &i, &j, xnot, ynot);

	    if (i >= 0 && i <= NX && j >= 0 && j <= NY) {
	     con = (float) map[jloop][iloop] ;
	     conc[j][i] += con;
	     count[j][i] += 1;
	    }
        }
   }

   for (j = 0; j <= NY; j++)
   {  for (i = 0; i <= NX; i++)
	  {  if (count[j][i] != 0)  conc[j][i] = conc[j][i]/count[j][i]/100.;
	  }
   }

   n = fwrite(conc, sizeof(float), (NX+1)*(NY+1), output);
   if (n != (NX+1)*(NY+1) ) {
	 printf("Failed to write to output file!!\n");
	 return -1;
   }

   return 0;

}
