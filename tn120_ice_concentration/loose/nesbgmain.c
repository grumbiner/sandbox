#include <stdio.h>
#include <math.h>
#include "bgnh.h"
#include "nesdisnh.h"

void mapll(float lat, float lon, int *i, int *j);

void mapxy(float *lat, float *lon, int i, int j);

/* Program to remap NESDIS ssmi information onto the sea ice grid */
/* Northern Hemisphere version */
/* Bob Grumbine 13 April 1994 */

int main(int argc, char *argv[])
{
   FILE *input, *output;
   float lat, lon, con;
   int i, j, n, iloop, jloop;
   ssmi map[NES_NY][NES_NX];
   float conc[NY+1][NX+1];
   int count[NY+1][NX+1];

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


   n = fread(map, sizeof(ssmi), NES_NX*NES_NY, input);
   if (n != NES_NX*NES_NY) {
	 printf("Failure to read in the data!!\n");
	 return -1;
   }

   for (jloop = 0; jloop < NES_NY; jloop++)
   {  for ( iloop = 0; iloop < NES_NX; iloop++)
        {
	    mapxy(&lat, &lon, iloop, jloop);
	    mapll(lat, lon, &i, &j);
/*          printf("%d %d lat, lon, i, j %f %f %d %d\n",
              iloop, jloop, lat, lon, i, j); */
	    if (i >= 0 && i <= NX && j >= 0 && j <= NY) {
	     con = (float) map[jloop][iloop].nesdis;
	     conc[j][i] += con;
	     count[j][i] += 1;
	    }
        }
   }

   for (j = 0; j <= NY; j++)
   {  for (i = 0; i <= NX; i++)
	  {  if (count[j][i] != 0)  conc[j][i] = conc[j][i]/count[j][i];
	  }
   }

   n = fwrite(conc, sizeof(float), (NX+1)*(NY+1), output);
   if (n != (NX+1)*(NY+1) ) {
	 printf("Failed to write to output file!!\n");
	 return -1;
   }

   return 0;

}
