#include <stdio.h>
#include <string.h>
#include "nesdisnh.h"

/* Main program to extract either the NASA- A, or NESDIS- E, ice concentrations
     from the processed map file */
/* Input is name of the ssmi data file, then name of output file */
/* Bob Grumbine 6 December 1994 */

int main(int argc, char *argv[])
{
   FILE *input, *output;
   float lat, lon, con;
   int i, j, n, iloop, jloop;
   ssmi map[NES_NY][NES_NX];
   unsigned char conc[NES_NY][NES_NX];

   if (argc < 4) { printf("Need names of the arctic data file and the bg \
output file, followed by A or E (nasa or nesdis) \n");
		return -1;
   }
   input = fopen(argv[1], "r");
   output = fopen(argv[2], "w");
   if (input == NULL || output == NULL) {
	 printf("Failed to open one of the files!!\n");
	 return -1;
   }

   n = fread(map, sizeof(ssmi), NES_NX*NES_NY, input);
   if (n != NES_NX*NES_NY) {
	 printf("Failure to read in the data!!\n");
	 return -1;
   }

   if (strncmp(argv[3], "A", 1) == 0) {
     for (j = 0; j <= NES_NY; j++)
     {  for (i = 0; i <= NES_NX; i++)
  	  {  
               conc [j][i] = map[j][i].nasa;
  	  }
     }
    }
     else if (strncmp(argv[3], "E", 1) == 0 )  {
       for (j = 0; j <= NES_NY; j++)
       {  for (i = 0; i <= NES_NX; i++)
    	  {  
                 conc [j][i] = map[j][i].nesdis;
    	  }
       }
    }
     else {
       printf("Last argument _must_ be A or E, case matters!\n");
       printf("string = %s \n %d chars", argv[3], strlen(argv[3]) );
       return -1;
    }

   n = fwrite(conc, sizeof(unsigned char), (NES_NX)*(NES_NY), output);
   if (n != NES_NX*NES_NY ) {
	 printf("Failed to write to output file!!\n");
	 return -1;
   }

   return 0;

}
