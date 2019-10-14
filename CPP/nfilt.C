#include <stdio.h>
#include <math.h>
#include "foregrids.h"
#include "icessmi.h"
#include "icegrids.h"

extern int anyice(const char *map, const int nx, const int ny, 
                  const int i, const int j, const int range);

int main(int argc, char *argv[])
{
/* Compare two ssmi maps */
/* Northern Hemisphere version */
   FILE *today, *yesterday, *freeze, *output, *outchar, *land;
   float lat, lon, con;
   int i, j, n, iloop, jloop, jnes;
   char map_today[NY_NORTH][NX_NORTH];
   char map_yester[NY_NORTH][NX_NORTH];
   char landm[NY_NORTH][NX_NORTH];
   char flags[NY_NORTH][NX_NORTH];
   float f0[FORE_NORTH_NY][FORE_NORTH_NX];
   float f02[FORE_NORTH_NY][FORE_NORTH_NX];

/* Input is name of the ssmi data file, then name of bg
	 output file */

   if (argc < 7) { printf("Need yesterday's map, today's map estimate,\
land mask, freezing rate, then the real and char output files.\n");
		return -1;
   }
   yesterday = fopen(argv[1], "r");
   today     = fopen(argv[2], "r");
   land      = fopen(argv[3], "r");
   freeze    = fopen(argv[4], "r");
   output    = fopen(argv[5], "w");
   outchar   = fopen(argv[6], "w");
   if (yesterday == NULL || today == NULL || land == NULL || freeze == NULL) {
	 printf("Failed to open one of the input files!!\n");
         printf("%s \n %s \n %s \n %s \n",argv[1], argv[2], argv[3], argv[4]);
	 return -1;
   }

   n = fread(map_yester, sizeof(char), NX_NORTH*NY_NORTH, yesterday);
   if (n != NX_NORTH*NY_NORTH) {
	 printf("Failure to read in yesterday's concentration data!\n");
	 return -1;
   }
   n = fread(map_today, sizeof(char), NX_NORTH*NY_NORTH, today);
   if (n != NX_NORTH*NY_NORTH) {
	 printf("Failure to read in today's concentration data!\n");
	 return -1;
   }
   n = fread(landm, sizeof(char), NX_NORTH*NY_NORTH, land);
   if (n != NX_NORTH*NY_NORTH) {
	 printf("Failure to read in the land mask data!!\n");
	 return -1;
   }

   n = fread(f0, sizeof(float), NX_NORTH*NY_NORTH, freeze);
   if (n != NX_NORTH*NY_NORTH) {
	 printf("Failure to read in the freezing data!! %d\n", n);
	 return -1;
   }
   n = fread(f02, sizeof(float), NX_NORTH*NY_NORTH, freeze);
   if (n != NX_NORTH*NY_NORTH) {
	 printf("Failure to read in the second freezing data!! %d\n", n);
	 return -1;
   }


/* Begin working with the present day file */
   printf("calling pole fill\n");
/* last arg = 1 for north, -1 for south */
   pole_fill(&map_today[0][0], 1);

/* Now loop through and compare the two files */
   for (j = 0; j < NY_NORTH; j++)
   {  for ( i = 0; i < NX_NORTH; i++)
        {
        flags[j][i] = 0;
        if (landm[j][i] == LAND || landm[j][i] == COAST) {
          map_today[j][i] = landm[j][i];
        }
        if (map_today[j][i] == BAD_DATA) map_today[j][i] = 0;
        if (map_today[j][i] == NO_DATA ) map_today[j][i] = map_yester[j][i];

/* Freezing rate cut off - present day */
        if ( (landm[j][i] != LAND && landm[j][i] != COAST) 
                      && map_today[j][i] != 0 &&
             (f0[j][i] < MAX_FREEZE) ) {
              map_today[j][i] = 0;
              flags[j][i] = 32;
        }

/* Concentrations test -- no growth when high melting rates */
        if ((map_today[j][i]-map_yester[j][i]) > 0. 
               && (landm[j][i] != LAND) && (landm[j][i] != COAST)
                     && f0[j][i] < MAX_GROW_FREEZE) {
          map_today[j][i] = map_yester[j][i];
          flags[j][i] = 64;
        }

/* Concentration - Advection test: no ice if melting even if you assume ice 
     present, and there was no ice in the area yesterday */
        if ( map_today[j][i] > 0  && f0[j][i] < 0.0 &&
             landm[j][i] != LAND && landm[j][i] != COAST) {
                                
          if ( anyice(&map_yester[0][0], NX_NORTH, NY_NORTH, i, j, 1) > 0 ) {
          }
          else {
            map_today[j][i] = 0;
            flags[j][i] = 96;
          } /* end of fixing concentration with advection consideration */

        }

/* Enforce minimum concentrations */
        if (map_today[j][i] < MIN_CONC ) map_today[j][i] = 0;

/* Test with freezing rate assuming that no ice was already present */
        if ( anyice(&map_yester[0][0], NX_NORTH, NY_NORTH, i, j, 1) == 0 
             && (landm[j][i] != LAND && landm[j][i] != COAST) 
             && f02[j][i] <= 0.0 ) {
           map_today[j][i] = 0;
           flags[j][i]     = 160;
        }




        } /* End of i */
   } /* End of j */

/* Write out the revised map */
   n = fwrite(map_today, sizeof(char), (FORE_NORTH_NX)*(FORE_NORTH_NY), output);
   if (n != (FORE_NORTH_NX)*(FORE_NORTH_NY) ) {
	 printf("Failed to write to output file!!\n");
	 return -1;
   }
   
   n = fwrite(flags, sizeof(char), (FORE_NORTH_NX)*(FORE_NORTH_NY),
               outchar);
   if (n != (FORE_NORTH_NX)*(FORE_NORTH_NY) ) {
	 printf("Failed to write to output file!!\n");
	 return -1;
   }

   return 0;

}
