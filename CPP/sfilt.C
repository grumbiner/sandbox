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
/* Southern Hemisphere version */
   FILE *today, *yesterday, *freeze, *output, *outchar, *land;
   float lat, lon, con;
   int i, j, n, iloop, jloop, jnes;
   char map_today[NY_SOUTH][NX_SOUTH];
   char map_yester[NY_SOUTH][NX_SOUTH];
   char landm[NY_SOUTH][NX_SOUTH];
   char flags[NY_SOUTH][NX_SOUTH];
   float f0[FORE_SOUTH_NY][FORE_SOUTH_NX];

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

   n = fread(map_yester, sizeof(char), NX_SOUTH*NY_SOUTH, yesterday);
   if (n != NX_SOUTH*NY_SOUTH) {
	 printf("Failure to read in yesterday's concentration data!\n");
	 return -1;
   }
   n = fread(map_today, sizeof(char), NX_SOUTH*NY_SOUTH, today);
   if (n != NX_SOUTH*NY_SOUTH) {
	 printf("Failure to read in today's concentration data!\n");
	 return -1;
   }
   n = fread(landm, sizeof(char), NX_SOUTH*NY_SOUTH, land);
   if (n != NX_SOUTH*NY_SOUTH) {
	 printf("Failure to read in the land mask data!!\n");
	 return -1;
   }

   n = fread(f0, sizeof(float), NX_SOUTH*NY_SOUTH, freeze);
   if (n != NX_SOUTH*NY_SOUTH) {
	 printf("Failure to read in the freezing data!! %d\n", n);
	 return -1;
   }


/* Begin working with the present day file */
   printf("calling pole fill\n");
/* last arg = 1 for north, -1 for south */
   pole_fill(&map_today[0][0], 1);

/* Now loop through and compare the two files */
   for (j = 0; j < NY_SOUTH; j++)
   {  for ( i = 0; i < NX_SOUTH; i++)
        {
        flags[j][i] = 0;
        if (landm[j][i] == LAND || landm[j][i] == COAST) {
          map_today[j][i] = landm[j][i];
        }
        if (map_today[j][i] == BAD_DATA) map_today[j][i] = 0;
        if (map_today[j][i] == NO_DATA) map_today[j][i] = map_yester[j][i];

/* Freezing rate cut off - present day */
        if ( (landm[j][i] != LAND && landm[j][i] != COAST) 
                      && map_today[j][i] != 0 &&
             (f0[j][i] < MAX_FREEZE) ) {
/*              printf("Melting, reset %3d at %3d %3d %6.2f\n", 
                map_today[j][i], i, j, f0[j][i] );
*/
              map_today[j][i] = 0;
              flags[j][i] = 32;
        }

/* Concentrations test -- no growth when high melting rates */
        if ((map_today[j][i]-map_yester[j][i]) > 0. 
               && (landm[j][i] != LAND) && (landm[j][i] != COAST)
                     && f0[j][i] < MAX_GROW_FREEZE) {
/*          printf("false increase, %3d %3d %3d %3d %6.2f\n", 
                  i, j, map_yester[j][i], map_today[j][i], f0[j][i]);
*/
          map_today[j][i] = map_yester[j][i];
          flags[j][i] = 128;
        }

/* Concentration - Advection test: no ice if melting > 175 w/m2 and no
   ice yesterday: */
        if ( map_today[j][i] > 0  && f0[j][i] < -5.0 &&
             landm[j][i] != LAND && landm[j][i] != COAST) {
                                
          if ( anyice(&map_yester[0][0], NX_SOUTH, NY_SOUTH, i, j, 1) > 0 ) {
/*            printf("possible advection %3d %3d\n",i,j); */
          }
          else {
/*            printf("impossible advection %3d %3d\n",i,j); */
            map_today[j][i] = 0;
            flags[j][i] = 160;
          } /* end of fixing concentration with advection consideration */

        }

/* Enforce minimum concentrations */
        if (map_today[j][i] < MIN_CONC ) map_today[j][i] = 0;


        } /* End of i */
   } /* End of j */

/* Write out the revised map */
   n = fwrite(map_today, sizeof(char), (FORE_SOUTH_NX)*(FORE_SOUTH_NY), output);
   if (n != (FORE_SOUTH_NX)*(FORE_SOUTH_NY) ) {
	 printf("Failed to write to output file!!\n");
	 return -1;
   }
   
   n = fwrite(flags, sizeof(char), (FORE_SOUTH_NX)*(FORE_SOUTH_NY),
               outchar);
   if (n != (FORE_SOUTH_NX)*(FORE_SOUTH_NY) ) {
	 printf("Failed to write to output file!!\n");
	 return -1;
   }

   return 0;

}
