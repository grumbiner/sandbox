#include <stdio.h>
#include "icessmi.h"

/* Construct a filled lat-long grid of sea ice data from yesterday's
     filled map and today's observations.  If today has a valid 
     observation, use it.  If there is no valid observation today,
     use the value from yesterday's map and increment the age.
     If the age reaches 30 (i.e., 30 days with no valid observations),
     set the ice concentration to zero and restart the age counter.
  Robert Grumbine 4 June 1997 */ 
/* Return codes: */
/*    0  - Successful */
/*    1  - Failed to open input */
/*    2  - Failed to open output */
/* Maxage, Maxice added as defined parameters  18 Nov 1998 */
/* Return codes revised and defined            18 Nov 1998 */
/* Commentary on points which are reset due to being overage added 
                                               18 Nov 1998 */
/* icessmi.h is needed for MIN_CONC */

#define NX 720
#define NY 360
#define MAXAGE 30
#define MAXICE 126

/* Argument for size, with malloc of the arrays? */
int main(int argc, char *argv[])
{
  FILE *fiage, *foage, *fice1, *fice2, *foice;
  unsigned char iage[NY][NX], oage[NY][NX], ice1[NY][NX], ice2[NY][NX];
  unsigned char oice[NY][NX];
  int i, j, oldest=0;

  fice1 = fopen(argv[1], "r");
  fice2 = fopen(argv[2], "r");
  fiage = fopen(argv[3], "r");
  foage = fopen(argv[4], "w");
  foice = fopen(argv[5], "w");

  if ( fice1 == NULL || fice2 == NULL || fiage == NULL) {
    printf("Failed to open a required input file!\n");
    if (fice1 == NULL ) { printf("Failed to open old filled ice file\n"); }
    if (fice2 == NULL ) { printf("Failed to open new latlon ice file\n"); }
    if (fiage == NULL ) { printf("Failed to open old ice age file\n"); }
    return 1;
  }
  if (foage == NULL || foice == NULL ) {
    printf("Failed to open a required output file!\n");
    return 2;
  }

  fread(ice1, sizeof(unsigned char), NX*NY, fice1);
  fread(ice2, sizeof(unsigned char), NX*NY, fice2);
  fread(iage, sizeof(unsigned char), NX*NY, fiage);

  oldest = 0;
  for (j = 0; j < NY; j++) {
    for (i = 0; i < NX; i++) {
      if (ice2[j][i] >= ( (unsigned char) MAXICE ) ) { 
        oice[j][i] = ice1[j][i];
        oage[j][i] = iage[j][i] + 1;
      }
      else {
        oice[j][i] = ice2[j][i];
        oage[j][i] = 0;
      }
      if (oice[j][i] < MIN_CONC) oice[j][i] = 0;
      if (iage[j][i] > MAXAGE) { 
        printf("Overage point at %d %d, %d limit, old cover %3d\n",
                   i, j, MAXAGE, (int) oice[j][i]);
        oice[j][i] = 0; 
        oage[j][i] = 0;
      }
      oldest = max(oldest, oage[j][i]);

    }
  }

  printf("Oldest point at end of processing is %d days\n",oldest);
  fwrite(oice, sizeof(unsigned char), NX*NY, foice);
  fwrite(oage, sizeof(unsigned char), NX*NY, foage);

  return 0;

}
