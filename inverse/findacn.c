#include <stdio.h>
#include <stdlib.h>

#define NX 385
#define NY 465
#define NLAG 8

int main(int argc, char *argv[])
{
  unsigned char acs[NLAG][NY][NX];
  float acout[NY][NX];
  float eps;
  unsigned char land[NY][NX], atmp[NY][NX];
  int i, j, k;

  int base;
  float critical;
  FILE *fin;
  char fname[80];

  base = atoi(argv[1]);

  fin = fopen("nland.map", "r");
  fread(land, sizeof(unsigned char), NX*NY, fin);
  for (k = 0; k < NLAG; k++) {
     sprintf(fname, "auto.%4d.n.%1d",base, k); 
     fin = fopen(fname, "r");
     if (fin == NULL ) {
       printf("Failed to open file %s\n",fname);
       return -1;
     }
     else {
       fread(atmp, sizeof(unsigned char), NX*NY, fin);
       for (j = 0; j < NY; j++) {
         for (i = 0; i < NX; i++) {
            acs[k][j][i] = atmp[j][i];
         }
       }
     }
   }

/* Done reading in the data */
       
/* Now find the time at which the acor drops below argv2 */
   critical = atof(argv[2]);
   printf("%f\n", critical);
   for (j = 0; j < NY; j++) {
      for (i = 0; i < NX; i++) {
         k = 0;
         eps = -1.;
         while ( k < NLAG - 1 && eps == -1.) {
           if ( acs[k][j][i] > critical && acs[k+1][j][i] < critical ) {
             /* Have bracketed the critical point */ 
             eps = (acs[k][j][i] - critical) / (acs[k][j][i] - acs[k+1][j][i] );
             acout[j][i] = k + eps;
           }
           else {
            k += 1;
           }
         }
       /* Exception handling here */
       if ( acs[0][j][i] > 201 ) {
           eps = 0.0;
           acout[j][i] = 0.;
       }
       if (eps == -1.) acout[j][i] = 8.;
       if (land[j][i] > (unsigned char) 100) acout[j][i] = 12.;
/*       printf("i, j, time %3d %3d %5.2f\n", i, j, acout[j][i]); */
      }
   }

/* Write out the acor times */

   sprintf(fname, "acortime.%4d", base);
   fin = fopen(fname, "w");
   if (fin == NULL ) {
     printf("Failed to open the output file!\n");
     return -1;
   }
   else {
     fwrite(acout, sizeof(float), NX*NY, fin);
   }

   return 0;

}
