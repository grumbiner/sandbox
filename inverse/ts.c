#include <stdio.h>
#include "icegrids.h"

/* Program to handle the revision of data for input to the icevary
   routine.  This is probably egregiously inefficient. */

int main(int argc, char *argv[])
{
  FILE *afn[30], *rfn[30], *in, *fout;
  char aline[30][NX_NORTH];
  float rline[30][NX_NORTH];
  char fname[80];
  int b, c, i, j, k, ndays;


  in = fopen(argv[1], "r");
  fout = fopen(argv[2], "w");

  fscanf(in, "%d\n", &ndays);
  printf("%d\n",ndays);

  fname[79]='\0';
  for (i = 0; i < ndays; i++)
  {
    fscanf(in, "%s\n",fname);
    printf("fname = %s\n",fname);
    afn[i] = fopen(fname, "r");
    if (afn[i] == NULL) { printf("failed to open file %s \n",fname);
                          return -1; }
    fscanf(in, "%s\n",fname);
    printf("fname = %s\n",fname);
    rfn[i] = fopen(fname, "r");
    if (rfn[i] == NULL) { printf("failed to open file %s \n",fname);
                          return -1; }
  }

/* Now start to read in data, and write out in sequence */
  for (j = 0; j < NY_NORTH; j++)
  {
    printf("running line j = %d \n",j);
    for (k = 0; k < ndays; k++)
    {
      b = fread(&aline[k][0], sizeof(char), NX_NORTH, afn[k]);
      c = fread(&rline[k][0], sizeof(float), NX_NORTH, rfn[k]);
      if (b != NX_NORTH || c != NX_NORTH) { 
    printf("failed to read in %d data points, found %d %d\n",NX_NORTH, b, c);
    return -1;
      }
    }

    for (i = 0; i < NX_NORTH; i++)
    {
      for (k = 0; k < ndays; k++)
      {
        fprintf(fout, "%6.4f  %9.6f\n", (float)aline[k][i]/100., rline[k][i]);
      }
    }

  }

  return 0;

}
