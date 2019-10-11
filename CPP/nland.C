#include <stdio.h>
#include "icessmi.h"
#include "icegrids.h"

int main(int argc, char *argv[])
{
  FILE *in1, *in2, *out1;
  char fname[60];
  int i, j, k;
  int nday = 28;
  float x;

  char map[nday][NY_NORTH][NX_NORTH];
  char count[256];
  char obsd[NY_NORTH][NX_NORTH], land[NY_NORTH][NX_NORTH];

  for (i = 0; i < nday; i++)
  {
    j = i + 1;
    sprintf(fname, "a2north.9502%02d", j);
    in1 = fopen(fname, "r");
    fread(&map[i][0][0], sizeof(char), NY_NORTH*NX_NORTH, in1);
  }
  
  for (k = 0; k < NX_NORTH; k++)
  {  for (j = 0 ; j < NY_NORTH; j++)
     {
       for (i = 0; i < 256; i++) { count[i] = 0 ; }
       for (i = 0; i < nday; i++)
       {
         count[ (int) map[i][j][k] ] += 1;  
       }
       land[j][k] = count[ LAND ] ;
       obsd[j][k] = nday - count[ NO_DATA] ;
     }
  }

/* Now rescale land to a fraction of the possible observations */
/* And rescale obsd for pretty output (mult by 7) */
  for (i = 0 ; i < NY_NORTH; i++)
  { for (j = 0 ; j < NX_NORTH; j++)
    {
      if ( (land[i][j] == obsd[i][j]) && obsd[i][j] > 0 ) {
        land[i][j] = LAND;
      }
      else { 
        if ( (land[i][j] == 0 ) && obsd[i][j] > 0 ) {
          land[i][j] = 0;
        }
        else {
          land[i][j] = 0;
        }
      }
 
      obsd[i][j] *= 7 ;
    }
  }

  for (i = 1 ; i < NY_NORTH-1; i++)
  { for (j = 1 ; j < NX_NORTH-1; j++)
    {
       if ( ( land[i+1][  j] == LAND ||
              land[i+1][j+1] == LAND ||
              land[i+1][j-1] == LAND ||
              land[i  ][j+1] == LAND ||
              land[i  ][j-1] == LAND ||
              land[i-1][j-1] == LAND ||
              land[i-1][j  ] == LAND ||
              land[i-1][j+1] == LAND    )  &&
              land[i][j] != LAND ) {
           land[i][j] = COAST;
           }
          else {
           if (land[i][j] == 0) printf("%3d %3d %2d\n",i, j, obsd[i][j]/7);
          }
    }
  }

  out1 = fopen("land.map","w");
  fwrite (land, sizeof(char), NY_NORTH*NX_NORTH, out1);
  out1 = fopen("obsd.map","w");
  fwrite (obsd, sizeof(char), NY_NORTH*NX_NORTH, out1);
  

  return 0;

}
