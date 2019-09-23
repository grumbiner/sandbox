#include <stdio.h>
#include "icessmi.h"
#include "icegrids.h"

/* Estimate a growth rate term for use by other elements of the system */
/* Robert Grumbine */
/* Last Modified (file) 28 Mar 1996 */ 

int main(int argc, char *argv[])
{
  unsigned char revi[NY_NORTH][NX_NORTH];
  float grow[NY_NORTH][NX_NORTH];
  float r0[NY_NORTH][NX_NORTH];
  FILE *frev, *fgrow, *rout;
  int i, j;
  float dh = 10.;

  frev = fopen(argv[1], "r");
  fgrow = fopen(argv[2], "r");
  rout  = fopen(argv[3], "w");
  
  fread(revi, sizeof(char), NX_NORTH*NY_NORTH, frev);
  fread(grow, sizeof(float), NX_NORTH*NY_NORTH, fgrow);

  for (j = 0; j < NY_NORTH; j++)
  { for(i = 0; i < NX_NORTH; i++)
    {
      if ((int)revi[j][i] == (int)LAND || (int)revi[j][i] == (int)BAD_DATA || 
          (int)revi[j][i] == (int)NO_DATA || (int)revi[j][i] == (int)COAST ) {
        r0[j][i] = -grow[j][i] * 0.5 / dh;
      }
      else if (grow[j][i] > 0.) {
        r0[j][i] = -grow[j][i] * (1. - min(1., revi[j][i]/100.) ) / dh;
      }
      else {
        r0[j][i] = -grow[j][i] * min(1., revi[j][i]/100.) / dh;
      }
    }
  }

  fwrite(r0, sizeof(float), NX_NORTH*NY_NORTH, rout);

  return 0;

}
