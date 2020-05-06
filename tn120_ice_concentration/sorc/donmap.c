#include <stdio.h>
#include <stdlib.h>
#include "ssmi.h"
#include "icessmi.h"
#include "icegrids.h"

/* Read in an ssmi map and write out an ASCII text for a given center
   lat long, suitable for printing on 80*60 paper */

int main(int argc, char *argv[])
{
  FILE *nin, *nout;
  unsigned char nmap[NY_NORTH][NX_NORTH];
  char omap[59][79];
  unsigned char x;
  char y;
  float lat, longit;
  int i, j, iref, jref, imin, imax, jmin, jmax;

  nin = fopen(argv[1], "r");
  if (nin == NULL) {
    printf("Failed to open the ssmi map\n");
    return -1;
  }
  fread (nmap, sizeof(unsigned char), NX_NORTH*NY_NORTH, nin);

  lat = atof(argv[2]);
  longit = atof(argv[3]);
  printf("Working with lat, long = %f %f\n",lat, longit);

  mapll(lat, longit, &iref, &jref, xorig_NORTH, yorig_NORTH, eccen2,
        slat_NORTH, slon_NORTH, rearth, dx, dy, sgn_NORTH);
  printf("reference i j = %3d %3d\n",iref, jref);


  imin = iref - 37;
  imax = iref + 37;
  jmin = jref - 27;
  jmax = jref + 27;
  if (imin < 0 ) imin = 0;
  if (jmin < 0 ) jmin = 0;
  if (imax > NX_NORTH) imax = NX_NORTH;
  if (jmax > NY_NORTH) jmax = NY_NORTH;

  for (i = 0; i < 79; i++)
  { for (j = 0; j < 59; j++)
    { omap[j][i] = 'B' ;
    }
  }


/* Now begin the character remapping of the sub-field */
  printf("starting map \n");
  printf("    ");
  for (i = imin ; i <= imax ; i++)
  { printf("%01d", (i ) / 100 ) ;
  }
  printf("\n");
  printf("    ");
  for (i = imin ; i <= imax ; i++)
  { printf("%01d", ( (i ) / 10) % 10         ) ;
  }
  printf("\n");
  printf("    ");
  for (i = imin ; i <= imax ; i++)
  { printf("%01d",  (i ) % 10         ) ;
  }
  printf("\n");

  for (j = jmax; j >= jmin ; j-- )
  {  
     printf("%3d ", j ) ;
     for (i = imin; i <= imax ; i++)
     {  
        y = '.';
        x = nmap[j][i];
        if ( (int) x == LAND ) y = 'X';
        if ( (int) x == COAST ) y = 'C';
        if ( (int) x == NO_DATA) y = 'N';
        if ( (int) x == BAD_DATA) y = 'B';
        if ( (int) x >= 100 && x < LAND) x = 99;
        if ( (int) x < 100 ) sprintf(&y,"%1d",(int) ( x / 10 ) ) ;
        
        omap[j-jmin][i-imin] = y ;
        printf("%c",y);
     }
     printf("\n");
  }


  return 0;

} 
