#include <stdio.h>
#include "icessmi.h"
#include "icegrids.h"

/* Extract a specified field from the full data files */
/* Wish: pass the hemisphere as an argument */
/* Wish: malloc rather than fixed array sizes */
/* Bob Grumbine 12 December 1994 */

int main(int argc, char *argv[])
{
  ssmi north[NY_NORTH][NX_NORTH];
  ssmi south[NY_SOUTH][NX_SOUTH];
  unsigned char  fld_n[NY_NORTH][NX_NORTH];
  unsigned char  fld_s[NY_SOUTH][NX_SOUTH];
  float rfld_n[NY_NORTH][NX_NORTH];
  float rfld_s[NY_SOUTH][NX_SOUTH];

  char a;
  int nfld;

  int i, j, x;
  FILE *nin, *sin, *nout, *sout;
  float lat, lon;
 
/* Open data files */
  nin   = fopen(argv[1], "r");
  sin   = fopen(argv[2], "r");
  nout  = fopen(argv[3], "w");
  sout  = fopen(argv[4], "w");

  printf("Which field would you like?\n");
  printf(" 19V = %1d\n",T19V);
  printf(" 19H = %1d\n",T19H);
  printf(" 22V = %1d\n",T22V);
  printf(" 37V = %1d\n",T37V);
  printf(" 37H = %1d\n",T37H);
  printf(" 85V = %1d\n",T85V);
  printf(" 85H = %1d\n",T85H);
  printf(" averaged conc = %1d\n",CONC_BAR);
  printf(" averaged tb conc = %1d\n",BAR_CONC);
  scanf("%1d",&nfld);
/*  nfld = atoi(a); */


/* Read in the northern hemisphere data */
  i = fread(north, sizeof(ssmi), (NX_NORTH)*(NY_NORTH), nin);
  if (i != (NX_NORTH)*(NY_NORTH)) {
    printf("Error reading in data, only %d bytes read, when %d are expected\n",
      i, (NX_NORTH)*(NY_NORTH)*sizeof(ssmi) );
  }
  else {
    getfld(&north[0][0], NX_NORTH*NY_NORTH, &fld_n[0][0], &rfld_n[0][0], nfld);
    if (nfld == CONC_BAR || nfld == BAR_CONC) {
      fwrite(fld_n, sizeof(unsigned char), NX_NORTH*NY_NORTH, nout);
     }
     else {
      fwrite(rfld_n, sizeof(float), NX_NORTH*NY_NORTH, nout);
     }
  }

/* Read in the southern hemisphere data */
  i = fread(south, sizeof(ssmi), (NX_SOUTH)*(NY_SOUTH), sin);
  if (i != (NX_SOUTH)*(NY_SOUTH)) {
    printf("Error reading in south data, only %d bytes read, when %d are expected\n",
      i, (NX_SOUTH)*(NY_SOUTH) );
  }
  else {
    getfld(&south[0][0], NX_SOUTH*NY_SOUTH, &fld_s[0][0], &rfld_s[0][0], nfld);
    if (nfld == CONC_BAR || nfld == BAR_CONC) {
      fwrite(fld_s, sizeof(unsigned char), NX_SOUTH*NY_SOUTH, sout);
    }
    else {
      fwrite(rfld_s, sizeof(float), NX_SOUTH*NY_SOUTH, sout);
    }
  }
      
  return -1;

}
