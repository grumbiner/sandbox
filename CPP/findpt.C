#include <stdio.h>
#include <stdlib.h>

#include "ncepgrids.h"

//Utility to find the latitude-longitude corresponding to an input
//  i,j location.
//Robert Grumbine
//18 August 1998

int main(int argc, char *argv[]) {
  ijpt x;
  latpt lloc, irloc;
  fijpt cfsirrloc;
  GRIDTYPE<float> datagrid;

  x.i = atoi(argv[1]);
  x.j = atoi(argv[2]);
  lloc = datagrid.locate(x);
  printf("latitude-longitude %f %f for grid point %d %d\n",
               lloc.lat, lloc.lon, x.i, x.j);

  return 0;
}
