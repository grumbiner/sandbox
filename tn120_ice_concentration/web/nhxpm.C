#include <stdio.h>
#include <stdlib.h>
#include <f2c.h>

#include "ncepgrids.h"

#define LAND 157
#define COAST 195
#define NO_DATA 224
#define WEATHER 177

int main(int argc, char *argv[])
{
  FILE *sin, *slin;
  palette gg(19,65);
  northgrid<unsigned char> smap, sland;
  northgrid<float> sfloat;
  ijpt x;

  sin = fopen(argv[1], "r");
  slin = fopen(argv[2], "r");

  if (sin == NULL || slin == NULL ) {
    printf("failed to open a file!\n");
    return -1;
  }

  smap.binin(sin);
  sland.binin(slin);
// Apply mask:
  for (x.j = 0; x.j < smap.ypoints() ; x.j++) {
  for (x.i = 0; x.i < smap.xpoints() ; x.i++) {
    sfloat[x] = smap[x];
    if (sland[x] == LAND || sland[x] == COAST || 
        smap[x] == NO_DATA || smap[x] == WEATHER ) {
      sfloat[x] = 0.0;
    }
// Revise for color table:
       if (sland[x] == LAND) {
         smap[x] = 0;
       }
       else if (sland[x] == COAST) {
         smap[x] = 1;
       }
       else if (smap[x] == NO_DATA) {
         smap[x] = 2;
       }
       else if (smap[x] == WEATHER) {
         smap[x] = 3;
       }
       else {
         smap[x] = 4 + min(100,smap[x])/7;
       }

  }
  }

  smap.xpm(argv[3], 1, gg);

  printf("Million km^2 NH ice area %6.3f  ",sfloat.integrate()/1.e12/100.);

  for (x.j = 0; x.j < smap.ypoints() ; x.j++) {
  for (x.i = 0; x.i < smap.xpoints() ; x.i++) {
    if (sfloat[x] > 0.0) sfloat[x] = 1.0;
  }
  }
  printf(" Ice extent %6.3f\n",sfloat.integrate()/1.e12);



  return 0;
}
