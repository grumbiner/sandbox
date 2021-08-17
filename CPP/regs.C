#include <stdio.h>
#include <string.h>
#include "mvector.h"
#include "ncepgrids.h"

///////////////// GMT shorelines
#include "geometry.C"
int bcount = 0;

///////////// Utilities
// Sbr to get the next segment:
int getseas(mvector<latpt> &locations, float &west, float &east,
              float &north, float &south, char *name, FILE *fin) ;
template<class T>
void region_assess(psgrid<T> &field, psgrid<T> &land, int polyno,
                   mvector<latpt> &locations) ;
void corners(mvector<latpt> &x, float &north, float &south, 
                                float &east, float &west)   ;

#define REGIONS 900
int main(int argc, char *argv[]) {
  FILE *fin;
  char name[900];
  int found;
  float min_conc, west, east, north, south;
  mvector<latpt> locations;
  latpt ll;
  northhigh<unsigned char> field, land;
  palette<unsigned char> gg(19,65);

  fin = fopen("northmap","r");
  land.binin(fin);
  fclose(fin);
  
  fin = fopen("sea_data","r");
// Note that in this version, we have name and perimeter simultaneously,
//   so can go sequentially through the input file
  found = 1;
  field.set(0);
  int i = 1;
  while (!feof(fin) && found > 0 ) {
    found = getseas(locations, west, east, north, south, name, fin);
    region_assess(field, land, i, locations);
    i += 1;
  }
  field.xpm("areas.xpm",1,gg);

  return 0;
}
