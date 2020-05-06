#include <stdio.h>
#include <string.h>
#include "mvector.h"
#include "ncepgrids.h"

// Work on a polar stereographic grid to assess a set of regions 
//   (defined in the inputs, getseas)
// Robert Grumbine
// 11 Mar 2005

///////////////// GMT shorelines
#include "geometry.C"
int bcount = 0;

///////////// Utilities
// Sbr to get the next segment:
int getseas(mvector<latpt> &locations, float &west, float &east,
              float &north, float &south, char *name, FILE *fin) ;
template<class T>
void region_assess(psgrid<T> &field, psgrid<T> &land, char *polyname, 
                   mvector<latpt> &locations, float &min_conc) ;
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

  fin = fopen(argv[1],"r");
  field.binin(fin);
  fclose(fin);
  min_conc = atof(argv[2]);
  //printf("minimum concentration = %f\n",min_conc);
//  field.set(100);
  fin = fopen("northmap","r");
  land.binin(fin);
  fclose(fin);
  
  fin = fopen("sea_data","r");
// Note that in this version, we have name and perimeter simultaneously,
//   so can go sequentially through the input file
  found = 1;
  while (!feof(fin) && found > 0 ) {
    found = getseas(locations, west, east, north, south, name, fin);
    region_assess(field, land, name, locations, min_conc);
  }

  return 0;
}
