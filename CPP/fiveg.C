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
void region_assess(llgrid<T> &land, char *polyname, mvector<latpt> &locations, 
                   float north, float south, float east, float west,
                   float &min_conc, char *argv[]) ;
void corners(mvector<latpt> &x, float &north, float &south, 
                                float &east, float &west)   ;

#define REGIONS 900
int main(int argc, char *argv[]) {
  FILE *fin;
  char name[900];
  int found;
  float min_conc, west, east, north, south;
  mvector<latpt> locations;
  global_12th<unsigned char> land;
  palette <unsigned char> gg(19,65);
  llgrid<unsigned char> mask;

  min_conc = atof(argv[1]);
  fin = fopen("globmap","r");
  land.binin(fin);
  fclose(fin);
  //land.xpm("land.xpm",13,gg);

  fin = fopen("sea_data","r");
// Note that in this version, we have name and perimeter simultaneously,
//   so can go sequentially through the input file
  found = 1;
  while (!feof(fin) && found > 0 ) {
    found = getseas(locations, west, east, north, south, name, fin);
    region_assess(land, name, locations, north, south, east, west, min_conc, argv);
  }

  return 0;
}
