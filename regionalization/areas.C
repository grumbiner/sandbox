#include <stdio.h>
#include <string.h>
#include "mvector.h"
#include "ncepgrids.h"

///////////////// GMT shorelines
#include "gshhs.h"
//#include "geometry.C"
int bcount = 0;

///////////// Utilities
// Sbr to get the next segment:
size_t getseg(mvector<latpt> &locations, int &level, float &area, 
            float &west, float &east,
            float &north, float &south, FILE *fin);
template<class T>
void region_assess(psgrid<T> &field, psgrid<T> &land, char *polyname, 
                   mvector<latpt> &locations, double region_area) ;
void corners(mvector<latpt> &x, float &north, float &south, 
                                float &east, float &west)   ;

#define REGIONS 900
int main(int argc, char *argv[]) {
  FILE *fin;
  char name[900];
  char arname[REGIONS][900];
  float lat[REGIONS], lon[REGIONS];
  bool used[REGIONS];
  int lakeno, n, i = 0;
  int level, found;
  float area, west, east, north, south;
  mvector<latpt> locations;
  bool match;
  latpt ll;
  northhigh<unsigned char> field, land;

  fin = fopen("names","r");
  while (!feof(fin)) {
    fscanf(fin, "%f %f ",&lon[i], &lat[i]);
    if (lon[i] < 0) lon[i] += 360.;
    fgets(&name[0],800,fin);
    // needed to remove the carriage return
    n = strlen(&name[0]);
    name[n-1] = '\0';
    strncpy(&arname[i][0], name, 800); 
    used[i] = false;
    i += 1;
  }
  n = i - 1;
  fclose(fin);

  fin = fopen("lakes","r");
  lakeno = 0;
  while (!feof(fin) ) {
    found = getseg(locations, level, area, west, east, north, south, fin);
    printf("lakeno = %d area = %f  %f %f %f %f\n",lakeno, area, west, east, north, south); fflush(stdout);
    lakeno++;
  }

  return 0;
}
