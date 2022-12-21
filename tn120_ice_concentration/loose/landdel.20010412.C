#include "ncepgrids.h"

// Program to intercompare land mask files --
//   read in two files
//   construct images of them
//   construct delta maps
//   compute integral statistics for land area, land in one and not the
//      other, ocean

template <class T>
float flagarea(GRIDTYPE<T> &x, int flag) ;

#define LAND 157
#define COAST 195

int main(int argc, char *argv[]) {
  GRIDTYPE<DTYPE> old, newer;
  GRIDTYPE<float> delta;
  FILE *fin1, *fin2;
  
  ijpt loc;
  latpt ll;
  palette<unsigned char> gg(19,65);

//Get data
  fin1 = fopen(argv[1], "r");
  fin2 = fopen(argv[2], "r");
  if (fin1 == (FILE *) NULL) {
    printf("Failed to open the old land file\n");
    return 1;
  }
  if (fin2 == (FILE *) NULL) {
    printf("Failed to open the new land file\n");
    return 2;
  }
  old.binin(fin1);
  newer.binin(fin2);
 
  printf("max, min, average on old grid: %f %f %f \n",
         (float) old.gridmax(), (float) old.gridmin(), (float) old.average() );
  printf("max, min, average on new grid: %f %f %f \n",
         (float) newer.gridmax(), (float) newer.gridmin(), (float) newer.average() );
  old.xpm("old.xpm", 14, gg);
  newer.xpm("new.xpm", 14, gg);

  printf("old area of land %f\n",flagarea(old, LAND)/1.e12 );
  printf("old area of coast %f\n",flagarea(old, COAST)/1.e12 );
  printf("old area of ocean %f\n",flagarea(old, 0)/1.e12 );
  printf("new area of land %f\n",flagarea(newer, LAND)/1.e12 );
  printf("new area of coast %f\n",flagarea(newer, COAST)/1.e12 );
  printf("new area of ocean %f\n",flagarea(newer, 0)/1.e12 );

//Compute delta map, shift in to range

  for (loc.j = 0; loc.j < delta.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < delta.xpoints(); loc.i++) {
    if (old[loc] != newer[loc] ) {
      delta[loc] = old[loc];
      delta[loc] -= newer[loc];
      ll = delta.locate(loc);
      printf("old new location %3d %3d %7.3f %8.3f\n",
              old[loc], newer[loc], ll.lat, ll.lon);
    }
    else {
      delta[loc] = 0.0;
    }
  }
  }
  delta -= delta.gridmin();
  delta /= 4.0;
  delta.xpm("delta.xpm", 7, gg);

  return 0;
}
template <class T>
float flagarea(GRIDTYPE<T> &x, int flag) {
  ijpt loc;
  GRIDTYPE<float> delta;  

  for (loc.j = 0; loc.j < delta.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < delta.xpoints(); loc.i++) {
    if (x[loc] == (DTYPE) flag) {
      delta[loc] = 1.0;
    }
    else {
      delta[loc] = 0.0;
    }
  }
  }

  return delta.integrate();
}
