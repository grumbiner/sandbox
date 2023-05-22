// Return the lat-long position of a given i,j coordinate
// Robert Grumbine
// 28 June 1999

#include <stdio.h>
#include <stdlib.h>
#include "ncepgrids.h"
#include "ssmi.h"
#include "icessmi.h"
#include "params.h"

int main(int argc, char *argv[]) {
  GRIDTYPE<unsigned char> n;
  char **x = NULL;
  ijpt loci;
  latpt locl;

  loci.i = strtol(argv[1], x, 10);
  loci.j = strtol(argv[2], x, 10);
  
  locl = n.locate(loci);
  printf("%3d %3d %7.3f %7.3f\n", loci.i, loci.j, locl.lat, locl.lon);

  return 0;
}

template <class T>
void xpmice(metricgrid<T> &newice, metricgrid<T> &land, char *fname) {
  ijpt x;
  latpt y;
  palette<unsigned char> gg(19, 65);
// Print out xpm files for graphic comparisons
  for (x.j = 0; x.j < newice.ypoints() ; x.j++) {
    for (x.i = 0; x.i < newice.xpoints() ; x.i++) {
       if (land[x] == LAND) {
         newice[x] = 0;
       }
       else if (land[x] == COAST) {
         newice[x] = 0;
       }
       if (newice[x] == NO_DATA) {
         newice[x] = 0;
       }
       else if (newice[x] > MAX_ICE) {
         newice[x] = 2;
       }
       else {
         newice[x] = 4 + min((unsigned char)100,newice[x])/7;
       }
    }
  }
  newice.xpm(&fname[0], 1, gg);

  return ;
}



////zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
// To complete writing
void recompute(metricgrid<ssmi> &x, metricgrid<unsigned char> &recomp) {
//recompute the ice concentration given the saved tb
  return;
}
// Extract a particular flagged field from a grid of ssmi type
void getfield(metricgrid<ssmi> &x, grid2<int> &y, int index) {
}
// This will treat 3 frequencies as R, G, B and then show a map thereof.
//   Use flags from icessmi for argument values.
void rgb(metricgrid<ssmi> &x, int r, int g, int b, char *name) {
  grid2<int> red(x.xpoints(), x.ypoints());
  grid2<int> green(x.xpoints(), x.ypoints());
  grid2<int> blue(x.xpoints(), x.ypoints());
  ijpt loc;
  getfield(x, red, r);
  getfield(x, green, g);
  getfield(x, blue, b);
  return;
}
// Donmap -- make an 80x60 ASCII display of the ice concentrations about
//   a given i,j
void donmap(metricgrid<ssmi> &x, ijpt &center, FILE *fout) {
  grid2<char> omap(79, 59);
  ijpt loc;
  // see donmap.c, remember to flip i,j
  return;
}

// Compute grid to grid deltas ... will want to think about delta
//   in the case of a flag value
// Many delta-computing programs to work from
template<class T>
void delta(metricgrid<T> &x, metricgrid<T> &y, metricgrid<T> &del) {
  ijpt loc;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    del[loc] = x[loc] - y[loc];
  }
  }
  return ;
}

// Compute the area and extent of a sea ice grid
//   note that declaring y a metricgrid doesn't work
// Will also want to manage flag values
// Many examples of area/extent computation
void area_extent(metricgrid<float> &x, metricgrid<unsigned char> &land, 
                 float &area, float &extent) {
  float scale = 1;
  ijpt loc;

  if (x.gridmax() > 3) scale = 0.01;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x[loc] > MAX_ICE*scale) x[loc] = 0;
    if (x[loc] > 100*scale && x[loc] <= MAX_ICE*scale) x[loc] = 1.0/scale;
  }
  } 
  area = x.integrate()*scale;

  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x[loc] > 0) x[loc] = 1.0;
  }
  }
  extent = x.integrate(); 

  return;
}
