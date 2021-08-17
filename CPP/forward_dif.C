#include <stdio.h>
#include "ncepgrids.h"
// 4 Mar 2005  Robert Grumbine

// Compute statistics inside lat-long polygons
///////////////// Geometric routines
#include "geometry.C"
int bcount = 0;

int main(int argc, char *argv[]) {
  FILE *fin, *polyin;
  northhigh<unsigned char> field, out;

  char polyname[9000];
  float north, south, east, west;
  mvector<latpt> locations;

  ijpt loc;
  int i, npts;
  latpt ll;
  palette<unsigned char> gg(19, 65);

  double area = 0., extent = 0.;

////////////////////////////////////////
  // Read in data
  fin = fopen(argv[1], "r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open %s\n",argv[1]);
    return 1;
  }
  field.binin(fin);
  fclose(fin);

  // Read in polygon
  polyin = fopen(argv[2], "r");

  while (!feof(polyin) ) {
    fscanf(polyin, "%s\n", polyname);
    fscanf(polyin, "%d %f %f %f %f\n",&npts, &north, &south, &east, &west);
    locations.resize(npts);
    for (i = 0; i < npts; i++) {
      fscanf(polyin, "%f %f\n",&locations[i].lon, &locations[i].lat);
      if (locations[i].lon < 0.) locations[i].lon += 360.;
    }
    fflush(stdout);

  ////////////// Work on the sectioned field -- to become
  //////////////    function
    out.set(0);
    area = 0.0;
    extent = 0.0;
    for (loc.j = 0; loc.j < out.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < out.xpoints(); loc.i++) {
      ll = field.locate(loc);
      if (ll.lon < 0.) ll.lon += 360.;
      if (wn_PnPoly(ll, locations, npts ) != 0 ) {
        out[loc] = field[loc];
      }
      if (out[loc] > 15 && out[loc] < 128) {
        if (out[loc] > 100) out[loc] = 100;
        area += out[loc] / 100. * out.cellarea(loc);
      }
      if (out[loc] > 15 && out[loc] < 128) extent += out.cellarea(loc);
    }
    }
    printf("%s area = %f  extent = %f million km^2 %f avg conc\n",polyname,
               area/1e12, extent/1e12, area/extent );
   

  
  }

  return 0;
}
