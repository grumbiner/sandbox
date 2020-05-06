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


//////////////////////////////////////////////

int getseas(mvector<latpt> &locations, float &west, float &east,
              float &north, float &south, char *name, FILE *fin) {
// Varying from getseg, this is to get ascii-crafted segments
  int i, n, npts; 
  latpt llat;

  fgets(name, 900, fin);
  // needed to remove the carriage return, and pad for columnar output
  n = strlen(&name[0]);
  for (i = n-1; i < 21; i++) {
    name[i] = ' ';
  }
  name[21] = '\0';
  //printf("%s\n",name);
  fscanf(fin,"%d %f %f\n",&npts, &llat.lat, &llat.lon);
  //printf("%d %f %f\n",npts, llat.lat, llat.lon);
  locations.resize(npts);
  
  for (i = 0; i < npts; i++) {
    fscanf(fin,"%f %f\n",&llat.lat, &llat.lon);
    //printf("%f %f\n",llat.lat, llat.lon);
    if (llat.lon < 0) llat.lon += 360.;
    locations[i] = llat;
  }
  fflush(stdout);

  if (locations[0].lat != locations[npts-1].lat || 
      locations[0].lon != locations[npts-1].lon    ) {
    printf("error, last point must be same as first\n");
    fflush(stdout);
    return -1;
  }
  corners(locations, north, south, east, west);

  return npts;
}
template<class T>
void region_assess(psgrid<T> &field, psgrid<T> &land, char *polyname, 
                   mvector<latpt> &locations, float &minconc) {
  double water_area = 0.0, land_area = 0.0, area = 0.0, extent = 0.0;
  ijpt loc;
  latpt ll;
  int npts = locations.xpoints();
  float north, south, east, west; //doing a box pre-selection on pts, vs. wn

  corners(locations, north, south, east, west);
  if (east < 0) east += 360.;
  if (west < 0) west += 360.;

  for (loc.j = 0; loc.j < field.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < field.xpoints(); loc.i++) {
    ll = field.locate(loc); 
    if (ll.lon < 0.) ll.lon += 360.;
    if (ll.lon > east || ll.lon < west || 
        ll.lat > north || ll.lat < south) continue;

    if (wn_PnPoly(ll, locations, npts ) != 0 ) {
      if (land[loc] > 0) {
        land_area += field.cellarea(loc);
      }
      else if (field[loc] >= minconc && field[loc] < 128) {
        if (field[loc] > 100) field[loc] = 100;
        area   += field[loc] / 100. * field.cellarea(loc);
        extent += field.cellarea(loc);
      }
// Note that this skips 'weather' flagged water
      else if (field[loc] < minconc && land[loc] == 0) {
        water_area += field.cellarea(loc);
      }
    }
  }
  }
  extent += 1e-3; // added to avoid problems with the division for concentration
  printf("%s %9.3f  %9.3f  %5.3f   %9.3f %9.3f\n",
             polyname, area/1e9, extent/1e9, area/extent, 
             water_area/1.e9, land_area/1.e9 );
  fflush(stdout);

  return;
}
void corners(mvector<latpt> &x, float &north, float &south, 
                                float &east, float &west)   {
  north = -95.0;
  south = +95.0;
  east = -400.0;
  west =  400.0;
  for (int i = 0; i < x.xpoints(); i++) {
    north = max(north, x[i].lat);
    south = min(south, x[i].lat);
    east  = max(east, x[i].lon);
    west  = min(west, x[i].lon);
  }  

  return ;
}
