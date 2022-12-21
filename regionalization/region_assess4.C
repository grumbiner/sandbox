#include "metric.h"
#include "mvector.h"
#include "points.h"
#include "corners.C"
#include "geometry.C"

template<class T>
void region_assess(llgrid<T> &field, llgrid<T> &land, char *polyname, 
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
