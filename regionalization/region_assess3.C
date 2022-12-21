#include "metric.h"
#include "mvector.h"
#include "points.h"
#include "corners.C"
#include "geometry.C"

// Variant -- fill in with incremental flag
template<class T>
void region_assess(psgrid<T> &field, psgrid<T> &land, int polyno,
                   mvector<latpt> &locations) {
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
      if (field[loc] != 0) {
        printf("overlap at %d %d between %d and %d\n",loc.i, loc.j, field[loc], polyno);
      }
      else {
        field[loc] = polyno; 
      }
    }
  }
  }
  fflush(stdout);

  return;
}
