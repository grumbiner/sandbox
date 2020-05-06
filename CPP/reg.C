
#include "ncepgrids.h"

#ifndef POINTSH
  #include "points.h"
#endif

class region {
  public:
    latpt ll, ur;
    region();
    region(latpt &, latpt &); 
    bool in(latpt &);
    void show();
};
void region::show() {
  printf("%f %f  %f %f\n",ll.lon, ll.lat, ur.lon, ur.lat);
}
region::region() {
  ll.lat = -90.0;
  ll.lon = 0.0;
  ur.lat = 90.0;
  ur.lon = 360.0;
}
region::region(latpt &x, latpt &y) {
  if (x.lat < y.lat) { 
    ll.lat = x.lat;
    ur.lat = y.lat;
  }
  else {
   ll.lat = y.lat;
   ur.lat = x.lat;
  }
  if (x.lon < 0 && y.lon < 0) {
    if (x.lon < y.lon) {
      ll.lon = x.lon + 360.;
      ur.lon = y.lon + 360.;
    }
    else {
      ll.lon = y.lon + 360.;
      ur.lon = x.lon + 360.;
    }
  }
  else if (x.lon > 0 && y.lon> 0.) {
    if (x.lon < y.lon) {
      ll.lon = x.lon;
      ur.lon = y.lon;
    }
    else {
      ll.lon = y.lon;
      ur.lon = x.lon;
    }
  }
  else {
    printf("incompatible sign for longitude, exiting\n");
    exit (1);
  }
}

bool region::in(latpt &x) {
// assumes that lons are positive only
  return (x.lat >= ll.lat && x.lat <= ur.lat &&
          x.lon >= ll.lon && x.lon <= ur.lon ) ;
}


void test(latpt &ll, latpt &ur, metricgrid<bool> &grid, ijpt &llij, ijpt &urij ) ;

int main(void) {
  northgrid<bool> grid;
  northgrid<float> conc;
  northgrid<unsigned char> fulland;
  psgrid<float> newpsconc;
  psgrid<unsigned char> newland;
  latpt ll, ur, tmp;
  ijpt llij, urij, loc;
  FILE *fin;
  palette<unsigned char> gg(19, 65);

  fin = fopen("n","r");
  conc.binin(fin);
  if (conc.average() < 2.5) conc *= 100.;
  fclose(fin);

  fin = fopen("nland","r");
  fulland.binin(fin);
  fclose(fin);
  ll.lat = 50.0; ll.lon = 155.;
  ur.lat = 75.0; ur.lon = 240.;

  test(ll, ur, grid, llij, urij);
  
  newpsconc.subset(conc, llij, urij);
  newland.subset(fulland, llij, urij);
  newpsconc.xpm("ak.xpm", 14, gg);

  conc.xpm("north.xpm", 14, gg);

  return 0;
}

void test(latpt &ll, latpt &ur, metricgrid<bool> &grid, ijpt &llij, ijpt &urij ) {
  region alaska(ll, ur);
  ijpt loc;
  latpt tmp;
  bool any;

  alaska.show();

  for (loc.j = 0; loc.j < grid.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < grid.xpoints(); loc.i++) {
    tmp = grid.locate(loc);
    if (tmp.lon < 0.) tmp.lon += 360.0;
    grid[loc] = alaska.in(tmp);
  }
  }

// Then loop across grid in ij space looking for first/last in any row that
  any = false;
  loc.j = 0;
  do {
    for (loc.i = 0; loc.i < grid.xpoints(); loc.i++) {
      if (grid[loc]) any = true;
    }
    loc.j++;
  } while ( !any && loc.j < grid.ypoints() );
  if (any) llij.j = loc.j-1;

  any = false;
  loc.j = grid.ypoints() - 1;
  do {
    for (loc.i = 0; loc.i < grid.xpoints(); loc.i++) {
      if (grid[loc]) any = true;
    }
    loc.j--;
  } while ( !any && loc.j >= 0 );
  if (any) urij.j = loc.j+1;


  any = false;
  loc.i = 0;
  do { 
    for (loc.j = 0; loc.j < grid.ypoints() ; loc.j++) {
       if (grid[loc]) any = true;
    }
    loc.i++;
  } while ( !any && loc.i < grid.xpoints() );
  if (any) llij.i = loc.i - 1;

  any = false;
  loc.i = grid.xpoints() - 1 ;
  do { 
    for (loc.j = 0; loc.j < grid.ypoints() ; loc.j++) {
       if (grid[loc]) any = true;
    }
    loc.i--;
  } while ( !any && loc.i > 0);
  if (any) urij.i = loc.i + 1;

  printf("ij of ll, ur %d %d  %d %d\n",llij.i, llij.j, urij.i, urij.j);


  return;
} 
