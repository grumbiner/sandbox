#include <stdio.h>
#include <string.h>
#include "mvector.h"
#include "ncepgrids.h"

///////////////// GMT shorelines
#include "gshhs.h"
#include "geometry.C"
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


//////////////////////////////////////////////

size_t getseg(mvector<latpt> &locations, int &level, float &area, 
              float &west, float &east,
              float &north, float &south, FILE *fin) {
// Coastline file input
  struct POINT gshhspoint;
  struct GSHHS gshhsheader;
  int max_east = 270000000;
  
  int i; 
  latpt llat;
  size_t found;
  
  found = fread ( (void*) &gshhsheader, sizeof(struct GSHHS), 1, fin);
  if (found == (size_t) 0) return found;
    
  // We do need FLIP in Linux on Intel
  #ifdef FLIP
     gshhsheader.id = swabi4 ((unsigned int)gshhsheader.id);
     gshhsheader.n = swabi4 ((unsigned int)gshhsheader.n);
     gshhsheader.level = swabi4 ((unsigned int)gshhsheader.level);
     gshhsheader.west = swabi4 ((unsigned int)gshhsheader.west);
     gshhsheader.east = swabi4 ((unsigned int)gshhsheader.east);
     gshhsheader.south = swabi4 ((unsigned int)gshhsheader.south);
     gshhsheader.north = swabi4 ((unsigned int)gshhsheader.north);
     gshhsheader.area = swabi4 ((unsigned int)gshhsheader.area);
     gshhsheader.greenwich = swabi2 ((unsigned int)gshhsheader.greenwich);
     gshhsheader.source = swabi2 ((unsigned int)gshhsheader.source);
  #endif


    #ifdef VERBOSE
       printf("About to resize locations\n"); fflush(stdout);
    #endif 
    locations.resize(gshhsheader.n);
    #ifdef VERBOSE
       printf("back from resize locations\n"); fflush(stdout);
    #endif 
  
    for (i = 0; i < gshhsheader.n; i++) {
        
      if (fread ((void *)&gshhspoint, (size_t)sizeof(struct POINT),
                              (size_t)1,                  fin) != 1) {
        fprintf (stderr, 
             "gshhs:  Error reading for polygon %d, point %d.\n",
             gshhsheader.id, i);
          return 0;
        }
        #ifdef FLIP
          gshhspoint.x = swabi4 ((unsigned int)gshhspoint.x);
          gshhspoint.y = swabi4 ((unsigned int)gshhspoint.y);
        #endif
        //llat.lon = (gshhsheader.greenwich && gshhspoint.x > max_east) ? 
        //   gshhspoint.x * 1.0e-6 - 360.0 : gshhspoint.x * 1.0e-6;
        if (gshhsheader.greenwich && gshhspoint.x > max_east) {
          llat.lon = gshhspoint.x * 1.0e-6 - 360.0;
        }
        else {
          llat.lon = gshhspoint.x * 1.0e-6;
        }

        llat.lat = gshhspoint.y * 1.0e-6;

        locations[i].lon = llat.lon;
        locations[i].lat = llat.lat;
      }
      north = gshhsheader.north / 1.e6;
      south = gshhsheader.south / 1.e6;
      east = gshhsheader.east / 1.e6;
      west = gshhsheader.west / 1.e6;
      area = gshhsheader.area / 10.;
      level = gshhsheader.level;
// Cannot, at the moment, handle boundary crossings
      if (east * west == 0.) {
         printf("%d %d west, east\n",gshhsheader.west, gshhsheader.east);
         fflush(stdout);
         // If unmanageable, call again 
         getseg(locations, level, area, west, east, north, south, fin);
      }

    if (locations[0].lat != locations[gshhsheader.n-1].lat || 
        locations[0].lon != locations[gshhsheader.n-1].lon    ) {
      locations[gshhsheader.n-1] = locations[0];
      //printf("Resetting terminal point\n"); fflush(stdout);
    }
  
  return found;
}
template<class T>
void region_assess(psgrid<T> &field, psgrid<T> &land, char *polyname, 
                   mvector<latpt> &locations, double region_area) {
  double water_area = 0.0, land_area = 0.0, area = 0.0, extent = 0.0;
  ijpt loc;
  latpt ll;
  int npts = locations.xpoints();
  float north, south, east, west; //doing a box pre-selection on pts, vs. wn
  float minconc = 70;

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
  printf("%s %7.3f  %7.3f  %5.3f   %7.3f %7.3f %5.3f\n",
             polyname, area/1e9, extent/1e9, area/extent, 
             water_area/1.e9, land_area/1.e9, (land_area+water_area+extent)/
             region_area/1e6);

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
