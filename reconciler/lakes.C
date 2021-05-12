#include "ncepgrids.h"
#include "buoy.h"
#include "gshhs.h"

// Construction of an auxiliary mask, devoted to sea ice interests

// 1) Go through gshhs files and extract (echo full info out to a 
//      separate file) all the inland water bodies (2) islands in 
//      them (3) and lakes on those (4)
//    -- this limits file size that we do further work on in reruns or
//    alternate processing

// 2) While doing that, loop over grid for all 2 and 4 types and flag
//    as water points inside the curves.  For 3, flip water flags to land.
//    if inside but 'too close' to curve, flag as coast

// 3) Loop over grid and for all points 'too close_2' to inland water, 
//    flag as bypass. 


float dist_to_polygon(mvector<latpt> &locations, latpt &trial) ;
void set_flags(GRIDTYPE<unsigned char> &mask, mvector<latpt> &locations, 
               int type, double north, double south, double east, double west,
               unsigned char land, unsigned char water, 
               unsigned char bypass, unsigned char undef, float too_close) ;
void bypass_scan(metricgrid<unsigned char> &mask, unsigned char land, 
            unsigned char water, unsigned char coast, unsigned char bypass,
            float near) ; 
size_t getseg(mvector<latpt> &locations, int &level, float &west, float &east,
            float &north, float &south, FILE *fin, FILE *fout) ;

int bcount = 0;
#include "geometry.C"

int main(int argc, char *argv[]) {
  GRIDTYPE<unsigned char> mask, mask2;
  FILE *fin, *fout, *fout2;
// SSMI grid conventions
  unsigned char land = 157, coast = 195, water = 0, bypass = 194, undef = 224;
  float too_close = 12.7e3/2., too_close2 = 50e3; //meters
// gshhs stuff
  mvector<latpt> locations;
  int type = 0;
  float east, west, north, south;

  fin   = fopen(argv[1], "r");
  fout  = fopen(argv[2], "w");
  fout2 = fopen(argv[3], "w");
  if (fin == (FILE*) NULL) {
    printf("failed to open input file %s\n",argv[1]);
    return 1;
  }
  if (fout == (FILE*) NULL) {
    printf("failed to open output file for inland polygons %s\n",argv[2]);
    return 2;
  }
  if (fout2 == (FILE*) NULL) {
    printf("failed to open output file for inland masks %s\n",argv[3]);
    return 2;
  }
  mask.set(undef);
//

  while (!feof(fin)) {
    // get a gshhs record
    // if type 1, continue, else echo to fout 
    // else
    //  echo to fout
    //  label appropriately every point inside the curve 
    // getseg and set_flags know about these policies
    getseg(locations, type, west, east, north, south, fin, fout);
    set_flags(mask, locations, type, north, south, east, west, 
              land, water, bypass, undef, too_close);
    // 
  }
  fclose(fout);
  fclose(fin);

// do bypass scan
  bypass_scan(mask, land, water, coast, bypass, too_close2);

  mask.binout(fout2);
  fclose(fout2);

  return 0;


}

/////////////////////////////////////
void bypass_scan(metricgrid<unsigned char> &mask, unsigned char land, 
            unsigned char water, unsigned char coast, unsigned char bypass,
            float near) { 
  float tdx, tdy;
  ijpt loc, iloc;
  latpt ll1, ll2;
  int buf;

  loc.i = mask.xpoints()/2;
  loc.j = mask.ypoints()/2;
  ll1 = mask.locate(loc);
  loc.i += 1;
  ll2 = mask.locate(loc);
  tdx = 1e3*arcdis_(ll1.lon, ll1.lat, ll2.lon, ll2.lat);
  loc.j += 1;
  tdy = 1e3*arcdis_(ll1.lon, ll1.lat, ll2.lon, ll2.lat);
// set up a buffer size to be used around grid points such that
//   the bypass scan will only need to look this far out from each
//   point.  A large buffer (5x) is used to ensure that even with
//   say, pole convergence, we'll have enough points to check anything
//   'near'.
  buf = (int) (0.5 + 5.*near/min(tdx, tdy) );
  printf("buffer size is %d\n",buf); fflush(stdout);
  buf = max(buf, 5);
   

// loop over all points 'near' water and reflag as bypass
  for (loc.j = 0; loc.j < mask.ypoints(); loc.j++ ) {
  for (loc.i = 0; loc.i < mask.xpoints(); loc.i++ ) {
    if (mask[loc] == water) {
      printf("found a water point at %d %d\n",loc.i, loc.j); fflush(stdout);
      ll1 = mask.locate(loc);

      for (iloc.j = max(0,loc.j - buf);  
           iloc.j <= min(mask.ypoints() - 1, loc.j + buf) ; iloc.j++ ) {
        //printf("iloc.j = %d\n",iloc.j); fflush(stdout);
      for (iloc.i = max(0,loc.i - buf); 
           iloc.i <= min(mask.xpoints() - 1, loc.i + buf) ; iloc.i++ ) {

        //printf("loc %d %d  iloc %d %d\n",loc.i, loc.j, iloc.i, iloc.j); fflush(stdout);
        ll2 = mask.locate(iloc);
        tdx = 1e3*arcdis_(ll1.lon, ll1.lat, ll2.lon, ll2.lat);
        if (mask[iloc] != water && tdx < near  ) {
          printf("bypassing %d %d formerly %d\n",iloc.i, iloc.j, mask[iloc]);
          mask[iloc] = bypass;
        }
      }
      }

    } // end of if = water

  }
  }

  return;
}

/////////////////////////////////////
float dist_to_polygon(mvector<latpt> &locations, latpt &trial) {
  int i;
  float dist, mindist = FLT_MAX;

  for (i = 0; i < locations.xpoints(); i++) {
    // must convert arcdis from km to m
    dist = 1e3*arcdis_(locations[i].lon, locations[i].lat, trial.lon, trial.lat);
    mindist = min(dist, mindist);
  }

  return mindist;
}

/////////////////////////////////////
// Loop over grid and if point is inside polygon, flag according to type
void set_flags(GRIDTYPE<unsigned char> &mask, mvector<latpt> &locations, 
               int type, double north, double south, double east, double west,
               unsigned char land, unsigned char water, 
               unsigned char bypass, unsigned char undef, float too_close) {
  ijpt loc;
  unsigned char local;
  latpt ll, tlat;
  float lonmax = 360., lonmin = 0;
  int mini, minj, maxi, maxj;
  fijpt floc;
  

  if (type == 1) return;
  if (type == 2 || type == 4) local = water;
  if (type == 3) local = land;

// This segment, finding min max coordinates is to avoid checking all
//    zillion points on the grid and just those which might actually be
//    inside the given polygon
 if (mask.in(north, west) || mask.in(north,east) || mask.in(south, west) || mask.in(south,east)) {
  tlat.lat = north;
  tlat.lon = west;
  floc = mask.locate(tlat);
  mini = (int) floc.i;
  minj = (int) floc.j;
  maxi = (int) (0.5 + floc.i);
  maxj = (int) (0.5 + floc.j);

  tlat.lon = east;
  floc = mask.locate(tlat);
  mini = min(mini, (int) floc.i);
  minj = min(minj, (int) floc.j);
  maxi = max(maxi, (int) (0.5 + floc.i) );
  maxj = max(maxj, (int) (0.5 + floc.j) );

  tlat.lat = south;
  floc = mask.locate(tlat);
  mini = min(mini, floc.i);
  minj = min(minj, floc.j);
  maxi = max(maxi, (int) (0.5 + floc.i) );
  maxj = max(maxj, (int) (0.5 + floc.j) );

  tlat.lon = west;
  floc = mask.locate(tlat);
  mini = min(mini, floc.i);
  minj = min(minj, floc.j);
  maxi = max(maxi, (int) (0.5 + floc.i) );
  maxj = max(maxj, (int) (0.5 + floc.j) );
 }
 else {
   mini = 0;
   minj = 0;
   maxi = 0;
   maxj = 0;
 }

// Add some buffering so that we certainly encompass the entire possible curve
  mini -= 5;
  minj -= 5;
  maxi += 5;
  maxj += 5;
  if (mini < 0) mini = 0;
  if (minj < 0) minj = 0;
  if (maxi > mask.xpoints() - 1) maxi =  mask.xpoints() - 1;
  if (maxj > mask.ypoints() - 1) maxj =  mask.ypoints() - 1;

//printf("first, last location %f %f  %f %f\n",locations[0].lat, locations[0].lon, 
//       locations[locations.xpoints() - 1].lat, locations[locations.xpoints() - 1].lon);

  //for (loc.j = minj; loc.j < mask.ypoints(); loc.j++) {
  //for (loc.i = mini; loc.i < mask.xpoints(); loc.i++) {
  for (loc.j = minj; loc.j < maxj;  loc.j++) {
  for (loc.i = mini; loc.i < maxi;  loc.i++) {
     ll = mask.locate(loc);

     if (ll.lon > lonmax && ll.lon - 360. > lonmin) ll.lon -= 360.;
     if (ll.lon < lonmin && ll.lon + 360. < lonmax) ll.lon += 360.;

     if (wn_PnPoly(ll, locations, locations.xpoints() ) != 0) {
       if (mask[loc] != undef) {
         printf("re-examining point %d %d, val %d local %d\n",
                loc.i, loc.j, mask[loc], local);
       }
       if (dist_to_polygon(locations, ll)  < too_close) {
         mask[loc] = bypass;
       }
       else {
         mask[loc] = local;
       }
     }
  }
  }

  return;
}

size_t getseg(mvector<latpt> &locations, int &level, float &west, float &east,
            float &north, float &south, FILE *fin, FILE *fout) {
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

// Novel to lakes, write back out if we're not an island/continent
  if (gshhsheader.level > 1) {
    fwrite ( (void*) &gshhsheader, sizeof(struct GSHHS), 1, fout);
  }
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
// Novel to lakes, write back out if we're not an island/continent
      if (gshhsheader.level > 1) {
          fwrite ((void *)&gshhspoint, (size_t)sizeof(struct POINT),
                                  (size_t)1,                  fout);
      }
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
      level = gshhsheader.level;
// Cannot, at the moment, handle boundary crossings
      //if (east * west == 0.) {
      //   printf("%d %d west, east\n",gshhsheader.west, gshhsheader.east);
      //   fflush(stdout);
      //   // If unmanageable, call again 
      //   getseg(locations, level, west, east, north, south, fin);
      //}

    if (locations[0].lat != locations[gshhsheader.n-1].lat ||
        locations[0].lon != locations[gshhsheader.n-1].lon    ) { 
      locations[gshhsheader.n-1] = locations[0];
      printf("Resetting terminal point\n"); fflush(stdout);
    }

  return found;
}
