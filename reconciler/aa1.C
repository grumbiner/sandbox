#include "ncepgrids.h"
#include "resops.h"

// Read in GMT coastline file and flag a target grid appropriately.
// Flags are:
//   0 Coast (not used in this program)
//   1 Boundary
//   3 Undefined
//   5 Land
//  15 Ocean
//  16 Final_ocean (not used in this program)
//  17 Water (outside of ocean)

// Robert Grumbine 19 August 2003

///////////////// Geometric routines
#include "geometry.C"

///////////////// GMT shorelines
#include "gshhs.h"

///////////// Utilities
// SBR to fill fields
void field_fill(mvector<latpt> &locations, 
                FAMILY<unsigned char> &field, 
                unsigned char boundary, unsigned char fill, 
                unsigned char unfill, unsigned char undef, int npts ) ;

// Sbr to get the next segment:
size_t getseg(mvector<latpt> &locations, int &level, float &west, float &east,
            float &north, float &south, FILE *fin);

/////////////////////////////////
// Function to ingest a set of points on a line, and the target grid
//    (the latter for mapping purposes only) and produce a set which
//    is trimmed to those which correspond to unique grid points on the
//    target.
void line_thin(mvector<latpt> &locations, FAMILY<unsigned char> &field);

// Find the corner points in grid IJ space for a set of input points
void corners(mvector<ijpt> &V, ijpt &ll, ijpt &lr, ijpt &ur, ijpt &ul) ;

///////////////////////////////////////////////////////////////////
int bcount;

int main(int argc, char *argv[]) {
  FILE *fin, *fout;

  FAMILY<unsigned char> field;
  TARGET<unsigned char> tot_field;

  mvector<latpt> locations;

  float north, south, east, west;
  unsigned char boundary = 1, land = 5, water = 17, ocean = 15, undef = 3;
  unsigned char fill, unfill;

  ijpt loc, tloc;
  fijpt fij1;
  latpt ll;
  int j, k;
  int level;
  size_t found;
  palette<unsigned char> gg(19, 65);
  char fname[900];

////////////////////////////////////////
  tot_field.set( (unsigned char) undef);

  fin = fopen(argv[1], "r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open %s\n",argv[1]);
    return 1;
  }
  j = 0;
  k = 0;
  found = (size_t) 1;
  while (!feof(fin) && found != (size_t) 0) {
    k += 1;
    #ifdef VERBOSE
      printf("k = %d\n",k); fflush(stdout);
      printf("j = %d found = %d\n",j,(int) found); fflush(stdout);
    #endif
    found = getseg(locations, level, west, east, north, south, fin);
    if (found == (size_t) 0) {
      #ifdef VERBOSE
        printf("found = 0, breaking out of loop\n");
      #endif
      break;
    }
//DEBUG
    if (k != 6) continue;
    //if (k > 20) break;


//  Construct a field grid around the bounds of the segment:
//  This if test is to avoid using overkill on the various small grids.
    printf("k %d\n",k);
    if (k < 15) {
    #if !defined(resops) 
      field.resize(tot_field.xpoints(), tot_field.ypoints() );
      field.dlat = tot_field.dlat;
      field.dlon = tot_field.dlon;
      field.firstlat = tot_field.firstlat;
      field.firstlon = tot_field.firstlon; 
      //printf("equating field and tot_field\n"); fflush(stdout);
    #endif
    field = tot_field;
    }
    else {
      tot_field.subset(field, north+2., south-2., east+2., west-2.);
      //printf("subsetting %d\n",k); fflush(stdout);
    }
    if (field.xpoints() == 0 || field.ypoints() == 0) continue;

//  Initialize the field to undefined
    field.set( (unsigned char) undef);

//  Set the flags for filling
    if (level == 1 || level == 3 ) {
      fill = land;
      unfill = water;
    }
    else if (level == 2 || level == 4 ) {
      fill = water;
      unfill = land;
    }
    else {
      printf("j = %d unknown level = %d\n",j,level); fflush(stdout);
    }

    if (locations.xpoints() > 2) {
      line_thin(locations, field);
    }
    if (locations.xpoints() < 3) {
      j += 1;
      continue;
    }


//  Fill inside the bounding curve
//     -- this will invoke boundary fill, which is where there can be stack
//         problems.
    field_fill(locations, field,
                  boundary, fill, unfill, undef, locations.xpoints() ) ;
    #ifdef VERBOSE
      sprintf(fname, "field%d.xpm",j); 
      field.xpm(fname, 1, gg);
    #endif 

 // Now incorporate the relevant parts of field into tot_field
    for (tloc.j = 0; tloc.j < field.ypoints(); tloc.j++ ) {
    for (tloc.i = 0; tloc.i < field.xpoints(); tloc.i++ ) {
      ll = field.locate(tloc);
      #if defined(resops)
        if (ll.lon - tot_field.firstlon() > 360.) {
        printf("minlon reset %f %f\n",ll.lon, tot_field.firstlon());
      #else
        if (ll.lon - tot_field.firstlon > 360.) {
        printf("minlon reset %f %f\n",ll.lon, tot_field.firstlon);
      #endif
        fflush(stdout);
        ll.lon -= 360.;
      }
      fij1 = tot_field.locate(ll);
      loc = fij1;
      if ( (field[tloc] == fill || field[tloc] == boundary) && 
            tot_field.in(loc) ) {
        tot_field[loc] = field[tloc];
      }
    }
    } 

    j += 1;
  } // end looping through segments
  fclose(fin);

  for (loc.j = 0; loc.j < tot_field.ypoints(); loc.j++ ) {
  for (loc.i = 0; loc.i < tot_field.xpoints(); loc.i++ ) {
    if (tot_field[loc] == undef) {
      tot_field[loc] = ocean;
    }
  }
  } 

  tot_field.xpm(argv[3], 1, gg);
  fout = fopen(argv[2],"w");
  tot_field.binout(fout);
  fclose(fout);
   
  return 0;
}

// Utility to return corner points of a mvector of points:
void corners(mvector<ijpt> &V, ijpt &ll, ijpt &lr, ijpt &ur, ijpt &ul) {
  int i;
  int maxi, maxj, mini, minj;

  if (V.xpoints() < 3) { 
    printf("Too few points to work with %d\n",V.xpoints() );
    fflush(stdout);
    return;
  }
  maxi = max(V[0].i, V[1].i);
  maxj = max(V[0].j, V[1].j);
  mini = min(V[0].i, V[1].i);
  minj = min(V[0].j, V[1].j);

  for (i = 2; i < V.xpoints() ; i++) {
    maxi = max(maxi, V[i].i);
    maxj = max(maxj, V[i].j);
    mini = min(mini, V[i].i);
    minj = min(minj, V[i].j);
  }

  ll.i = mini; ll.j = minj;
  lr.i = maxi; lr.j = minj;
  ur.i = maxi; ur.j = maxj;
  ul.i = mini; ul.j = maxj;

  return;
}



size_t getseg(mvector<latpt> &locations, int &level, float &west, float &east,
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
    if (south == -90.0) {
      mvector<latpt> tloc(gshhsheader.n * 6 );
      int ti, tj, tk, tmpindex;
      latpt tmplat;
      float dlat = 1./32., reflon;
      printf("Have found Antarctica, need to augment the list?\n");
      // Copying over the original set
      for (ti = 0; ti < locations.xpoints() ; ti++) {
        printf("ti = %d\n",ti); 
        tloc[ti] = locations[ti];
      }
      // Run from last longitude down to the pole
      tmpindex = ti - 1;
      tmplat.lon = tloc[tmpindex].lon;
      if (tmplat.lon == 360.) {
        tmplat.lon = 360. - dlat;
      }
      reflon = tmplat.lon;
      for (tmplat.lat = locations[tmpindex].lat; 
           tmplat.lat >= -90.0 + dlat;
           tmplat.lat -= dlat   ) {
        tloc[ti] = tmplat;
        ti += 1;
      } 
      // Run along pole -- ensure that the longs are monotonically increasing
      ti -= 1; tk = 1;
      tj = 0;
      tloc[tj + ti ].lon = locations[locations.xpoints() - 1 - tj].lon;
      tloc[tj + ti ].lat = -90.0 + dlat;
      for (tj = 1; tj < locations.xpoints() ; tj++) {
        //if (tloc[tk + ti ].lon > locations[locations.xpoints() - 1 - tj].lon) {
          tloc[tk + ti ].lon = locations[locations.xpoints() - 1 - tj].lon;
          tloc[tk + ti ].lat = -90.0 + dlat;
          tk += 1;
        //}
      }
      // Run back up to the original
      tmplat.lon = tloc[tk - 1].lon;
      if (tmplat.lon == reflon) {
        if (reflon == 360. - dlat) {
          tmplat.lon = dlat;
        }
        else {
          tmplat.lon = 360. - dlat;
        }
      }
      printf("second longitude seam = %f\n",tmplat.lon);

      for (tmplat.lat = -90.0 + dlat; 
           tmplat.lat <= locations[tmpindex].lat;
           tmplat.lat += dlat   ) {      
        tloc[ti + tk] = tmplat;
        tk += 1;
      }

      locations.resize(ti+tk+1);
      for (ti = 0; ti < locations.xpoints() - 1; ti++ ){
        locations[ti] = tloc[ti];
      }
      locations[ti] = locations[0];
      for (ti = 0 ; ti < locations.xpoints(); ti++) {
        printf("AA locations %d  %f %f\n",
                ti, locations[ti].lon, locations[ti].lat);
      }
    }

  return found;
}

/////////////////////////////////
// Function to ingest a set of points on a line, and the target grid 
//    (the latter for mapping purposes only) and produce a set which
//    is trimmed to those which correspond to unique grid points on the
//    target.
void line_thin(mvector<latpt> &locations, FAMILY<unsigned char> &field) {
  int i = 0, j = 0, nx = locations.xpoints();
  fijpt ij1;
  ijpt prev, cur;
  mvector<latpt> tmp(locations.xpoints() );

// Loop forward until we find a point on the grid.
  j = 0;
  i = 0;
  ij1.i = -1;
  ij1.j = -1;
  while (ij1.i == -1. && ij1.j == -1. && i < nx - 1 ) {
    ij1 = field.locate(locations[i]);
    i += 1;
  }
  tmp[j] = locations[i];
  j += 1;
  prev = ij1;

  //for (i = 1; i < nx - 1; i++) {
  for ( ; i < nx - 1; i++) {
     ij1 = field.locate(locations[i]);
     cur = ij1;
     // note changes for -1, -1 case -- rounds to 0,0
     if (cur != prev || (ij1.i == -1 && ij1.j == -1) ) {
       tmp[j] = locations[i];
       prev.j = -1;
       prev.i = -1;
       j += 1;
     }
  }

  #ifdef VERBOSE
  printf("line_thin %d points on entry %d on exit\n",
            locations.xpoints(),j); 
  fflush(stdout);
  #endif

  locations.resize(j+1); 
  for (i = 0; i < j; i++) { 
     locations[i] = tmp[i]; 
     #ifdef VERBOSE2
       printf("%d %f %f\n",i,locations[i].lon, locations[i].lat); 
       fflush(stdout);
     #endif
  }
  locations[j] = locations[0];
  fflush(stdout);

}

void field_fill(mvector<latpt> &locations, 
                FAMILY<unsigned char> &field, 
                unsigned char boundary, unsigned char fill, 
                unsigned char unfill, unsigned char undef, int npts ) {
  int i, nx = locations.xpoints();
  int field_nx = field.xpoints(), field_ny = field.ypoints(); 
  fijpt ij1, ij2;
  ijpt loc;
  mvector<ijpt> ijlocs;
  latpt ll1, ll2;
  #ifdef VERBOSE
    palette<unsigned char> gg(19,65);
    char fname[900];
  #endif
  float lonmax = -3600., lonmin = +3600.;

  for (i = 0; i < nx ; i++) {
    lonmax = max(lonmax, locations[i].lon);
    lonmin = min(lonmin, locations[i].lon);
  }
  #ifdef VERBOSE
  printf("lon max, min = %f %f\n",lonmax, lonmin);
  fflush(stdout);
  #endif

  ijlocs.resize(nx + 1);

  loc.i = 0; loc.j = 0; ll1 = field.locate(loc);
  loc.i = 1; loc.j = 1; ll2 = field.locate(loc);

  for (i = 0; i < nx - 1; i++) {

     ij1 = field.locate(locations[i]);

     ijlocs[i].i = (int) (0.5 + ij1.i);
     ijlocs[i].j = (int) (0.5 + ij1.j);
     ij2 = field.locate(locations[i+1]);
     #ifdef VERBOSE2
         printf("%f %f to %f %f\n",locations[i].lat, locations[i].lon, 
                 ij1.i, ij1.j);
         fflush(stdout);
     #endif
     lineFill(ij1, ij2, field, boundary);

  }

  ij1 = field.locate(locations[nx - 1]);
  ijlocs[nx - 1].i = (int) (0.5 + ij1.i);
  ijlocs[nx - 1].j = (int) (0.5 + ij1.j);
  ijlocs[nx ] = ijlocs[0]; // For winding number routine
  ij2 = field.locate(locations[0]);
  lineFill(ij1, ij2, field, boundary);
  #ifdef VERBOSE
    sprintf(fname, "bfield%d.xpm",npts); 
    field.xpm(fname, 1, gg);
  #endif 

  // List off the ijlocations for diagnostic purposes
  #ifdef VERBOSE
  for (i = 0; i < ijlocs.xpoints() - 1; i++) {
    printf("ijl %4d  %d %d  %f %f\n",i, ijlocs[i].i, ijlocs[i].j, 
                         locations[i].lon, locations[i].lat);
    fflush(stdout);
  }
  #endif

//// The boundaryFill algorithm is capable of being overloaded as there
////   is finite, albeit large, space for the stack in the recursive
////   algorithm.
  if (field_nx*field_ny > 1.25e5) {
    #if defined(resops)
    printf("Too many points in grid, skipping. %d ",
        field_nx*field_ny); fflush(stdout);
    exit(1);
    #endif
    //int range = (int) (0.5 + sqrt( (field_nx*field_ny)/400.) );
    int range = 2;
    int ti, tj;
    printf("Rescaling by %d\n",range);
    for (ti = 0; ti < range; ti++) {
    for (tj = 0; tj < range; tj++) {
       printf("ti, tj = %d %d range of %d\n",ti, tj, range); fflush(stdout);
      for (loc.i = ti; loc.i < field_nx; loc.i += range) {
      for (loc.j = tj; loc.j < field_ny; loc.j += range) {
      if (field[loc] == undef) {
        ll1 = field.locate(loc);
        if (ll1.lon > lonmax && ll1.lon - 360. > lonmin) ll1.lon -= 360.;
        if (ll1.lon < lonmin && ll1.lon + 360. < lonmax) ll1.lon += 360.;
        if (ll1.lon > lonmax || ll1.lon < lonmin) {
           field[loc] = unfill;
           continue;
        }
        if (wn_PnPoly(ll1, locations, nx ) != 0 ) {
  //      Note that boundaryFill will segfault if loc is not inside the curve
          bcount = 0;
          boundaryFill(loc.i, loc.j, fill, boundary, field, range);
        }
        else {
          field[loc] = unfill;
        }
      }
    } // loc.i, loc.j
    }
    } // ti, tj
    }
// Now repeat with a step of 1 to fill in all the small patches:
      for (loc.i = 0; loc.i < field_nx; loc.i += 1) {
      for (loc.j = 0; loc.j < field_ny; loc.j += 1) {
      if (field[loc] == undef) {
        ll1 = field.locate(loc);
        if (ll1.lon > lonmax && ll1.lon - 360. > lonmin) ll1.lon -= 360.;
        if (ll1.lon < lonmin && ll1.lon + 360. < lonmax) ll1.lon += 360.;
        if (ll1.lon > lonmax || ll1.lon < lonmin) {
           field[loc] = unfill; 
           continue;
        }
        if (wn_PnPoly(ll1, locations, nx ) != 0 ) {
  //      Note that boundaryFill will segfault if loc is not inside the curve
          bcount = 0;
          boundaryFill(loc.i, loc.j, fill, boundary, field);
        }
        else {
          field[loc] = unfill;
        }
      }
    } // loc.i, loc.j
    }

  }
  else {  // Small field branch:
//// Find a point inside the polygon
  //printf("In small field branch\n"); fflush(stdout);
  for (loc.i = 0; loc.i < field_nx; loc.i++) {
  for (loc.j = 0; loc.j < field_ny; loc.j++) {
    if (field[loc] == undef) {
      ll1 = field.locate(loc);
      if (ll1.lon > lonmax && ll1.lon - 360. > lonmin) ll1.lon -= 360.;
      if (ll1.lon < lonmin && ll1.lon + 360. < lonmax) ll1.lon += 360.;
      if (ll1.lon > lonmax || ll1.lon < lonmin) {
         field[loc] = unfill;
         continue;
      }
      if (wn_PnPoly(ll1, locations, nx ) != 0 ) {
//      Note that boundaryFill will segfault if loc is not inside the curve
        bcount = 0;
        boundaryFill(loc.i, loc.j, fill, boundary, field);
      }
      else {
        field[loc] = unfill;
      }
    }
  } // loc.i, loc.j
  }

  }

  #ifdef VERBOSE
    sprintf(fname, "ffield%d.xpm",npts); 
    field.xpm(fname, 1, gg);
  #endif 
  #ifdef VERBOSE
    printf("leaving field_fill\n");
    fflush(stdout);
  #endif

  return ;
}
