#include "gshhs.h"
#include "ncepgrids.h"
#include "cofs.h"
#include "llgrid.h"
#include <stdlib.h>

// Version to work on/with regional target grids.

///////////////// Geometric routines
#include "geometry.C"

///////////// Utilities
// SBR to fill fields
void field_fill(mvector<latpt> &locations, 
                FAMILY<unsigned char> &field, 
                unsigned char boundary, unsigned char fill, 
                unsigned char unfill, unsigned char undef, int npts ) ;

// Sbr to get the next segment:
size_t getseg(mvector<latpt> &locations, int &level, float &west, float &east,
            float &north, float &south, FILE *fin);

int regionalize(TARGET<unsigned char> &field, mvector<latpt> &locations,
                float &west, float &east, float &north, float &south);

// Sbr to setablish a grid subset for a given family
void subset(TARGET<unsigned char> &tot_field, 
            FAMILY<unsigned char> &field, 
      float west, float east, float north, float south, ijpt &delta); 

// Downscale constructs a factor of 'scaling' smaller grid (larger dx, dy)
// from the input grid, preserving all 'boundary' flagged points at the
// expense of fill or undefined.
// Upscale does the reverse step.  Purpose of pair is to permit filling of
// large domain fields, as Eurasia, without overruning the ability of
// the system to permit recursive calls in boundary fill.
void downscale(FAMILY<unsigned char> &tot_field, 
            FAMILY<unsigned char> &field, int scaling);
void upscale(FAMILY<unsigned char> &tot_field, 
            FAMILY<unsigned char> &field, int scaling);

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

  ijpt loc, tloc, delta;
  fijpt fij;
  latpt ll;
  int j;
  unsigned char boundary = 1, land = 5, water = 17, ocean = 15, undef = 3;
  unsigned char fill, unfill;
  float north, south, east, west;
  int level;
  size_t found;
  //#ifdef VERBOSE
  palette<unsigned char> gg(19, 65);
  char fname[900];
  //#endif

////////////////////////////////////////
  tot_field.set( (unsigned char) undef);

  fin = fopen(argv[1], "r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open %s\n",argv[1]);
    return 1;
  }
  j = 0;

  found = (size_t) 1;
  while (!feof(fin) && found != (size_t) 0 && j < atoi(argv[4]) ) {
    #ifdef VERBOSE
      printf("j = %d found = %d\n",j,(int) found); fflush(stdout);
    #endif
    found = getseg(locations, level, west, east, north, south, fin);
    if (found == (size_t) 0) {
      #ifdef VERBOSE
        printf("found = 0, breaking out of loop\n");
      #endif
      break;
    }
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


//  For regional grids, need to look through and see which points, if 
//    any, are in the domain of the region.
//  Note that, for regional, it is entirely possible, and problematic,
//    for all the locations to come out identical.
    line_thin(locations, tot_field);
    if (regionalize(tot_field, locations, west, east, north, south) == 0 )  
      continue;

//  Construct a field grid around the bounds of the segment, return the
//    shift involved tot_field[loc+delta] = field[loc]
    subset(tot_field, field, west, east, north, south, delta); 
      
//  Initialize the field to undefined
    field.set( (unsigned char) undef);

    // If there are many points, attempt to thin the set for the sake 
    //   of execution speed.  The choice of 1000 is arbitrary.  Limiting
    //   to 1e6 means that only 2 segments from the fine resolution are
    //   affected.  Using 1 means all 188,598 are affected.  With the target
    //   being 1/2 degree global, the execution speed is 42 with 1e6, 13.1 with 1.
    //   (arbitrary units corresponding to seconds on my system today, 24 Feb 2003.)
    if (locations.xpoints() > 1) {
      line_thin(locations, field);
    }
//  Fill inside the bounding curve
//     -- this will invoke boundary fill, which is were there can be stack
//         problems.
    field_fill(locations, field,
                  boundary, fill, unfill, undef, locations.xpoints() ) ;
    #ifdef VERBOSE
      sprintf(fname, "field%d.xpm",j); 
      field.xpm(fname, 1, gg);
    #endif 

 // Now incorporate the relevant parts of field into tot_field
//  In the regional grids, cannot assume that there is simply a shift involved,
//  total revision of logic here:
    //printf("   field parms %f %f  %f %f  %d %d\n",
    //  field.firstlat, field.firstlon, field.dlat, field.dlon,
    //  field.xpoints(), field.ypoints() );
    //printf("totfield parms %f %f  %f %f  %d %d\n",
    //  tot_field.firstlat, tot_field.firstlon, tot_field.dlat, tot_field.dlon,
    //  tot_field.xpoints(), tot_field.ypoints() );
    for (tloc.j = 0; tloc.j < field.ypoints(); tloc.j++) {
    for (tloc.i = 0; tloc.i < field.xpoints(); tloc.i++) {
      ll = field.locate(tloc);
      if (ll.lon - tot_field.firstlon > 360.) {
        //printf("firstlon reset %f %f\n",ll.lon, tot_field.firstlon);
        ll.lon -= 360.;
      }
      fij = tot_field.locate(ll); 
      loc = fij;

      //if (j == 1 ) {
      //  printf("%d %d  %d %d  %f %f  %d %d\n",tloc.i, tloc.j, 
      //               loc.i, loc.j, ll.lat, ll.lon, 
      //               tot_field.in(loc), (int) field[tloc]  );
      //}

      if (tot_field.in(loc) && 
           (field[tloc] == fill || field[tloc] == boundary) ) {
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


// Sbr to establish a grid subset for a given family
void subset(TARGET<unsigned char> &tot_field, 
            FAMILY<unsigned char> &field, 
      float west, float east, float north, float south, ijpt &delta) { 
  latpt llat;
  fijpt fll, flr, fur, ful;
  ijpt ll, lr, ur, ul, pt;
  mvector<ijpt> V(4);
  int i, nx, ny;
  float dlat = tot_field.dlat;
  float dlon = tot_field.dlon;
  float augment = 2.;

  //printf("dlat dlon %f %f north south %f %f east west %f %f aug %f\n",
  //  dlat, dlon, north, south, east, west, augment);
  llat.lat = south - augment;
  llat.lon = west - augment;
  fll = tot_field.locate(llat);
  V[0] = fll;

  llat.lat = south - augment;
  llat.lon = east + augment;
  flr = tot_field.locate(llat);
  //printf("flr %f %f ",flr.i, flr.j);
  V[1] = flr;
  //printf("%d %d\n",V[1].i, V[1].j);

  llat.lat = north + augment;
  llat.lon = east + augment;
  fur = tot_field.locate(llat);
  //printf("fur %f %f ",fur.i, fur.j);
  V[2] = fur;
  //printf("%d %d\n",V[2].i, V[2].j);

  llat.lat = north + augment;
  llat.lon = west - augment;
  ful = tot_field.locate(llat);
  V[3] = ful;
  corners(V, ll, lr, ur, ul);
  //for (i = 0; i < 4; i++) {
  //  printf("i %1d  %d %d\n",i,V[i].i,V[i].j);
  //}

  nx = lr.i - ll.i + 1;
  ny = ur.j - lr.j + 1;

  if (nx > 2*tot_field.xpoints() ) {
    printf("nuts in sizing nx ");
    printf("lr.i = %d, ll.i = %d delta = %f\n",lr.i, ll.i,dlon);
    printf("nuts %f %f %f %f corners %f %f %f %f\n",fll.i, flr.i, fur.i, ful.i,
              north, south, east, west);
    exit(1) ;
  }
// Will probably want to do some bound checking in j and cyclicity in x
// Note that the following has assumed we're working with llgrids 

  pt = ll;
  delta = ll;

  llat = tot_field.locate(pt);
  //printf("subset %f %f %f %f\n",llat.lon, west, augment, tot_field.firstlon);
  if (west - augment < tot_field.firstlon) {
    llat.lon += 360.;
    printf("reset llat.lon to %f\n",llat.lon);
  } 
  #ifdef VERBOSE
    printf("location of first point %d %d = %f %f\n",
                     pt.i, pt.j, llat.lat, llat.lon);
    printf("field = %f %f  %f %f\n",north, south, east, west);
  #endif
  field.firstlat = llat.lat;
  field.firstlon = llat.lon;
  field.dlat = dlat;
  field.dlon = dlon;

  #ifdef VERBOSE
    printf("resizing the target field\n"); fflush(stdout);
  #endif
  field.resize(nx, ny);
  #ifdef VERBOSE
    printf("done resizing the target field\n"); fflush(stdout);
  #endif

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

  // We do need FLIP
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
      if (east * west == 0.) {
         printf("%d %d west, east\n",gshhsheader.west, gshhsheader.east);
         // If unmanageable, call again 
         getseg(locations, level, west, east, north, south, fin);
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

  ij1 = field.locate(locations[i]);
  tmp[j] = locations[i];
  j += 1;
  prev = ij1;

  for (i = 1; i < nx - 1; i++) {
     ij1 = field.locate(locations[i]);
     cur = ij1;
     if (cur != prev) {
       tmp[j] = locations[i];
       prev = cur;
       j += 1;
     }
  }

  #ifdef VERBOSE
    printf("line_thin %d points on entry %d on exit\n",locations.xpoints(),j); 
    fflush(stdout);
  #endif

  locations.resize(j); 
  for (i = 0; i < j; i++) { 
     locations[i] = tmp[i]; 
     //if ((locations[i].lon - field.firstlon) > 360.) {
     //  //printf("Reset %f %f\n",locations[i].lon, locations[i].lon - 360.);
     //  locations[i].lon -= 360.;
     //} 
     #ifdef VERBOSE2
       printf("list %d %f %f\n",i,locations[i].lon, locations[i].lat); 
       fflush(stdout);
     #endif
  }

}

void field_fill(mvector<latpt> &locations, 
                FAMILY<unsigned char> &field, 
                unsigned char boundary, unsigned char fill, 
                unsigned char unfill, unsigned char undef, int npts ) {
  FAMILY<unsigned char> tmp_field;
  int i, nx = locations.xpoints();
  int field_nx = field.xpoints(), field_ny = field.ypoints(); 
  fijpt ij1, ij2;
  ijpt loc;
  mvector<ijpt> ijlocs;
  latpt ll1, ll2;
    palette<unsigned char> gg(19,65);

  #ifdef VERBOSE
     printf("About to resize ijlocs\n"); fflush(stdout);
  #endif
  ijlocs.resize(locations.xpoints() + 1);
  #ifdef VERBOSE
     printf("done to resize ijlocs\n"); fflush(stdout);
  #endif

  loc.i = 0; loc.j = 0; ll1 = field.locate(loc);
  loc.i = 1; loc.j = 1; ll2 = field.locate(loc);

  for (i = 0; i < nx - 1; i++) {
     #ifdef VERBOSE2
        printf("In field_fill, i = %d\n",i); fflush(stdout);
     #endif

     ij1 = field.locate(locations[i]);

     ijlocs[i].i = (int) (0.5 + ij1.i);
     ijlocs[i].j = (int) (0.5 + ij1.j);
     ij2 = field.locate(locations[i+1]);

     lineFill(ij1, ij2, field, boundary);

  }

  ij1 = field.locate(locations[nx - 1]);
  ijlocs[nx - 1].i = (int) (0.5 + ij1.i);
  ijlocs[nx - 1].j = (int) (0.5 + ij1.j);
  ijlocs[nx ] = ijlocs[0]; // For winding number routine
  ij2 = field.locate(locations[0]);
  lineFill(ij1, ij2, field, boundary);

  // List off the ijlocations for diagnostic purposes
  #ifdef VERBOSE
  for (i = 0; i < ijlocs.xpoints() - 1; i++) {
    printf("ijl %4d  %d %d  %f %f\n",i, ijlocs[i].i, ijlocs[i].j, 
                         locations[i].lon, locations[i].lat);
  }
  #endif

//// The boundaryFill algorithm is capable of being overloaded as there
////   if finite, albeit large, space for the stack in the recursive
////   algorithm.
//// As protection, do a preliminary version of the call on smaller
////   subset if there are a large number of points
  if (field_nx*field_ny > 1.75e5) {
    printf("trying to downscale, nx ny = %d %d\n",field_nx, field_ny); 
    fflush(stdout);
    bcount = 0;
    downscale(field, tmp_field, 10);
    #ifdef VERBOSE
      field.xpm("tfield1.xpm", 1, gg);
      tmp_field.xpm("tmp_field1.xpm", 1, gg);
    #endif
    field_fill(locations, tmp_field, boundary, fill, 
                  unfill, undef, npts ) ;
    //VERBOSEtmp_field.xpm("tmp_field2.xpm", 1, gg);

    upscale(field, tmp_field, 10);
    #ifdef VERBOSE
      field.xpm("tfield3.xpm", 1, gg);
      tmp_field.xpm("tmp_field3.xpm", 1, gg);
    #endif

    for (loc.j = 0; loc.j < field_ny; loc.j++) {
    for (loc.i = 0; loc.i < field_nx; loc.i++) {
      if ( field[loc] == undef ) {
        if (wn_PnPoly(loc, ijlocs, npts) != 0) {
          boundaryFill(loc.i, loc.j, fill, boundary, field);
        }
        else {
          field[loc] = unfill;
        }
      }
    }
    }
    //VERBOSEfield.xpm("tfield4.xpm", 1, gg);

    #ifdef VERBOSE
      printf("Done with pre-emptive pass on a large field_fill\n"); 
      fflush(stdout);
    #endif
  }
  else {  // Small field branch:
//// Find a point inside the polygon
  for (loc.j = 0; loc.j < field_ny; loc.j++) {
  for (loc.i = 0; loc.i < field_nx; loc.i++) {
    if (field[loc] == undef) {
      if (wn_PnPoly(loc, ijlocs, npts) != 0 ) {
//       Note that boundaryFill will segfault if loc is not inside the curve
        bcount = 0;
        boundaryFill(loc.i, loc.j, fill, boundary, field);
      }
      else {
        field[loc] = unfill;
      }
    }
  }
  }

  }

  #ifdef VERBOSE
    printf("leaving field_fill\n");
    fflush(stdout);
  #endif

  return ;
}


////////////////////////////////////////////////////////////////
void downscale(FAMILY<unsigned char> &tot_field, FAMILY<unsigned char> &field, 
            int scaling) {
// Totfield is input, field is output
  int nx, ny;
  ijpt loc, delta, tloc;
  unsigned char boundary = 1, undef = 3;

  field.dlat = tot_field.dlat * scaling;
  field.dlon = tot_field.dlon * scaling;
  field.firstlat = tot_field.firstlat - tot_field.dlat/2. + field.dlat/2.;
  field.firstlon = tot_field.firstlon - tot_field.dlon/2. + field.dlon/2.;

  if (tot_field.xpoints() % scaling == 0 &&
      tot_field.ypoints() % scaling == 0   ) {
    field.resize(tot_field.xpoints() / scaling, tot_field.ypoints() / scaling);
  }
  else {
    // ensure that new grid has at least enough points:
    nx = (tot_field.xpoints() / scaling) + 1;
    ny = (tot_field.ypoints() / scaling) + 1;
    #ifdef VERBOSE
      printf("rescaling %d %d by %d to %d %d\n",tot_field.xpoints(), 
                  tot_field.ypoints(), scaling, nx, ny);
    #endif
    field.resize(nx, ny);
  }

  // Should now have grid available.  Loop through source grid and 
  field.set(undef);
  for (loc.j = 0; loc.j < field.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < field.xpoints(); loc.i++) {
    //VERBOSEprintf("downscaling to %d %d\n",loc.i, loc.j); fflush(stdout);
    for (delta.j = 0; delta.j < scaling; delta.j++) {
    for (delta.i = 0; delta.i < scaling; delta.i++) {
      tloc = loc;
      tloc.i *= scaling;
      tloc.j *= scaling;
      tloc += delta;
      if (tot_field.in(tloc) ) {
        if (tot_field[tloc] == boundary) {
          field[loc] = boundary;
        }
      }
    }
    }
  }
  }

  return;

}
void upscale(FAMILY<unsigned char> &tot_field, FAMILY<unsigned char> &field, 
            int scaling) {
  ijpt tloc, loc, delta;
  unsigned char boundary = 1;

  for (loc.j = 0; loc.j < field.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < field.xpoints(); loc.i++) {
    //VERBOSEprintf("upscaling to %d %d\n",loc.i, loc.j); fflush(stdout);
    for (delta.j = 0; delta.j < scaling; delta.j++) {
    for (delta.i = 0; delta.i < scaling; delta.i++) {
      tloc = loc;
      tloc.i *= scaling;
      tloc.j *= scaling;
      tloc += delta;
      if (tot_field.in(tloc) ) {
        if (tot_field[tloc] != boundary && field[loc] != boundary) {
          tot_field[tloc] = field[loc];
        }
      }
    }
    }
  }
  }

  return ;
}

int regionalize(TARGET<unsigned char> &field, mvector<latpt> &locations,
                float &west, float &east, float &north, float &south) {
  fijpt fij;
  int count = 0, i;
  bool mod = false;
  
  for (i = 0 ; i < locations.xpoints(); i++) {
    fij = field.locate(locations[i]);

    mod = false;
//  Look for out of boundary points
    if (fij.j > field.ypoints() + 1) {
      mod = true;
    }
    if (fij.j < -1) {
      mod = true;
    }
    //if (fij.i > field.xpoints() + 1) {
    //    mod = true;
    //}
    //if (fij.i < -1) {
    //    mod = true;
    //}

    if (!mod) {
      count += 1;
    }

  }
    
  printf("Count = %d of %d points\n",count, locations.xpoints() );
  if (west > east) {  
    printf("inverted - q\n"); 
    printf("boundaries: %f %f  %f %f\n",north, south, west, east);
  }
  if (count >= 0) {
    printf("boundaries: %f %f  %f %f\n",north, south, west, east);
  }

  return count;
}
