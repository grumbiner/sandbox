#include "gshhs.h"

#include "ncepgrids.h"


// Perform a fill, given the assumption that we have a boundary set
//  and a point inside the boundary.
void boundaryFill(int x, int y, unsigned char fill, unsigned char boundary, 
        metricgrid<unsigned char> &field) ;
// Compute the winding number (from SoftSurfer)
inline int isLeft(ijpt &p0, ijpt &p1, ijpt &p2) ;
int wn_PnPoly(ijpt &p, mvector<ijpt> &V, int n) ;


// Given 2 points, connect them by flagging relevant boundary points:
#define STEPS 5.0
void lineFill(fijpt &ll, fijpt &ul, metricgrid<unsigned char> &field, 
                        unsigned char &boundary); 

// Compute a bathymetry for the target grid
void bathyfigure(metricgrid<short int> &topo, TARGET<float> &bathy);
// SBR to fill fields
void field_fill(mvector<latpt> &locations, 
                metricgrid<unsigned char> &field, 
                unsigned char boundary, unsigned char fill, 
                unsigned char unfill, unsigned char undef, int npts ) ;

// Sbr to get the next segment:
void getseg(mvector<latpt> &locations, int &level, float &west, float &east,
            float &north, float &south, FILE *fin);

// Sbr to setablish a grid subset for a given family
void subset(TARGET<unsigned char> &tot_field, 
            FAMILY<unsigned char> &field, 
      float west, float east, float north, float south, ijpt &delta); 
// Find the corner points in grid IJ space for a set of input points
void corners(mvector<ijpt> &V, ijpt &ll, ijpt &lr, ijpt &ur, ijpt &ul) ;

///////////////////////////////////////////////////////////////////
int main(int argc, char *argv[]) {
// Etopo2 input
  llgrid<short int> topo(10800, 5400, -1./30., 1./30., 90.0, -180.0);


  FILE *fin;

  FAMILY<unsigned char> field;
  TARGET<unsigned char> tot_field;
  TARGET<float> bathy;

  mvector<latpt> locations;

  latpt llat;
  ijpt loc, tloc, delta;
  int j;
  unsigned char boundary = 1, land = 5, ocean = 17, undef = 3;
  unsigned char fill, unfill;
  palette<unsigned char> gg(19, 65);
  char fname[900];
  float north, south, east, west;
  int level;

////////////////////////////////////////
  tot_field.set( (unsigned char) undef);

  fin = fopen(argv[2], "r");
  for (j = 0; (j < 1750 && !feof(fin)) ; j++) {
    printf("j = %d\n",j); fflush(stdout);
    getseg(locations, level, west, east, north, south, fin);

//  Construct a field grid around the bounds of the segment, return the
//    shift involved tot_field[loc+delta] = field[loc]
    subset(tot_field, field, west, east, north, south, delta); 
    //VERBOSEprintf("size after return from subset %d %d\n",field.xpoints(), field.ypoints());
    //VERBOSEprintf("delta = %d %d\n",delta.i, delta.j);
    //VERBOSEfflush(stdout);
      
//  Initialize the field to undefined
    field.set( (unsigned char) undef);

//  Set the flags for filling
    if (level == 1 || level == 3 ) {
      fill = land;
      unfill = ocean;
    }
    else if (level == 2) {
      fill = ocean;
      unfill = land;
    }
    else {
      printf("level = %d\n",level); fflush(stdout);
    }

//  Fill inside the bounding curve
    
    //VERBOSEprintf("calling field.fill\n"); fflush(stdout);
    field_fill(locations, field,
                  boundary, fill, unfill, undef, locations.xpoints() ) ;
    //VERBOSEprintf("back from field.fill\n"); fflush(stdout);
    //sprintf(fname, "field%d.xpm",j); 
    //field.xpm(fname, 1, gg);
 // Now incorporate the relevant parts of field into tot_field
    for (loc.j = 0; loc.j < field.ypoints(); loc.j++ ) {
    for (loc.i = 0; loc.i < field.xpoints(); loc.i++ ) {
      if (field[loc] == fill || 
          field[loc] == boundary) {
        tloc = loc;
        tloc += delta;
        tot_field[tloc] = field[loc];
      }
    }
    } 
  } // end looping through segments

  tot_field.xpm("tot_field.xpm", 1, gg);
         


//////////////////////////////
// Get the bathymetry:
  fin = fopen(argv[1], "r");
  topo.binin(fin);
  fclose(fin);

  bathyfigure(topo, bathy);

////////////////////////////
// Loop around the points that are boundary or land (i.e., got specified
//   by the polygon) and look at the topography for those points
  for (loc.j = 0; loc.j < tot_field.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < tot_field.xpoints(); loc.i++) {
    if (tot_field[loc] == boundary) {
      llat = tot_field.locate(loc);
      printf("%7.3f %8.3f  %3d %3d  %2d %7.1f\n",llat.lat, llat.lon, loc.i, loc.j, (int) tot_field[loc], bathy[loc]);
    }
  }
  }
     
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

  llat.lat = south - augment;
  llat.lon = west - augment;
  fll = tot_field.locate(llat);
  V[0] = fll;

  llat.lat = south - augment;
  llat.lon = east + augment;
  flr = tot_field.locate(llat);
  V[1] = flr;

  llat.lat = north + augment;
  llat.lon = east + augment;
  fur = tot_field.locate(llat);
  V[2] = fur;

  llat.lat = north + augment;
  llat.lon = west - augment;
  ful = tot_field.locate(llat);
  V[3] = ful;

  corners(V, ll, lr, ur, ul);
  //VERBOSEfor (i = 0; i < 4; i++ ){
  //VERBOSE  printf("original mvectors %d %d\n",V[i].i, V[i].j);
  //VERBOSE}
  //VERBOSEprintf("corners ll %d %d\n",ll.i, ll.j);
  //VERBOSEprintf("corners lr %d %d\n",lr.i, lr.j);
  //VERBOSEprintf("corners ur %d %d\n",ur.i, ur.j);
  //VERBOSEprintf("corners ul %d %d\n",ul.i, ul.j);

  nx = lr.i - ll.i + 1;
  ny = ur.j - lr.j + 1;
// Will probably want to do some bound checking in j and cyclicity in x
// Note that the following has assumed we're working with llgrids 
  //if (dlat > 0.) {
  //  if (dlon > 0.) {
  //    pt = ll;
  //  }
  //  else {
  //    pt = lr;
  //  }
  //}
  //else {
  //  if (dlon > 0.) {
  //    pt = ul;
  //  }
  //  else {
  //    pt = ur;
  //  }
  //}
  // if dlat > 0, want southern boundary
  // if dlat < 0, want northern boundary
  // if dlon > 0  want west bndy
  // if dlon < 0  want east bndy
  pt = ll;
  llat = tot_field.locate(pt);
  //VERBOSEprintf("location of first point %d %d = %f %f\n",
  //VERBOSE                 pt.i, pt.j, llat.lat, llat.lon);
  //VERBOSEprintf("field = %f %f  %f %f\n",north, south, east, west);
  field.dlat = dlat;
  field.dlon = dlon;
  field.firstlat = llat.lat;
  field.firstlon = llat.lon;
  field.resize(nx, ny);

  delta = ll;

  return;
}

void getseg(mvector<latpt> &locations, int &level, float &west, float &east,
            float &north, float &south, FILE *fin) {
// Coastline file input
  struct POINT gshhspoint;
  struct GSHHS gshhsheader;
  int max_east = 270000000;

  int i;
  latpt llat;

  fread ( (void*) &gshhsheader, sizeof(struct GSHHS), 1, fin);
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
    
    locations.resize(gshhsheader.n);

    for (i = 0; i < gshhsheader.n; i++) {
        
      if (fread ((void *)&gshhspoint, (size_t)sizeof(struct POINT), 
                              (size_t)1,                  fin) != 1) {
        fprintf (stderr, 
             "gshhs:  Error reading for polygon %d, point %d.\n", 
             gshhsheader.id, i);
          return ;
        }
        #ifdef FLIP
          gshhspoint.x = swabi4 ((unsigned int)gshhspoint.x);
          gshhspoint.y = swabi4 ((unsigned int)gshhspoint.y);
        #endif
        llat.lon = (gshhsheader.greenwich && gshhspoint.x > max_east) ? 
           gshhspoint.x * 1.0e-6 - 360.0 : gshhspoint.x * 1.0e-6;
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
      if (east * west <= 0.) {
         printf("%d %d west, east\n",gshhsheader.west, gshhsheader.east);
         // If unmanageable, call again 
         getseg(locations, level, west, east, north, south, fin);
      }

  return ;
}

/////////////////////////////////
void field_fill(mvector<latpt> &locations, 
                metricgrid<unsigned char> &field, 
                unsigned char boundary, unsigned char fill, 
                unsigned char unfill, unsigned char undef, int npts ) {
  int i, nx = locations.xpoints();
  int field_nx = field.xpoints(), field_ny = field.ypoints(); 
  fijpt ij1, ij2;
  ijpt loc;
  mvector<ijpt> ijlocs;
  latpt ll1, ll2;

  ijlocs.resize(locations.xpoints() + 1);

  //VERBOSEprintf("field nx ny %d %d\n",field.xpoints(), field.ypoints() ); 
  loc.i = 0; loc.j = 0; ll1 = field.locate(loc);
  loc.i = 1; loc.j = 1; ll2 = field.locate(loc);
  //VERBOSEprintf("field params %f %f  %f %f\n",ll1.lat, ll1.lon, ll2.lat, ll2.lon);
  //VERBOSEfflush(stdout);

  for (i = 0; i < nx - 1; i++) {
     //VERBOSE2printf("i = %d top\n",i); fflush(stdout);
     ij1 = field.locate(locations[i]);
     //VERBOSEprintf("%f %f  %f %f\n",ij1.i, ij1.j, locations[i].lon, locations[i].lat);
     //VERBOSEfflush(stdout);
     ijlocs[i].i = (int) (0.5 + ij1.i);
     ijlocs[i].j = (int) (0.5 + ij1.j);
     ij2 = field.locate(locations[i+1]);
     //VERBOSEprintf("calling lineFill\n");
     lineFill(ij1, ij2, field, boundary);
     //VERBOSE2printf("i = %d bottom %f %f\n",i,ij1.i, ij1.j); fflush(stdout);
  }
  ij1 = field.locate(locations[nx - 1]);
  ijlocs[nx - 1].i = (int) (0.5 + ij1.i);
  ijlocs[nx - 1].j = (int) (0.5 + ij1.j);
  ijlocs[nx ] = ijlocs[0]; // For winding number routine
  ij2 = field.locate(locations[0]);
  lineFill(ij1, ij2, field, boundary);

  //VERBOSE// List off the ijlocations for diagnostic purposes
  //VERBOSEfor (i = 0; i < ijlocs.xpoints(); i++) {
  //VERBOSE  printf("%4d  %d %d\n",i, ijlocs[i].i, ijlocs[i].j);
  //VERBOSE}

//// Find a point inside the polygon
  for (loc.j = 0; loc.j < field_ny; loc.j++) {
  for (loc.i = 0; loc.i < field_nx; loc.i++) {
    if (wn_PnPoly(loc, ijlocs, npts) != 0 && field[loc] == undef) {
      //printf("%d %d is inside the polygon\n", loc.i, loc.j);
//      Note that boundaryFill will segfault if loc is not inside the curve
      boundaryFill(loc.i, loc.j, fill, boundary, field);
    }
    else {
      if (field[loc] == undef) {
        field[loc] = unfill;
      }
    }
  }
  }

  return ;
}

//////////////////////////
// Given a bathymetry, construct a target grid bathymetry
void bathyfigure(metricgrid<short int> &topo, TARGET<float> &bathy) {
  TARGET<short int> countp, countm;
  TARGET<int> sump, summ; 
  TARGET<float> tbathy;
  ijpt loc, iloc;
  fijpt ij1;
  latpt llat;
  palette<unsigned char> gg(19, 65);

// We now have a fully flagged grid in 'field'.  Take a look at the 
//   bathymetry.
  summ.set(0);
  sump.set(0);
  countm.set(0);
  countp.set(0);
  bathy.set( (float) 0.);

  //printf("topography max, min, average %d %d %d\n",topo.gridmax(), 
  //        topo.gridmin(), topo.average() ); fflush(stdout);
  for (loc.j = 0; loc.j < topo.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < topo.xpoints(); loc.i++) {
    llat = topo.locate(loc);
    ij1  = bathy.locate(llat);
    iloc.i = (int) (0.5 + ij1.i);
    iloc.j = (int) (0.5 + ij1.j);
    if (bathy.in(ij1) ) {
      if (topo[loc] > 0) {
        sump[iloc] += topo[loc];
        countp[iloc] += 1;
      }
      else if (topo[loc] < 0) {
        summ[iloc] += topo[loc];
        countm[iloc] += 1;
      }
      else {
        // Policy:
        //printf("== 0 topo at %f %f\n",llat.lat, llat.lon); fflush(stdout);
        //unnecessary: summ[loc] += topo[loc];
        countm[iloc] += 1;
      }
    }
  }
  }
  //printf("Exited the topo locations loop\n"); fflush(stdout);

  // Now that we've dropped in bucket (policy) the topography to target
  //   grid, average and look at flags
  for (loc.j = 0; loc.j < summ.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < summ.xpoints(); loc.i++) {
    if (countm[loc] == 0 && countp[loc] == 0) {
      printf("Failed to find any valid topography information at %d %d!\n",
          loc.i, loc.j); fflush(stdout);
    }
    // Policies:
    else if (countm[loc] > countp[loc] && countm[loc] > 0) {
      bathy[loc] = summ[loc] / countm[loc];
    }
    else {
      if (countp[loc] > 0) 
        bathy[loc] = sump[loc] / countp[loc];
    }
  }
  }

  tbathy = bathy;
  tbathy.scale();
  tbathy.xpm("bathy.xpm",8,gg);
 
  return;     
}


// Fill in lines, ensuring that there are enough points for the line to
//   be continuous in the ij space of the destination grid. 
void lineFill(fijpt &ll, fijpt &lr, metricgrid<unsigned char> &field, 
              unsigned char &boundary) { 
  fijpt floc, fdelta, tdelta, rdelta;
  ijpt tloc;
  int steps = 0;
  float mag;

  //VERBOSEprintf("Entered lineFill, ll, lr = %f %f  %f %f\n",ll.i, ll.j, lr.i, lr.j);
  //VERBOSEfflush(stdout);
  fdelta = lr;
  fdelta -= ll;
  floc.i = ll.i;
  floc.j = ll.j;

  tdelta = fdelta;
  mag  = fdelta.magnitude();
  tdelta.i /= (STEPS*mag);
  tdelta.j /= (STEPS*mag);    // step size to ensure all cells get filled
  rdelta = tdelta;  // running delta
  floc += tdelta;

  tloc = ll;
  field[tloc] = boundary;
  while ( (rdelta.magnitude() < mag) &&  (steps < 1e5)) {
    //VERBOSE2printf("in while, steps = %d\n",steps); fflush(stdout);
    tloc.i = (int) (0.5 + floc.i);
    tloc.j = (int) (0.5 + floc.j);
    
    field[tloc] = boundary;
    rdelta += tdelta;
    floc += tdelta;
    steps += 1;
  }
  if (steps > 0.9*1.e5 ) {
    printf("getting close to limit of steps: %d\n", steps);
    fflush(stdout);
  }    
}

/////////////////////////////////////////////////////////////////////
// boundaryFill from MIT notes 
// Note that the first point passed in must be a point inside the 
//  domain.  Your job to figure out where the inside is.
void boundaryFill(int x, int y, unsigned char fill, unsigned char boundary, 
        metricgrid<unsigned char> &field) {
  ijpt tloc;
  int nx = field.xpoints();
  float current = field[x+y*nx];
  //VERBOSEprintf("entered boundaryFill for point %d %d\n",x,y); fflush(stdout);

  tloc.i = x;
  tloc.j = y;
  if ( x < 0 || x > nx - 1 || 
       y < 0 || y > field.ypoints() - 1    ) {
    //VERBOSEprintf("Out of range point passed to boundaryFill %d %d\n",x, y); fflush(stdout);
    return;
  }
  
  if ( (current != boundary) && (current != fill) ) {
    field[tloc] = fill;
    boundaryFill(x+1, y, fill, boundary, field);
    boundaryFill(x, y+1, fill, boundary, field);
    boundaryFill(x-1, y, fill, boundary, field);
    boundaryFill(x, y-1, fill, boundary, field);
  }

  return;

}
// Winding number implementation derived from softSurfer
inline int isLeft(ijpt &p0, ijpt &p1, ijpt &p2) {
  return ( (p1.i - p0.i) * (p2.j - p0.j) -
           (p2.i - p0.i) * (p1.j - p0.j) );
}
int wn_PnPoly(ijpt &p, mvector<ijpt> &V, int n) {
  int wn = 0; 
  // loop through all edges of the polygon
  for (int i = 0; i < n; i++) {
    if (V[i].j <= p.j) {
      if (V[i+1].j > p.j)
        if (isLeft( V[i], V[i+1],p) > 0) 
           ++wn;
    }
    else {
      if (V[i+1].j <= p.j)
        if (isLeft(V[i], V[i+1], p) < 0) 
          --wn;
    }
   }
   return wn;
}
