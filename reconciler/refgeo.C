//////////////////#include "gshhs.h"
#include "ncepgrids.h"

extern int bcount;

// Version to work on/with regional target grids.

///////////////// Geometric routines
// Perform a fill, given the assumption that we have a boundary set
//  and a point inside the boundary.
void boundaryFill(int x, int y, unsigned char fill, unsigned char boundary, 
        metricgrid<unsigned char> &field) ;
void boundaryFill(int x, int y, unsigned char fill, unsigned char boundary,
        metricgrid<unsigned char> &field, int range) ;
void blockfill(metricgrid<unsigned char> &field, ijpt &tloc, int range, 
               unsigned char fill);


// Compute the winding number (from SoftSurfer)
inline int isLeft(const ijpt &p0, const ijpt &p1, const ijpt &p2) ;
inline int isLeft(const latpt &p0, const latpt &p1, const latpt &p2) ;
int wn_PnPoly(ijpt &p, mvector<ijpt> &V, int n) ;
int wn_PnPoly(latpt &p, mvector<latpt> &V, int n) ;

///////////// Utilities
// Given 2 points, connect them by flagging relevant boundary points:
#define STEPS 15.0
void lineFill(fijpt &ll, fijpt &ul, metricgrid<unsigned char> &field, 
                        const unsigned char boundary); 

// Fill in lines, ensuring that there are enough points for the line to
//   be continuous in the ij space of the destination grid. 
void lineFill(fijpt &ll, fijpt &lr, metricgrid<unsigned char> &field, 
              const unsigned char boundary) { 
  fijpt floc, fdelta, tdelta, rdelta;
  ijpt tloc;
  int steps = 0, steplim = (int)1e5;
  float mag;

  fdelta = lr;
  fdelta -= ll;
  mag  = fdelta.magnitude();

  tdelta = fdelta;

  tdelta.i /= (STEPS*mag);
  tdelta.j /= (STEPS*mag);    // step size to ensure all cells get filled
  rdelta = tdelta;  // running delta

  floc.i = ll.i;
  floc.j = ll.j;
  floc += tdelta;

  tloc = ll;
  if (field.in(tloc)) {
     field[tloc] = boundary;
  }

  while ( (rdelta.magnitude() < mag) &&  (steps < steplim)) {
    tloc.i = (int) (0.5 + floc.i);
    tloc.j = (int) (0.5 + floc.j);
    
    if (field.in(tloc)){
       field[tloc] = boundary;
    }
    rdelta += tdelta;
    floc += tdelta;
    steps += 1;
  }
  if (steps > 0.9*steplim ) {
    printf("getting close to limit of steps: %d\n", steps);
    fflush(stdout);
  }    
}

/////////////////////////////////////////////////////////////////////
// boundaryFill from MIT notes 
// Note that the first point passed in must be a point inside the 
//  domain.  Your job to figure out where the inside is.
void boundaryFill(int x, int y, unsigned char fill, unsigned char boundary,
        metricgrid<unsigned char> &field, int range) {
  ijpt tloc;
  int nx = field.xpoints();
  float current = field[x+y*nx];
  int del;

  bcount += 1;
  if (bcount > 1.2e5) {
     printf("are probably about to overload the stack with boundary fill,\
             stopping here with bcount = 1 million\n");
     return;
  }

  #ifdef VERBOSE2
    printf("%d %d \n",x, y); fflush(stdout);
    printf("Entered boundaryFill_r\n");
    fflush(stdout);
  #endif
  tloc.i = x;
  tloc.j = y;
  if ( x < 0 || x > nx - 1 ||
       y < 0 || y > field.ypoints() - 1    ) {
    #ifdef VERBOSE2
      printf("boundaryFill_r given out of range point %d %d\n",x, y);
    #endif
    return;
  }

  del = range;
  if ( (current != boundary) && (current != fill) ) {
    if (field.anyof(boundary, range, tloc) == 0) {
      blockfill(field, tloc, range, fill);
      del = range;
    }
    else {
      field[tloc] = fill;
      del = 1;
    }
    boundaryFill(x+del, y, fill, boundary, field, range);
    boundaryFill(x, y+del, fill, boundary, field, range);
    boundaryFill(x-del, y, fill, boundary, field, range);
    boundaryFill(x, y-del, fill, boundary, field, range);
  }

  #ifdef VERBOSE2
    printf("leaving boundaryFill_r %d %d\n",x,y);
    fflush(stdout);
  #endif
  return;

}

void boundaryFill(int x, int y, unsigned char fill, unsigned char boundary, 
        metricgrid<unsigned char> &field) {
  ijpt tloc;
  int nx = field.xpoints();
  float current = field[x+y*nx];

  bcount += 1;
  #ifdef VERBOSE2
    printf("%d %d\n",x, y); fflush(stdout);
    printf("Entered boundaryFill\n"); fflush(stdout);
    if (bcount >= 100000) {
      printf("bcount = %d\n",bcount); fflush(stdout);
    }
  #endif
  tloc.i = x;
  tloc.j = y;
  if ( x < 0 || x > nx - 1 || 
       y < 0 || y > field.ypoints() - 1    ) {
    return;
  }
  
  if ( (current != boundary) && (current != fill) ) {
    field[tloc] = fill;
    boundaryFill(x+1, y, fill, boundary, field);
    boundaryFill(x, y+1, fill, boundary, field);
    boundaryFill(x-1, y, fill, boundary, field);
    boundaryFill(x, y-1, fill, boundary, field);
  }

  #ifdef VERBOSE2
    printf("leaving boundaryFill\n");
    fflush(stdout);
  #endif
  return;

}

// Winding number implementation derived from softSurfer
// Note that p2 is the point being tested
inline int isLeft(const latpt &p0, const latpt &p1, const latpt &p2) {
  float tmp = ( (p1.lon - p0.lon) * (p2.lat - p0.lat) -
                (p2.lon - p0.lon) * (p1.lat - p0.lat) );
  #ifdef VERBOSE
  if (fabs((double)tmp) > 700 || fabs((double)p1.lon - p0.lon) > 8 ) {
    fprintf(stderr,"isl ll %f  %f %f %f  %f %f\n",
             tmp, p0.lon, p1.lon, p2.lon, p1.lon - p0.lon, p2.lon-p0.lon);
  }
  #endif
  return (int) tmp;
}
inline int isLeft(const ijpt &p0, const ijpt &p1, const ijpt &p2) {
  int tmp = ( (p1.i - p0.i) * (p2.j - p0.j) -
           (p2.i - p0.i) * (p1.j - p0.j) );
  #ifdef VERBOSE
  if (fabs((double) tmp) > 7000 || fabs((double) p1.i - p0.i) > 12) {
    fprintf(stderr,"isl %d  %d %d %d  %d\n",
             tmp, p0.i, p1.i, p2.i, p1.i - p0.i);
  }
  #endif
  return tmp;
}

int wn_PnPoly(ijpt &p, mvector<ijpt> &V, int n) {
  int wn = 0; 
  // loop through all edges of the polygon
  for (int i = 0; i < n-1; i++) {
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
// Variant for lat points.  Could actually be abstracted to T
int wn_PnPoly(latpt &p, mvector<latpt> &V, int n) {
  int wn = 0; 

  if (V[0].lon != V[n-1].lon || V[0].lat != V[n-1].lat) {
    printf("error on input to wn_P %d  %f %f  %f %f\n",n, 
            V[0].lon, V[n-1].lon, V[0].lat, V[n-1].lat);
    return 5;
  }

  // loop through all edges of the polygon
  // n-1 limit since will reference point i+1
  for (int i = 0; i < n-1; i++) {
    if (V[i].lat <= p.lat) {
      if (V[i+1].lat > p.lat)
        if (isLeft( V[i], V[i+1],p) > 0) 
           ++wn;
    }
    else {
      if (V[i+1].lat <= p.lat)
        if (isLeft(V[i], V[i+1], p) < 0) 
          --wn;
    }
   }
   return wn;
}
// Fill in a block of points -- utility to the geometric routine boundaryFill
void blockfill(metricgrid<unsigned char> &field, ijpt &tloc, int range, 
               unsigned char fill) {
  ijpt loc;

  #ifdef VERBOSE
    printf("Entered blockfill, bcount = %d\n",bcount); fflush(stdout);
  #endif

  for (loc.j = max(0,tloc.j - range);
       loc.j < min(field.ypoints()-1, tloc.j + range); loc.j++) {
  for (loc.i = max(0,tloc.i - range);
       loc.i < min(field.xpoints()-1, tloc.i + range); loc.i++) {
     field[loc] = fill;
  }
  }
  return;
}

