//////////////////#include "gshhs.h"
#include "ncepgrids.h"

#define BCOUNT_LIM 1.5e5
// 19 August 2003  Robert Grumbine

///////////////// Geometric routines
// Perform a fill, given the assumption that we have a boundary set
//  and a point inside the boundary.
void boundaryFill(const int x, const int y, const unsigned char fill, 
     const unsigned char boundary, grid2<unsigned char> &field) ;
void boundaryFill(const int x, const int y, const unsigned char fill, 
     const unsigned char boundary, const unsigned char undef, 
     grid2<unsigned char> &field) ;
void boundaryFill(const int x, const int y, const unsigned char fill, 
     const unsigned char boundary, grid2<unsigned char> &field, int range) ;
void blockfill(grid2<unsigned char> &field, ijpt &tloc, const int range, 
               const unsigned char fill);


// Compute the winding number (from SoftSurfer)
inline int isLeft(const ijpt &p0, const ijpt &p1, const ijpt &p2) ;
inline int isLeft(const latpt &p0, const latpt &p1, const latpt &p2) ;
int wn_PnPoly(ijpt &p, mvector<ijpt> &V, const int n) ;
int wn_PnPoly(latpt &p, mvector<latpt> &V, const int n) ;

///////////// Utilities
extern int bcount;

// Given 2 points, connect them by flagging relevant boundary points:
void lineFill(fijpt &ll, fijpt &ul, metricgrid<unsigned char> &field, 
                        const unsigned char boundary); 

// Fill in lines, ensuring that there are enough points for the line to
//   be continuous in the ij space of the destination grid. 
void lineFill(fijpt &ll, fijpt &lr, metricgrid<unsigned char> &field, 
              const unsigned char boundary) { 
  fijpt floc, fdelta, tdelta, rdelta;
  ijpt tloc;
  int steps = 0, steplim = (int)1e6;
  float mag;
  bool opposite = false;
  int nsteps; 

  if ( (ll.i == -1 && ll.j == -1) || (lr.i == -1 && lr.j == -1) ) {
    //printf("off grid point, returning\n"); fflush(stdout);
    return;
  }

  //float seam_width = 15.;
  float seam_width = (float)(field.xpoints() - 1.) /30.;
  //printf("seam_width = %f\n",seam_width);
  if ( (ll.i < seam_width && lr.i > field.xpoints()-1. - seam_width) ||
       (lr.i < seam_width && ll.i > field.xpoints()-1. - seam_width) ) {
    #ifdef VERBOSE
    #endif
      printf("seam  %f %f  %f %f\n", ll.i, ll.j, lr.i, lr.j);
    opposite = true;
  }

  fdelta = lr;
  fdelta -= ll;
  mag  = fdelta.magnitude();
  nsteps = 25;  // ensure that we have at least one step per grid point 
  #ifdef VERBOSE
  if (mag > seam_width / 2.) {
      printf("mag %f  %f %f  %f %f\n", mag, ll.i, ll.j, lr.i, lr.j);
      fflush(stdout);
  }
  #endif

  if (! opposite) {
    tdelta = fdelta;

    tdelta.i /= (nsteps*mag);
    tdelta.j /= (nsteps*mag);    // step size to ensure all cells get filled
    rdelta = tdelta;  // running delta
  
    floc.i = ll.i;
    floc.j = ll.j;

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
  } // end of if(!opposite)
  else { // then we are on opposite sides
    float di, dj;
    fijpt middle, fdelta;
    printf("Entered branch for opposite sides\n"); fflush(stdout);
    if (field.iscyclicx() ) {
      if (ll.i < 0 ) {
        di = (0. - ll.i) + fabs(field.xpoints() - 1. - lr.i) ;
        middle.i = 0.;
      }
      else if (ll.i < field.xpoints() / 2) {
        di = (ll.i - 0.) +  fabs(field.xpoints() - 1. - lr.i) ;
        middle.i = 0.;
      }
      else if (ll.i > field.xpoints() / 2. && ll.i < field.xpoints() - 1. ) {
        di = (field.xpoints() - 1. - ll.i) + fabs(lr.i - 0.);
        middle.i = field.xpoints() - 1.;
      }
      else if (ll.i > field.xpoints() - 1.) {
        di = (ll.i - (field.xpoints() - 1.)) + fabs(lr.i - 0.);
        middle.i = field.xpoints() - 1.;
      }
        else {
        printf("unmanaged case in filling points\n"); fflush(stdout);
        exit(1);
      }
      dj = lr.j - ll.j;
      middle.j = ll.j + fabs(middle.i - ll.i)*dj/di;
      printf("di, dj = %f %f\n",di, dj);
      printf("ll  %9.5f %9.5f ",ll.i, ll.j);
      printf("middle  %9.5f %9.5f ",middle.i, middle.j);
      printf("lr  %9.5f %9.5f\n",lr.i, lr.j);
      fflush(stdout);
      fdelta.i = di/nsteps; 
      fdelta.j = dj/nsteps;
      if (middle.i > ll.i) {
        floc = ll;
        while (floc.i < middle.i) {
          tloc = floc;
          printf("tloc = %d %d\n",tloc.i, tloc.j); fflush(stdout);
          field[tloc] = boundary;
          floc += fdelta;
        }
      }
      else {
        floc = middle;
        while (floc.i < ll.i) {
          tloc = floc;
          printf("tloc = %d %d\n",tloc.i, tloc.j); fflush(stdout);
          field[tloc] = boundary;
          floc += fdelta;
        }
      }
//    Flop middle to the other side of the grid
      if (middle.i == 0.) {
        middle.i = field.xpoints() - 1.;
      }
      else {
        middle.i = 0.;
      }
//    Now close off the lr portion:
      if (middle.i > lr.i) {
        floc = lr;
        while (floc.i < middle.i) {
          tloc = floc;
          printf("tloc = %d %d\n",tloc.i, tloc.j); fflush(stdout);
          field[tloc] = boundary;
          floc += fdelta;
        }
      }
      else {
        floc = middle;
        while (floc.i < lr.i) {
          tloc = floc;
          printf("tloc = %d %d\n",tloc.i, tloc.j); fflush(stdout);
          field[tloc] = boundary;
          floc += fdelta;
        }
      }

     
    }

  } // end of else clause

}

//////////////////////////////////////////////////////////////////
// Place these outside the function so as to avoid false stack
//  overflows.
  char fname[900];
  palette<unsigned char> gg(19,65);
  int freq = 1000;
/////////////////////////////////////////////////////////////////////
// boundaryFill from MIT notes 
// Note that the first point passed in must be a point inside the 
//  domain.  Your job to figure out where the inside is.

void boundaryFill(const int x, const int y, const unsigned char fill, 
       const unsigned char boundary, grid2<unsigned char> &field, int range) {
  ijpt tloc;
  int nx = field.xpoints();
  float current;
  int del;

  #ifdef VERBOSE
  if ( (bcount % 1000) == 0 && bcount > 41000 ) {
    sprintf(fname,"tmp%d.xpm",bcount/freq);
    field.xpm(fname, 1, gg);
  }
  if (bcount > 41000) printf("bcount = %d\n",bcount); fflush(stdout);
  #endif

  current = field[x+y*nx];
  bcount += 1;
  if (bcount > BCOUNT_LIM) {
     //printf("are probably about to overload the stack with boundary fill,
     //        stopping here with bcount = BCOUNT_LIM\n");
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
      //del = 1;
    }
    //boundaryFill(x, y+del, fill, boundary, field, range);
    //boundaryFill(x+del, y, fill, boundary, field, range);
    //boundaryFill(x, y-del, fill, boundary, field, range);
    //boundaryFill(x-del, y, fill, boundary, field, range);
  }

  #ifdef VERBOSE2
    printf("leaving boundaryFill_r %d %d\n",x,y);
    fflush(stdout);
  #endif
  return;

}


void boundaryFill(const int x, const int y, const unsigned char fill, 
                  const unsigned char boundary, grid2<unsigned char> &field) {

  bcount += 1;
  //printf("bcount = %d\n",bcount); fflush(stdout);

  #ifdef VERBOSE2
  if ( (bcount % 1000) == 0 && bcount > 41000 ) {
    sprintf(fname,"tmp%d.xpm",bcount/freq);
    field.xpm(fname, 1, gg);
  }
  if (bcount > 41000) printf("bcount = %d\n",bcount); fflush(stdout);
  #endif

  #ifdef VERBOSE2
    printf("Entered boundaryFill %d %d\n",x, y); fflush(stdout);
    if (bcount >= BCOUNT_LIM/2) {
      printf("bcount = %d\n",bcount); fflush(stdout);
    }
  #endif
  if (bcount > BCOUNT_LIM) {
    //printf("beta are probably about to overload the stack with boundary fill,
    //       stopping here with bcount = BCOUNT_LIM\n");
    return;
  } // trial on 9 Jul 2007 -- some limit to avoid overrunning

  if ( x < 0 || x > field.xpoints() - 1 || 
       y < 0 || y > field.ypoints() - 1    ) {
    return;
  }
  
  if ( (field[x+y* field.xpoints()] != boundary) && 
       (field[x+y* field.xpoints()] != fill)        ) {
    field[x+y* field.xpoints()] = fill;
    #ifdef VERBOSE2
      printf("filled %d %d\n",x, y); fflush(stdout);
    #endif
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

void boundaryFill(const int x, const int y, const unsigned char fill,
                  const unsigned char boundary, const unsigned char undef,
                  grid2<unsigned char> &field) {

  bcount += 1;

  if (bcount > BCOUNT_LIM) {
    //printf("beta are probably about to overload the stack with boundary fill,
    //       stopping here with bcount = BCOUNT_LIM\n");
    return;
  } // trial on 9 Jul 2007 -- some limit to avoid overrunning

  if ( x < 0 || x > field.xpoints() - 1 ||
       y < 0 || y > field.ypoints() - 1    ) {
    return;
  }
 
  if (field[x+y* field.xpoints()] == undef) {
    field[x+y* field.xpoints()] = fill;
    boundaryFill(x+1, y, fill, boundary, undef, field);
    boundaryFill(x, y+1, fill, boundary, undef, field);
    boundaryFill(x-1, y, fill, boundary, undef, field);
    boundaryFill(x, y-1, fill, boundary, undef, field);
  }

  return;

}

// Winding number implementation derived from softSurfer
// Note that p2 is the point being tested

// Copyright 2001, softSurfer (www.softsurfer.com)
// This code may be freely used and modified for any purpose
// providing that this copyright notice is included with it.
// SoftSurfer makes no warranty for this code, and cannot be held
// liable for any real or imagined damage resulting from its use.
// Users of this code must verify correctness for their application.


inline int isLeft(const latpt &p0, const latpt &p1, const latpt &p2) {
  float tmp = ( (p1.lon - p0.lon) * (p2.lat - p0.lat) -
                (p2.lon - p0.lon) * (p1.lat - p0.lat) );

  if (tmp > 0.) {
    return 1;
  }
  else if (tmp < 0.) {
    return -1;
  }
  else {
    return 0;
  }
  //return (int) tmp;
}
inline int isLeft(const ijpt &p0, const ijpt &p1, const ijpt &p2) {
  int tmp = ( (p1.i - p0.i) * (p2.j - p0.j) -
              (p2.i - p0.i) * (p1.j - p0.j) );
  return tmp;
}

int wn_PnPoly(ijpt &p, mvector<ijpt> &V, const int n) {
  int wn = 0; 

  if (V[0].i != V[n-1].i || V[0].j != V[n-1].j) {
    printf("error on input to wn_P %d  %d %d  %d %d\n",n,
            V[0].i, V[n-1].i, V[0].j, V[n-1].j);
    return 5;
  }

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
int wn_PnPoly(latpt &p, mvector<latpt> &V, const int n) {
  int wn , i; 
  int nx;
  
  i = 0;
  wn = 0;
  nx = V.xpoints();
  #ifdef VERBOSE
  if (V[0].lon != V[nx-1].lon || V[0].lat != V[nx-1].lat) {
    printf("error on input to wn_P %d  %f %f  %f %f\n",nx, 
            V[0].lon, V[nx-1].lon, V[0].lat, V[nx-1].lat);
    return 5;
  }
  #endif

  // loop through all edges of the polygon
  // n-1 limit since will reference point i+1
  for (i = 0; i < nx-1; i++) {
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
void blockfill(grid2<unsigned char> &field, ijpt &tloc, const int range, 
               const unsigned char fill) {
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

