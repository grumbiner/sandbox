
#include "ncepgrids.h"

// Perform a fill, given the assumption that we have a boundary set
//  and a point inside the boundary.
void boundaryFill(int x, int y, float fill, float boundary, 
        metricgrid<float> &field) ;

// Given 4 points, connect them by flagging relevant boundary points:
void lineFill(ijpt &ll, ijpt &ul, metricgrid<float> &field, float &boundary); 

#define STEPS 5.0

int main(int argc, char *argv[]) {
  TARGET<float> field;
  ijpt loc, ll, ur, lr, ul;
  latpt llat;
  float boundary = 1.0, fill = 5.0, undef = 3.0;
  palette<unsigned char> gg(19, 65);

  field.set( (float) undef);

  ll.i = 90; ll.j = 90;
  ur.i = ll.i+95; ur.j = ll.j+90;
  lr.i = ll.i+90; lr.j = ll.j+5;
  ul.i = ll.i+0;  ul.j = ll.j+90;
  lineFill(ll, lr, field, boundary);
  lineFill(lr, ur, field, boundary);
  lineFill(ur, ul, field, boundary);
  lineFill(ul, ll, field, boundary);

  printf("Returned from the last lineFill\n"); fflush(stdout);

  loc.i = 105;
  loc.j = 105;
// Note that boundaryFill will segfault if loc is not inside the curve
  boundaryFill(loc.i, loc.j, fill, boundary, field);
  field.xpm("field.xpm", 1, gg);

  return 0;
}

// Note that the first point passed in must be a point inside the 
//  domain.  Your job to figure out where the inside is.
void boundaryFill(int x, int y, float fill, float boundary, 
        metricgrid<float> &field) {
  ijpt tloc;
  float current = field[x+y*field.xpoints()];

  tloc.i = x;
  tloc.j = y;
  if (!field.in(tloc) ) return;
  
  if (current != boundary && (current != fill) ) {
    field[x+y*field.xpoints()] = fill;
    boundaryFill(x+1, y, fill, boundary, field);
    boundaryFill(x, y+1, fill, boundary, field);
    boundaryFill(x-1, y, fill, boundary, field);
    boundaryFill(x, y-1, fill, boundary, field);
  }

  return;

}

void lineFill(ijpt &ll, ijpt &lr, metricgrid<float> &field, float &boundary) { 
  fijpt floc, fdelta, tdelta, rdelta;
  ijpt tloc, delta;
  int steps = 0;
  float mag;

  delta = lr;
  delta -= ll;
  fdelta.i = delta.i;
  fdelta.j = delta.j;
  floc.i = ll.i;
  floc.j = ll.j;

  tdelta = fdelta;
  mag  = fdelta.magnitude();
  tdelta.i /= (STEPS*mag);
  tdelta.j /= (STEPS*mag);    // step size to ensure all cells get filled
  rdelta = tdelta;  // running delta
  floc += tdelta;

  field[ll] = boundary;
  while ( (rdelta.magnitude() < mag) &&  (steps < 1e5)) {
    tloc.i = (int) (0.5 + floc.i);
    tloc.j = (int) (0.5 + floc.j);
    //printf("step floc %d %f %f\n",steps, floc.i, floc.j);
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
