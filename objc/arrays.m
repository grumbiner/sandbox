#import "arrays.h"

// -- @implementation of the marray interface --------
@implementation marray;
-(int) npoints {
  return (nx*ny);
}
-(float) ip: (int) x {
  if (x >= 0 && x < nx*ny-1) {
    return (grid[x+1]);
  }
}
-(float) im: (int) x {
  if (x > 0 && x <= nx*ny) {
    return (grid[x-1]);
  }
}
-(float) jp: (int) x {
  if (x > 0 && x < nx*ny - nx - 1) {
    return (grid[x+nx]);
  }
}
-(float) jm: (int) x {
  if (x >= nx && x < nx*ny ) {
    return (grid[x-nx]);
  }
}
-(float) index: (int) x {
  return (grid[x]);
}
-(float) index2: (int) x : (int) y {
  return (grid[x+y*nx]);
}

-(void) release {
  free (grid);
}
-(void) print {
  printf(" %d  %d\n",nx, ny);
}
-(void) setsize: (int) x : (int) y {
  nx = x;
  ny = y;
  grid = (float *) malloc(sizeof(float)*nx*ny);
}
@end
