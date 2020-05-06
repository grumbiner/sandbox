#import <objc/Object.h>
#import <stdio.h>
#import <stdlib.h>

// -- @interface --------------------------------------
@interface marray: Object {
  @public
  int nx, ny;
  float *grid;
}
- (void) print;
- (void) setsize: (int) nx : (int) ny;
- (void) release;
- (float) index: (int) x;
- (float) index2: (int) x : (int) y;
@end
// ---------------------------------------------------

// -- @implementation of the marray interface --------
@implementation marray;
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
// ---- end of specifying the implementations ---------------

int main(int argc, const char *argv[]) {
  marray *george;

  george = [marray new]; // uses objc library for object allocation

  [george setsize: 480 : 640]; // note odd method of passing argument
  [george print];

  george->grid[9] = 17.0;
  printf("index of 9 = %f\n", [george index: 9]);
  printf("index of 9 0 = %f\n", [george index2: 9 : 0]);

  [george release];
  [george free];

  return 0;
}
