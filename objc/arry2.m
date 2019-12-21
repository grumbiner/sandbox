#import <objc/Object.h>
#import <stdio.h>
#import <stdlib.h>
#import <time.h>

// -- @interface --------------------------------------
@interface marray: Object {
  @public
  int nx, ny;
  float *grid;
}
- (void) print;
- (void) setsize: (int) nx : (int) ny;
- (void) release;
- (int) npoints;
- (float) index: (int) x;
- (float) index2: (int) x : (int) y;
- (float) ip: (int) x;
- (float) im: (int) x;
- (float) jp: (int) x;
- (float) jm: (int) x;

@end
// ---------------------------------------------------

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
// ---- end of specifying the implementations ---------------

void tester(marray *x);

int main(int argc, const char *argv[]) {
  marray *george, *lapl;
  int i, j;

  george = [marray new]; // uses objc library for object allocation
  lapl   = [marray new]; // uses objc library for object allocation

  [george setsize: 480 : 640]; // note odd method of passing argument
  [lapl   setsize: 480 : 640]; // note odd method of passing argument

//  [george print];

//  george->grid[9] = 17.0;
// New item:
  tester(george);
  for (i = 0; i < [george npoints]; i++) {
    if (george->grid[i] != i) {
      printf("oops %d\n",i);
    }
  }

  // laplacean:
  clock_t before, after;
  before = clock();
  printf("before %d tics %d \n",(int) before, CLOCKS_PER_SEC / 1000 );

for (j = 0; j < 160; j++) {
  for (i = george->nx ; i < [george npoints]-george->nx; i++) {
    lapl->grid[i] =  ([george im: i] + [george ip: i] + [george jp: i] + [george jm: 1] -
      4.*[george index:i] ) ;
  }
}
  after = clock();
  printf("after %d %e\n",(int) after, (float) (after-before) / (float) CLOCKS_PER_SEC  );
  printf("floppage: %f\n", george->nx*george->ny*50 / (float) (after-before) * (float) CLOCKS_PER_SEC / 1.e6 * (float) j );
  printf(" j = %d\n",j);

//
//  printf("index of 9 = %f\n", [george index: 9]);
//  printf("index of 9 0 = %f\n", [george index2: 9 : 0]);
//  printf("index of 0 9 = %f\n", [george index2: 0 : 9]);

  [lapl release];
  [lapl free];

  [george release];
  [george free];

  return 0;
}
void tester(marray *x) {
  int i;
  for (i = 0; i < x->nx*x->ny; i++) {
    x->grid[i] = i;
  }
  return;
}
