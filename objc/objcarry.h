#import <objc/Object.h>
#import <stdio.h>
#import <stdlib.h>
#import <time.h>

// -- @interface --------------------------------------
//@interface marray: NSObject {
@interface marray: Object {
  @public
  int nx, ny;
  float *grid;
}
- (void) print;
- (void) setsize: (int) nx : (int) ny;
//- (void) release;
- (int) npoints;
- (float) index: (int) x;
- (float) index2: (int) x : (int) y;
- (float) ip: (int) x;
- (float) im: (int) x;
- (float) jp: (int) x;
- (float) jm: (int) x;

@end


// -- @implementation of the marray interface --------
@implementation marray;
-(int) npoints {
    return (nx*ny);
}
-(float) ip: (int) x {
    return (grid[x+1]);
}
-(float) im: (int) x {
    return (grid[x-1]);
}
-(float) jp: (int) x {
    return (grid[x+nx]);
}
-(float) jm: (int) x {
    return (grid[x-nx]);
}
-(float) index: (int) x {
    return (grid[x]);
}
-(float) index2: (int) x : (int) y {
    return (grid[x+y*nx]);
}

//-(void) release {
//    free (grid);
//}
-(void) print {
    printf(" %d  %d\n",nx, ny);
}
-(void) setsize: (int) x : (int) y {
    int i;
    nx = x;
    ny = y;
    grid = (float *) malloc(sizeof(float)*nx*ny);
    for (i = 0; i < nx*ny; i++) {
        grid[i] = 0;
    }
}
@end
