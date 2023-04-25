//
//  main.m
//  rgempty
//
//  Created by Robert Grumbine on 9/27/14.
//  Copyright (c) 2014 ___FULLUSERNAME___. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <Foundation/Foundation.h>
#import "AppDelegate.h"

#import <stdlib.h>
#import <time.h>
// -- @interface --------------------------------------
@interface marray: NSObject {
@public
    int nx, ny;
    float *grid;
}
- (void) setsize: (int) nx : (int) ny;
- (int) npoints;
- (float) index: (int) x;
@end
//----------------------------------

// -- @implementation of the marray interface --------
@implementation marray;
-(int) npoints {
    return (nx*ny);
}
-(float) index: (int) x {
    return (grid[x]);
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

void setup(marray *x) ;
int shallow(int argc);
//----------------------------------


//#import "arrays.h"
#define NX 360
#define NY 180

int main(int argc, char * argv[]) {
    int alpha;
    
    alpha = 5;
    @autoreleasepool {
        shallow(alpha);
        return UIApplicationMain(argc, argv, nil, NSStringFromClass([AppDelegate class]));
    }
}

int shallow(int argc) {
    marray *u, *v, *eta, *h, *f;
    int i, j, nx, npts;
    UIView* square = [[UIView alloc] initWithFrame:
                      CGRectMake(100, 100, 100, 100)];
    square.backgroundColor = [UIColor grayColor];
//    [self.view addSubview:square];
    
    
    u      = [marray new]; // uses objc library for object allocation
    v      = [marray new];
    h      = [marray new];
    f      = [marray new];
    eta    = [marray new];

   [u    setsize: NX : NY ]; // note odd method of passing argument
   [v    setsize: NX : NY ];
   [h    setsize: NX : NY ];
   [f    setsize: NX : NY ];
   [eta  setsize: NX : NY ];
    
    nx = f->nx;
    npts = f->nx*f->ny;
    setup(eta);
    
    NSLog(@"log note 1, npoints = %@\n",@([eta npoints]));
    clock_t before, after;
    before = clock();
    NSLog(@"before %@ tics %@ \n",@(before) , @(CLOCKS_PER_SEC / 1000) );
    
    for (j = 0; j < 12*100; j++) {
        for (i = nx ; i < npts - nx - 2; i++) {
            f->grid[i] =  (
                eta->grid[i-1]+eta->grid[i+1] + eta->grid[i+nx] + eta->grid[i-nx]
                           + 4.*eta->grid[i]
            ) ;
        }
    }
    after = clock();
    NSLog(@"after %@ %@\n",@((int) after), @((float) (after-before) / (float) CLOCKS_PER_SEC)  );
    NSLog(@"floppage: %@\n", @((float) (eta->nx*eta->ny*5) / (float) (after-before) * (float) CLOCKS_PER_SEC / 1.e6 * (float) j) );
    NSLog(@" j = %@\n",@(j));
    
    
  return 0;
}

void setup(marray *x) {
    int i;
    for (i = 0; i < x->nx*x->ny; i++) {
        x->grid[i] = i;
    }
    return;
}