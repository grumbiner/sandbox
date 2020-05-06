//
//  main.m
//  arry
//
//  Created by Robert Grumbine on 11/4/14.
//  Copyright (c) 2014 Robert Grumbine. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "arrays.h"

int ratio = 10;

#define NX 360
#define NY 180

void setup(marray *x) ;

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // insert code here...
        //NSLog(@"Hello, World!");
        // Begin RG code
        marray *eta, *f;
        int i, j, nx;
        
        f      = [marray new];
        eta    = [marray new];
        
        [f    setsize: NX : NY ];
        [eta  setsize: NX : NY ];
        
        setup(eta);
        printf("npoints = %d\n",[eta npoints]);
        nx = eta->nx;
        
        for (i = 0; i < [eta npoints]; i++) {
            //printf("%d  %f\n",i,[eta index: i]);
            if ([eta index: i] !=  (float) i) {
                printf("oops %d\n",i);
            }
        }
        ////////////////////////////////
        clock_t before, after;
        before = clock();
        printf("before %d tics %d \n",(int) before, CLOCKS_PER_SEC / 1000 );
        
        for (j = 0; j < 40*120; j++) {
            for (i = nx ; i < [eta npoints]- nx - 2; i++) {
                f->grid[i] =
                //eta->grid[i-1]+eta->grid[i+1]+eta->grid[i+nx]+eta->grid[i-nx]-4*eta->grid[i];
                ([eta index: (i-1)] + [eta index: (i+1)] +
                 [eta index: i+nx]  + [eta index: i-nx ] -
                               4.*[eta index:i] ) ;
            }
        }
        after = clock();
        printf("after %d %e\n",(int) after, (float) (after-before) / (float) CLOCKS_PER_SEC  );
        printf("floppage: %f\n", (float) (eta->nx*eta->ny*5) / (float) (after-before) * (float) CLOCKS_PER_SEC / 1.e6 * (float) j );
        printf(" j = %d\n",j);

    }
    return 0;
}
void setup(marray *x) {
    int i;
    for (i = 0; i < x->nx*x->ny; i++) {
        x->grid[i] = i;
    }
    return;
}
