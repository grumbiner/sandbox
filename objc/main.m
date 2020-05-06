//#import <Foundation/Foundation.h>
#import "objcarry.h"

#define NX 360
#define NY 180

void setup(marray *x) ;

int main(int argc, const char * argv[]) {
    //@autoreleasepool {
        // Begin RG code
        marray *u, *v, *eta, *h, *f;
        int i, j;
        
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
        
        setup(eta);
        printf("npoints = %d\n",[eta npoints]);
        
        for (i = 0; i < [eta npoints]; i++) {
            printf("%d  %f\n",i,[eta index: i]);
            if ([eta index: i] !=  (float) i) {
                printf("oops %d\n",i);
            }
        }
        ////////////////////////////////
        clock_t before, after;
        before = clock();
        printf("before %d tics %d \n",(int) before, CLOCKS_PER_SEC / 1000 );
        
        for (j = 0; j < 40*100; j++) {
            for (i = eta->nx ; i < [eta npoints]- eta->nx - 2; i++) {
                f->grid[i] =  ([eta im: i] + [eta ip: i] + [eta jp: i] + [eta jm: 1] -
                               4.*[eta index:i] ) ;
            }
        }
        after = clock();
        printf("after %d %e\n",(int) after, (float) (after-before) / (float) CLOCKS_PER_SEC  );
        printf("floppage: %f\n", (float) (eta->nx*eta->ny*5) / (float) (after-before) * (float) CLOCKS_PER_SEC / 1.e6 * (float) j );
        printf(" j = %d\n",j);

    //}
    return 0;
}
void setup(marray *x) {
    int i;
    for (i = 0; i < x->nx*x->ny; i++) {
        x->grid[i] = i;
    }
    return;
}
