#import "arrays.h"

int ratio = 1;

void setup(marray *x) ;

int main(int argc, const char *argv[]) {
  marray *u, *v, *eta, *h, *f;
  int i, j;

  u      = [marray new]; // uses objc library for object allocation
  v      = [marray new]; 
  h      = [marray new];
  f      = [marray new];
  eta    = [marray new];

  [u    setsize: 72*ratio : 36*ratio]; // note odd method of passing argument
  [v    setsize: 72*ratio : 36*ratio]; 
  [h    setsize: 72*ratio : 36*ratio]; 
  [f    setsize: 72*ratio : 36*ratio]; 
  [eta  setsize: 72*ratio : 36*ratio]; 

  setup(eta);
  for (i = 0; i < [eta npoints]; i++) {
    if (eta->grid[i] != i) {
      printf("oops %d\n",i);
    }
  }

  clock_t before, after;
  before = clock();
  printf("before %d tics %d \n",(int) before, CLOCKS_PER_SEC / 1000 );

for (j = 0; j < 160*7; j++) {
  for (i = eta->nx ; i < [eta npoints]-eta->nx; i++) {
    f->grid[i] =  ([eta im: i] + [eta ip: i] + [eta jp: i] + [eta jm: 1] -
      4.*[eta index:i] ) ;
  }
}
  after = clock();
  printf("after %d %e\n",(int) after, (float) (after-before) / (float) CLOCKS_PER_SEC  );
  printf("floppage: %f\n", (float) (eta->nx*eta->ny*5) / (float) (after-before) * (float) CLOCKS_PER_SEC / 1.e6 * (float) j );
  printf(" j = %d\n",j);


  [u release];
  [v release];
  [f release];
  [h release];
  [eta release];

  [u free];
  [v free];
  [f free];
  [h free];
  [eta free];

  return 0;
}
void setup(marray *x) {
  int i;
  for (i = 0; i < x->nx*x->ny; i++) {
    x->grid[i] = i;
  }
  return;
}
