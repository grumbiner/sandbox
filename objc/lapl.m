#import "arrays.h"

void setup(marray *x);

int main(int argc, const char *argv[]) {
  marray *george, *lapl;
  int i, j;

  george = [marray new]; // uses objc library for object allocation
  lapl   = [marray new]; // uses objc library for object allocation

  [george setsize: 720 : 360]; // note odd method of passing argument
  [lapl   setsize: 720 : 360]; // note odd method of passing argument

  setup(george);
  for (i = 0; i < [george npoints]; i++) {
    if (george->grid[i] != i) {
      printf("oops %d\n",i);
    }
  }

  // laplacean:
  clock_t before, after;
  before = clock();
  printf("before %d tics %d \n",(int) before, CLOCKS_PER_SEC / 1000 );

for (j = 0; j < 160*7; j++) {
  for (i = george->nx ; i < [george npoints]-george->nx; i++) {
    lapl->grid[i] =  ([george im: i] + [george ip: i] + [george jp: i] + [george jm: 1] -
      4.*[george index:i] ) ;
  }
}
  after = clock();
  printf("after %d %e\n",(int) after, (float) (after-before) / (float) CLOCKS_PER_SEC  );
  printf("floppage: %f\n", george->nx*george->ny*50 / (float) (after-before) * (float) CLOCKS_PER_SEC / 1.e6 * (float) j );
  printf(" j = %d\n",j);


  [lapl release];
  [lapl free];

  [george release];
  [george free];

  return 0;
}
void setup(marray *x) {
  int i;
  for (i = 0; i < x->nx*x->ny; i++) {
    x->grid[i] = i;
  }
  return;
}
