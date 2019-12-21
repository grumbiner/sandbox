#import <objc/Object.h>
#import <stdio.h>

// -- @interface ---
@interface Frac: Object {
  int num;
  int denom;
}
- (void) print;
- (void) setNum: (int) n;
- (void) setDenom: (int) d;
@end
// --------------------------------------------

// -- @implementation of the Frac interface ---
@implementation Frac;

-(void) print {
  printf(" %d / %d\n",num, denom);
}
-(void) setNum: (int) n {
  num = n;
}
-(void) setDenom: (int) d {
  denom = d;
}
@end
// ---- end of specifying the implementations

int main(int argc, const char *argv[]) {
  Frac *george;

  george = [Frac new]; // uses objc library for object allocation

  [george setNum: 5]; // note odd method of passing argument
  [george setDenom: 75]; 

  [george print];

  [george free];

  return 0;
}
