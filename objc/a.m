#import <objc/Object.h>
#import <stdio.h>

@interface Fred : Object {
  int n;
}
-(void) show;
@end

@implementation Fred ;
-(void) show {
  n = 5;
  printf("n = %d\n",n);
}
@end


int main(void) {
  Fred *alpha;
  alpha = [Fred new];
  [alpha show];
  return 0;
}
