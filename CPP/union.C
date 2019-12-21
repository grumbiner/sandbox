#include <stdio.h>

class critter {
  public:
    union {int ix; float fx;} fred;
    critter();
};
critter::critter() {
  fred.ix = 5;
}
int main(void) {
  critter x;
  printf("%d \n",x.fred);
  printf("%f \n",x.fred);

  return 0;
}
