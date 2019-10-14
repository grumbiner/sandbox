#include <stdio.h>

//Demonstration of using default arguments in a class operation
//Robert Grumbine 18 April 2000
 
//Class declaration
class alpha {
  public:
    int nx;
    alpha() {nx = 82;}
    void show(int n=8);
};
void alpha::show(int n) {
  printf("n is %d\n",n);
}

//Main program
int main(void) {
  alpha c;

  c.show();    //No argument supplied, default will be used
  c.show(15);  //Argument supplied, argument will be displayed

  return 0;
} 
