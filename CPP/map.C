#include <stdio.h>
#include <string>
#include <map>
using namespace std;

// Demonstration of using the C++ 'map' structure.  
// This permits construction of associative arrays.

// Robert Grumbine 18 April 2000

int main(void) {
  map<string, float> math_numbers;

  math_numbers[ string("pi") ] = 3.141592654;
  math_numbers[ string("e") ] = 2.7182818;

  printf("%f pi?\n",math_numbers[ "pi" ] );

//Note that requesting a non-existent value does not cause a crash!
  printf("%f george\n",math_numbers["george"] );

  return 0;
}
