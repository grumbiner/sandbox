#include <stdio.h>
main()
{
  enum sex {other, male=50, female};
  enum sex a, b;
  
  a = other;
  printf("a is of type enum with value %d \n",a);
  b = male;
  printf("b is of type enum with value %d \n",b);
  b = 2*male;
  printf("b is of type enum with value %d \n",b);
}
