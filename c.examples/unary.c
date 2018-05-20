#include <stdio.h>
main()
{
  int i, j, x, y;
  i = 2;
  j = 2;
  x = ++i;
  y = j++;
  printf("i = %d, j = %d, x = %d, y = %d \n", i, j, x, y);
}

