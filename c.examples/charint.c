#include <stdio.h>

int main(void)
{
  int i;
  char c;

  for (i = 0; i < 256; i++)
  {
    c = i;
    printf("i %3d c %3d\n",i, (int) c);
  }
  return 0;
}
