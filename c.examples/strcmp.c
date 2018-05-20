#include <string.h>
#include <stdio.h>

int main(void)
{
  char a[]="thas", b[]="that";
  printf("strcmp of thas, thats is %d\n",strcmp(a,b));
  return(0);
}