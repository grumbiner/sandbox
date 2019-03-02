#include <stdio.h>

#define NX 512
#define NY 512

int main (int argc, char *argv[])
{

  FILE *in, *out;
  char map[NY][NX];
  int i, j;


  in = fopen(argv[1], "r");
  out = fopen(argv[2], "w");

  fread(map, sizeof(char), NX*NY, in);
  for (j = 0; j < NY; j++)
  {  for (i = 0; i < NX; i++)
     {  printf("i, j, val = %4d %4d %4d\n",i, j, (int) map[j][i] );
     }
  }

  return 0 ;

}
  
