#include <stdio.h>

const int NX=345;
const int NY=355;

int main(int argc, char *argv[])
{
  FILE *in, *out;
  float x[NY][NX];
  char  y[NY][NX];
  int i, j;

  in = fopen(argv[1], "r");
  out = fopen(argv[2], "w");
  i = fread(y, sizeof(char), NX*NY, in);
  if (i != NX*NY) {
    printf("failed to read in the required data, only %d of %d\n",i, NX*NY);
    return -1;
  }

  for (i = 0; i < NX; i++)
  {  for (j = 0; j < NY; j++)
     {
       x[j][i] = (float) y[j][i];
     }
  }

  i = fwrite(x, sizeof(float), NX*NY, out);
  if (i != NX*NY) {
    printf("failed to write out the required data, only %d of %d\n",i, NX*NY);
    return -1;
  }
  
  return 0;

}
