#include <stdio.h>
#include <math.h>

#define nx 448
#define ny 304

int main(int argc, char *argv[])
{

  char cin[nx][ny];
  int  count[256];
  FILE *fp;
  
  int i, j;

  if (argc != 2) {
    printf("need the name of the file to count!\n");
    return -1;
  }

  fp = fopen(argv[1],"r");

  fread(&cin[0][0], sizeof(char), nx*ny, fp);

  for (i = 0; i < 256; i++)
  {  count[i] = 0;
  }

  for (i = 0; i < nx; i++)
  {  for (j = 0; j < ny; j++)
     {
       count[cin[i][j] ] += 1;
     }
  }


  for (i = 0; i < 256 ; i++)
  {  printf("%3d %6d\n",i,count[i]);
  }

  return 1;

}
