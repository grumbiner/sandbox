#include <stdio.h>

/* Create a character array from a northern hemisphere bathymetry file 
   Useful for examining output of bathymetry generator in the absence
     of visualization tools.
   Robert Grumbine
   Last Modified 20 July 1995
*/

int main(int argc, char *argv[])
{
  float depth[93][77];
  int i, j, tmp;
  unsigned char map[93][77];
  FILE *fin, *fout;

  fin = fopen(argv[1],"r");
  j = fread(depth,  sizeof(float), 93*77, fin);
  printf("Read %d values\n",j);

  for (j = 0; j < 93; j++) {
    for (i = 0; i < 77; i++) {
       tmp = 6000;
       if (depth[j][i] < tmp) {
         tmp = depth[j][i];
       }
      
       map[j][i] = (char) ( tmp / 25. );
    }
  }

  fout = fopen("bathy.char","w");
  fwrite(map, sizeof(unsigned char), 93*77, fout);

  return 0;

}
