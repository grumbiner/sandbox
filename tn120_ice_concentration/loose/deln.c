#include <stdio.h>
#define NY 465
#define NX 385

int main(int argc, char *argv[])
{

   int i, j;
   FILE *in1, *in2, *out1;
   unsigned char map1[NY][NX], map2[NY][NX], map3[NY][NX];

   in1 = fopen(argv[1], "r");
   in2 = fopen(argv[2], "r");
   out1 = fopen(argv[3], "w");
 
   fread(map1, sizeof(unsigned char), NY*NX, in1);
   fread(map2, sizeof(unsigned char), NY*NX, in2);
   
   for (i = 0 ; i < NY; i++) {
     for (j = 0; j < NX; j++) {
        map3[i][j] = 128 + (map1[i][j] - map2[i][j]);
        if (map1[i][j] != 157 && map1[i][j] != 166 &&
            map2[i][j] != 157 && map2[i][j] != 168 && 
            !(map1[i][j] == 0 && map2[i][j] == 0)  && (map1[i][j] - map2[i][j]) != 0   ) {
          printf("%3d %3d  %3d %3d %3d\n", i, j, 
               map1[i][j], map2[i][j], map1[i][j] - map2[i][j] );

        }
     }
   }

   fwrite(map3, sizeof(unsigned char), NY*NX, out1);

   return 0;

}
