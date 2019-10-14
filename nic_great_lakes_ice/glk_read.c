#include <stdio.h>

#define NX 512
#define NY 512
#define MAXHEADER 2048
#define REDUCTION 4

int ireduce(int *x, float *y, const int nx, const int ny, const int reduction);

int main(int argc, char *argv[])
{
  FILE *glkin, *glkout;
  int grid1[NY][NX];
  float d1, grid2[NY/REDUCTION][NX/REDUCTION];
  char header[MAXHEADER];
  int i, j;

  glkin = fopen(argv[1], "r");
  glkout = fopen(argv[2], "w");
  if (glkin == NULL) { 
    printf("Failed to open the Great Lakes analysis file %s\n", argv[1]);
    return -1;
  }
  if (glkout == NULL) {
    printf("Failed to open the Great Lakes output file %s\n", argv[2]);
    return -2;
  }

  fgets(header, MAXHEADER, glkin);
  fgets(header, MAXHEADER, glkin);
  fgets(header, MAXHEADER, glkin);
  fgets(header, MAXHEADER, glkin);
  fgets(header, MAXHEADER, glkin);
  fgets(header, MAXHEADER, glkin);

  for (j = NY-1; j >= 0; j--) {
/*    printf("j = %d\n",j); */
    for (i = 0; i < NX - 1; i++) {
/*      printf("i = %d\n",i); */
      fscanf(glkin, "%2d", &grid1[j][i]);
    }
    i = NX-1;
    fscanf(glkin, "%2d\n", &grid1[j][i]);
  }
/*
  for (j = 0; j < NY; j++) {
    for (i = 0; i < NX; i++) {
       printf("%1d",grid1[j][i]);
    }
    printf("\n");
  }
*/

  ireduce(&grid1[0][0], &grid2[0][0], NX, NY, REDUCTION);
  for (j = 0; j < NY/REDUCTION; j++) {
    for (i = 0; i < NX/REDUCTION; i++) {
       printf("%1.0f",grid2[j][i]);
    }
    printf("\n");
  }

  d1 = (float)NX;
  fwrite(&d1, sizeof(float), 1, glkout);
  d1 = (float)NY;
  fwrite(&d1, sizeof(float), 1, glkout);
  fwrite(grid2, sizeof(float), NX*NY/REDUCTION/REDUCTION, glkout);

  return 0;

}
