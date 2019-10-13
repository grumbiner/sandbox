#include <stdio.h>

#define NX 122
#define NY 122
#define NZ   6
int main(void) {
  FILE *fin, *fout;
  int i, j, k, tally = 0;
  float alon[NX*NY*NZ], alat[NX*NY*NZ], mask[NX*NY*NZ];
  float tlon, tlat, tmask;

  fin = fopen("dc.ascii", "r");
  fout = fopen("dc.binary","w");
  for (k = 0; k < NZ; k++) {
  for (j = 0; j < NY; j++) {
  for (i = 0; i < NX; i++) {
     tally += 1;
     fscanf(fin, "%f %f %f\n",&tlon, &tlat, &tmask);
     alon[i+j*NX+k*NY*NX] = tlon; 
     alat[i+j*NX+k*NY*NX] = tlat; 
     mask[i+j*NX+k*NY*NX] = tmask; 
  }
  }
  }
  printf("Read in %d\n",tally); 
  tally = fwrite(alon, sizeof(float), NX*NY*NZ, fout);
  printf("Wrote out %d\n",tally);
  tally = fwrite(alat, sizeof(float), NX*NY*NZ, fout);
  printf("Wrote out %d\n",tally);
  tally = fwrite(mask, sizeof(float), NX*NY*NZ, fout);
  printf("Wrote out %d\n",tally);

  fflush(fout);

  printf("nx, ny, nz = %d %d %d\n",NX, NY, NZ);
  return 0;
}
