#include <stdio.h>

#define COAST 195
#define LAND 157
#define SEA    0
#define NX 360
#define NY 180


int main(void)
{
  FILE *nin, *nout;
  short int gland[NY][NX], gsea[NY][NX], tmp;
  unsigned char  gfilt[NY][NX];
  int i,j, count;
  float tland, tsea;

  nin=fopen("globe","r");
  fread(gland, sizeof(short int), NX*NY, nin);
  fread(gsea , sizeof(short int), NX*NY, nin);

  count = 0;
  for (j = 0; j < NY; j++) {
    for (i = 0; i < NX; i++) {
      if (gland[j][i] >= 5 && gsea[j][i] == 0 ) {
        gfilt[j][i] = LAND;
      }
      else if (gland[j][i] == 0 && gsea[j][i] >= 5) {
        gfilt[j][i] = SEA;
      }
      else {
        count += 1;
        tmp = gland[j][i] + gsea[j][i]; 
        if (tmp == 0 ) {
          gfilt[j][i] = 224;
          continue;
        }
        if ( (float) gland[j][i] / tmp > 0.50 ) { 
          gfilt[j][i] = LAND;
        }
        else if ( (float) gland[j][i] / tmp > 0.25 ) {
          gfilt[j][i] = COAST;
        }
        else {
          gfilt[j][i] = SEA;
        } 
      }
          
    }
  }

  for (j = 1; j < NY - 1; j++) {
    for (i = 1; i < NX - 1; i++) {
       tland = gland[j][i] + gland[j][i+1] + gland[j][i-1] +
                             gland[j+1][i] + gland[j-1][i] +
                             gland[j+1][i+1] + gland[j+1][i-1] +
                             gland[j-1][i+1] + gland[j+1][i-1];
       tsea = gsea[j][i] + gsea[j][i+1] + gsea[j][i-1] +
                             gsea[j+1][i] + gsea[j-1][i] +
                             gsea[j+1][i+1] + gsea[j+1][i-1] +
                             gsea[j-1][i+1] + gsea[j+1][i-1];
       if ( tland / (tland+tsea) > 0.25 && gfilt[j][i] == SEA ) {
             gfilt[j][i] = COAST;
             count += 1;
       }
    }
  }
 
  printf("Count of decided points %d\n",count);
  nout=fopen("globe.mask","w");
  fwrite(&gfilt[0][0], sizeof(unsigned char), NX*NY, nout);

  return 0;
} 
