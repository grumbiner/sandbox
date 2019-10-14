#include <stdio.h>

#define COAST 195
#define LAND 157
#define SEA    0
#define SNX 345
#define SNY 355
#define NNX 385
#define NNY 465

int main(void)
{
  FILE *nin, *nout, *sout;
  short int nland[NNY][NNX], nsea[NNY][NNX], tmp;
  unsigned char  nfilt[NNY][NNX];
  short int sland[SNY][SNX], ssea[SNY][SNX];
  unsigned char  sfilt[SNY][SNX];
  int i,j, count;

  nin=fopen("north","r");
  fread(nland, sizeof(short int), NNX*NNY, nin);
  fread(nsea , sizeof(short int), NNX*NNY, nin);

  count = 0;
  for (j = 0; j < NNY; j++) {
    for (i = 0; i < NNX; i++) {
      if (nland[j][i] >= 5 && nsea[j][i] == 0 ) {
        nfilt[j][i] = LAND;
      }
      else if (nland[j][i] == 0 && nsea[j][i] >= 5) {
        nfilt[j][i] = SEA;
      }
      else {
        count += 1;
        tmp = nland[j][i] + nsea[j][i]; 
        if (tmp == 0 ) {
          nfilt[j][i] = 224;
          continue;
        }
        if ( (float) nland[j][i] / tmp > 0.50 ) { 
          nfilt[j][i] = LAND;
        }
        else if ( (float) nsea[j][i] / tmp < 0.25 ) {
          nfilt[j][i] = COAST;
        }
        else {
          nfilt[j][i] = COAST;
        } 
      }
          
    }
  }
 
  printf("Count of decided points %d\n",count);
  nout=fopen("nmask2","w");
  fwrite(nfilt, sizeof(unsigned char), NNX*NNY, nout);


/* Southern hemisphere side */
  nin=fopen("south","r");
  fread(sland, sizeof(short int), SNX*SNY, nin);
  fread(ssea , sizeof(short int), SNX*SNY, nin);

  count = 0;
  for (j = 0; j < SNY; j++) {
    for (i = 0; i < SNX; i++) {
      if (sland[j][i] >= 5 && ssea[j][i] == 0 ) {
        sfilt[j][i] = LAND;
      }
      else if (sland[j][i] == 0 && ssea[j][i] >= 5) {
        sfilt[j][i] = SEA;
      }
      else {
        count += 1;
        tmp = sland[j][i] + ssea[j][i]; 
        if (tmp == 0 ) {
          sfilt[j][i] = 224;
          continue;
        }
        if ( (float) sland[j][i] / tmp > 0.50 ) { 
          sfilt[j][i] = LAND;
        }
        else if ( (float) ssea[j][i] / tmp < 0.25 ) {
          sfilt[j][i] = COAST;
        }
        else {
          sfilt[j][i] = COAST;
        } 
      }
          
    }
  }
 
  printf("Count of decided points %d\n",count);
  sout=fopen("smask2","w");
  fwrite(sfilt, sizeof(unsigned char), SNX*SNY, sout);

  return 0;
} 
