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
  float tland, tsea;

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
        if ( (float) nland[j][i] / tmp > 0.15 ) { 
          nfilt[j][i] = LAND;
        }
        else if ( (float) nland[j][i] / tmp > 0.00 ) {
          nfilt[j][i] = COAST;
        }
        else {
          nfilt[j][i] = SEA;
        } 
      }
          
    }
  }

  for (j = 1; j < NNY - 1; j++) {
    for (i = 1; i < NNX - 1; i++) {
       tland = nland[j][i] + nland[j][i+1] + nland[j][i-1] +
                             nland[j+1][i] + nland[j-1][i] +
                             nland[j+1][i+1] + nland[j+1][i-1] +
                             nland[j-1][i+1] + nland[j+1][i-1];
       tsea = nsea[j][i] + nsea[j][i+1] + nsea[j][i-1] +
                             nsea[j+1][i] + nsea[j-1][i] +
                             nsea[j+1][i+1] + nsea[j+1][i-1] +
                             nsea[j-1][i+1] + nsea[j+1][i-1];
       if ( tland / (tland+tsea) > 0.05 && nfilt[j][i] == SEA ) {
             nfilt[j][i] = COAST;
             count += 1;
       }
    }
  }

 
  printf("Count of decided points %d\n",count);
  nout=fopen("nmask3","w");
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
        if ( (float) sland[j][i] / tmp > 0.15 ) { 
          sfilt[j][i] = LAND;
        }
        else if ( (float) sland[j][i] / tmp > 0.00 ) {
          sfilt[j][i] = COAST;
        }
        else {
          sfilt[j][i] = SEA;
        } 
      }
          
    }
  }
  for (j = 1; j < SNY - 1; j++) {
    for (i = 1; i < SNX - 1; i++) {
       tland = sland[j][i] + sland[j][i+1] + sland[j][i-1] +
                             sland[j+1][i] + sland[j-1][i] +
                             sland[j+1][i+1] + sland[j+1][i-1] +
                             sland[j-1][i+1] + sland[j+1][i-1];
       tsea = ssea[j][i] + ssea[j][i+1] + ssea[j][i-1] +
                             ssea[j+1][i] + ssea[j-1][i] +
                             ssea[j+1][i+1] + ssea[j+1][i-1] +
                             ssea[j-1][i+1] + ssea[j+1][i-1];
       if ( tland / (tland+tsea) > 0.05 && sfilt[j][i] == SEA ) {
             sfilt[j][i] = COAST;
             count += 1;
       }
    }
  }
 
  printf("Count of decided points %d\n",count);
  sout=fopen("smask3","w");
  fwrite(sfilt, sizeof(unsigned char), SNX*SNY, sout);

  return 0;
} 
