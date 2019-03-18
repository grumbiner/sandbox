#include <stdio.h>

#define NX_GLK 85
#define NY_GLK 85
#define ILAND 1
#define IWATER 0
#define OLAND 0
#define OWATER 1  

int main(int argc, char *argv[])
{
  FILE *fin, *outtsd, *outtss, *outmask, *outbathy;
  unsigned char imask[NY_GLK][NX_GLK];
  unsigned char omask[NY_GLK][NX_GLK];
  float tshal[NY_GLK][NX_GLK];
  float sshal[NY_GLK][NX_GLK];
  float tdeep[NY_GLK][NX_GLK];
  float sdeep[NY_GLK][NX_GLK];
  float bathy[NY_GLK][NX_GLK];

  int i, j;
  
  fin = fopen(argv[1], "r");
  outtss = fopen(argv[2], "w");
  outtsd = fopen(argv[3], "w");
  outmask = fopen(argv[4], "w");
  outbathy = fopen(argv[5], "w");

  fread(imask, sizeof(unsigned char), NX_GLK*NY_GLK, fin);

  for (j = 0; j < NY_GLK; j++) {
    for (i = 0; i < NX_GLK; i++) {
       if (imask[j][i] == ILAND ) {
         omask[j][i] = OLAND;
         tshal[j][i] = 0.0; 
         tdeep[j][i] = 0.0; 
         sshal[j][i] = 0.0; 
         sdeep[j][i] = 0.0; 
         bathy[j][i] = 0.0; 
       }
       else {
         omask[j][i] = OWATER;
         tshal[j][i] =  10.0; 
         tdeep[j][i] =   4.0; 
         sshal[j][i] =   0.0; 
         sdeep[j][i] =   0.0; 
         bathy[j][i] = 200.0; 
       }
       fprintf(outmask, "%1d", (int)omask[j][i]);
    }
    fprintf(outmask,"\n");
  }

  fwrite(tshal, sizeof(float), NX_GLK*NY_GLK, outtss);
  fwrite(sshal, sizeof(float), NX_GLK*NY_GLK, outtss);
  fwrite(tdeep, sizeof(float), NX_GLK*NY_GLK, outtsd);
  fwrite(sdeep, sizeof(float), NX_GLK*NY_GLK, outtsd);

  fwrite(bathy, sizeof(float), NX_GLK*NY_GLK, outbathy);

  return 0;

}
