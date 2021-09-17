#include <stdio.h>
#include <stdlib.h>

#define ILAND 1
#define IWATER 0
#define OLAND 0
#define OWATER 1  

void mclean(unsigned char *imask, unsigned char *vmask, const int nx, const int ny);

int main(int argc, char *argv[])
{
  FILE *fin, *outshal, *outdeep, *outmask, *outbathy;
  int divi, NY_GLK, NX_GLK;

  float *ibathy;

  unsigned char *omask;
  unsigned char *vmask;
  float *tshal;
  float *sshal;
  float *tdeep;
  float *sdeep;
  float *bathy;

  int i, j, revindex, index;
  
  fin = fopen(argv[1], "r");
  outshal = fopen(argv[2], "w");
  outdeep = fopen(argv[3], "w");
  outmask = fopen(argv[4], "w");
  outbathy = fopen(argv[5], "w");
  divi = atoi(argv[6]);
  NX_GLK = 512 / divi;
  NY_GLK = 512 / divi;

  omask = malloc(NX_GLK*NY_GLK*sizeof(unsigned char) );
  vmask = malloc(NX_GLK*NY_GLK*sizeof(unsigned char) );
  tshal = malloc(NX_GLK*NY_GLK*sizeof(float) );
  sshal = malloc(NX_GLK*NY_GLK*sizeof(float) );
  tdeep = malloc(NX_GLK*NY_GLK*sizeof(float) );
  sdeep = malloc(NX_GLK*NY_GLK*sizeof(float) );
  bathy = malloc(NX_GLK*NY_GLK*sizeof(float) );
  ibathy = malloc(NX_GLK*NY_GLK*sizeof(float) );

  fread(ibathy, sizeof(float), NX_GLK*NY_GLK, fin);

  for (j = 0; j < NY_GLK; j++) {
    for (i = 0; i < NX_GLK; i++) {
       index = i + j*NX_GLK;
       revindex = i + (NY_GLK-1-j)*NX_GLK;
       if (ibathy[index] <= 5. ) {
         omask[revindex] = OLAND;
         tshal[revindex] = 0.0; 
         tdeep[revindex] = 0.0; 
         sshal[revindex] = 0.0; 
         sdeep[revindex] = 0.0; 
         bathy[revindex] = 0.0; 
       }
       else {
         omask[revindex] = OWATER;
         tshal[revindex] =  10.0; 
         tdeep[revindex] =   4.0; 
         sshal[revindex] =   0.0; 
         sdeep[revindex] =   0.0; 
         bathy[revindex] = ibathy[index];
       }
    }
  }

    
  mclean(omask, vmask, NX_GLK, NY_GLK);

  for (j = 0; j < NY_GLK - 1; j++ ) {
    for (i = 0; i < NX_GLK - 1; i++) {
       index = i + j*NX_GLK;
       fprintf(outmask, "%1d", (int)vmask[index]);
    }
    fprintf(outmask,"\n");
  }

  for (j = 0; j < NY_GLK ; j++ ) {
    for (i = 0; i < NX_GLK ; i++) {
       index = i + j*NX_GLK;
       fprintf(outmask, "%1d", (int)omask[index]);
    }
    fprintf(outmask,"\n");
  }
  for (j = 0; j < NY_GLK ; j++ ) {
    for (i = 0; i < NX_GLK ; i++) {
       index = i + j*NX_GLK;
       fprintf(outmask, "%1d", (int)omask[index]);
    }
    fprintf(outmask,"\n");
  }

  for (j = 0; j < NY_GLK ; j++ ) {
    for (i = 0; i < NX_GLK ; i++) {
       index = i + j*NX_GLK;
       if ( (int) omask[index] == OLAND ) bathy[index] = 1.0;
    }
  }

  fwrite(tshal, sizeof(float), NX_GLK*NY_GLK, outshal);
  fwrite(sshal, sizeof(float), NX_GLK*NY_GLK, outshal);

  fwrite(tdeep, sizeof(float), NX_GLK*NY_GLK, outdeep);
  fwrite(sdeep, sizeof(float), NX_GLK*NY_GLK, outdeep);

  fwrite(bathy, sizeof(float), NX_GLK*NY_GLK, outbathy);

  return 0;

}

void mclean(unsigned char *imask, unsigned char *vmask, const int nx, const int ny)
{
  int i, j, rmask;
  int index, im1, ip1, jm1, jp1, ijp1;

  for (j = 1; j < ny-1; j++) {
    for (i = 1; i < nx - 1; i++) {
      index = i + j*nx;
      im1 = index - 1;
      ip1 = index + 1;
      jm1 = index - nx;
      jp1 = index + nx;
      if ( imask[index] == OWATER &&
           imask[im1  ] == OLAND  &&
           imask[ip1  ] == OLAND  &&
           imask[jm1  ] == OLAND ) {
        imask[index] = OLAND;
      }
    }
  }

  for (j = 0; j < ny - 1; j++) {
    for ( i = 0; i < nx - 1; i++) {
      rmask = 0;
      index = i + j*nx;
      ip1 = index + 1;
      jp1 = index + nx;
      ijp1 = index + nx + 1;
      if (imask[index] == OWATER) rmask += 1;
      if (imask[ip1  ] == OWATER) rmask += 1;
      if (imask[jp1  ] == OWATER) rmask += 1;
      if (imask[ijp1 ] == OWATER) rmask += 1;
      if (rmask >= 2) {
        vmask[index] = OWATER;
      }
      else {
        vmask[index] = OLAND;
      }

    }
  }

  return;

}
