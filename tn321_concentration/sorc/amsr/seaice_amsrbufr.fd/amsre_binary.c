#include <stdio.h>
#include "amsr.h"

FILE *fout;

#ifdef IBM
int openout(int *unit) {
#elif LINUX
int openout_(int *unit) {
#else
int openout_(int *unit) {
#endif
  char fname[80];
  sprintf(fname,"fort.%02d\0",*unit);
  fout = fopen(fname, "w");
  if (fout == (FILE *) NULL) {
    printf("Failed to open output unit %d\n",*unit);
    return 1;
  }
  return 0;
}

/* amsreout: */
#ifdef IBM
void amsreout(double *posit, double *ident, double *chan) {
#elif LINUX
void amsreout_(double *posit, double *ident, double *chan) {
#else
void amsreout_(double *posit, double *ident, double *chan) {
#endif

  amsrept x;
  int bad = 0, j;
  int positnx = 9, positny = 5;

  #ifdef VERBOSE
  for (j = 0; j < positnx*positny; j++) {
    printf("%f ",(float) posit[j]);
  }
  printf("\n");
  #endif

  x.year   = (short int)     posit[1];
  x.month  = (unsigned char) posit[2];
  x.day    = (unsigned char) posit[3];
  x.hour   = (unsigned char) posit[4];
  x.minute = (unsigned char) posit[5];
  x.second = (unsigned char) posit[6];
  x.satid = (short int) ident[0];
  
  x.clat[0] = posit[7 + 0*positnx];
  x.clon[0] = posit[8 + 0*positnx];
  if (x.clat[0] > 90 || x.clon[0] > 360 || x.clon[0] < -360) bad += 1;

/* Pick off the low frequencies: */
  for (j = 0; j < 10; j++) {
    x.obs[j].channel = (unsigned char) chan[j*4 + 0];
    x.obs[j].tmbr    = chan[j*4 + 3];
    if (chan[j*4 + 3] > 350) bad += 1;
  }

/* skip the fake 50 ghz and bad 89 ghz channels */
/* embed the write-out here, as we are using 89 ghz locations, both */
  for (j = 16; j <= 17; j++) {
    x.obs[j-6].channel = (unsigned char) chan[j*4 + 0];
    x.obs[j-6].tmbr    = chan[j*4 + 3];
    if (chan[j*4 + 3] > 350) bad += 1;
    x.clat[0] = posit[7 + 3*positnx];
    x.clon[0] = posit[8 + 3*positnx];
    if (x.clat[0] > 90 || x.clon[0] > 360 || x.clon[0] < -360) bad += 1;
  }
  for (j = 18; j <= 19; j++) {
    x.obs[j-6].channel = (unsigned char) chan[j*4 + 0];
    x.obs[j-6].tmbr    = chan[j*4 + 3];
    if (chan[j*4 + 3] > 350) bad += 1;
    x.clat[1] = posit[7 + 4*positnx];
    x.clon[1] = posit[8 + 4*positnx];
    if (x.clat[1] > 90 || x.clon[1] > 360 || x.clon[1] < -360) bad += 1;
  }


  if (bad == 0) {
    fwrite(&x, sizeof(amsrept), 1, fout);

    // for debuggig and/or sending ascii to other programs
    #ifdef TESTER
      for (j = 0; j < 14; j++) {
        printf(" %5.1f",x.obs[j].tmbr);
      }
      for (j = 0; j < 2; j++) {
        printf(" %7.3f %8.3f  ",x.clat[j], x.clon[j]);
      }
      printf("\n");
    #endif

  }

  

  return;
}
