#include <stdio.h>
#include "avhrr.h"

FILE *fout;

#ifdef IBM
int openout(int *unit) {
#elif LINUX
int openout_(int *unit) {
#else
int openout_(int *unit) {
#endif
  char fname[12];
  sprintf(fname,"fort.%02d\0",*unit);
  fout = fopen(fname, "w");
  if (fout == (FILE *) NULL) {
    printf("Failed to open output unit %d\n",*unit);
    return 1;
  }
  return 0;
}

#ifdef IBM
//void avhrrout(double *idst1, double *idst2, double *chan) {
void avhrrout(double *dst, double *chan) {
#elif LINUX
void avhrrout_(double *dst, double *chan) {
#else
void avhrrout_(double *dst, double *chan) {
#endif
  avhrrpt x;
  int j;

  x.year   = (short int)     dst[0];
  x.month  = (unsigned char) dst[1];
  x.day    = (unsigned char) dst[2];
  x.hour   = (unsigned char) dst[3];
  x.minute = (unsigned char) dst[4];
  x.second = (unsigned char) dst[5];
  x.sstype = (unsigned char) dst[6];
  x.sstsrc = (unsigned char) dst[7];
  x.satid = (short int) dst[8];
  
  x.clat = dst[9];
  x.clon = dst[10];
  x.sst  = dst[11];
  x.irel = (unsigned char) dst[12];
  x.rms  = (short int)     dst[13];
  x.soza = dst[14];
  x.saza = dst[15];
  x.solazi = dst[16];
  if (dst[8] > 1e6) dst[17] = 1e6;
  x.opth   = (int) dst[17];

  for (j = 0; j < 5; j++) {
    x.obs[j].channel = (unsigned char) chan[j*3 + 0];
    if (chan[j*3 + 1] > 1e6) chan[j*3 + 1] = 255;
    x.obs[j].albedo = (unsigned char) chan[j*3 + 1];
    if (chan[j*3 + 2] > 1e6) chan[j*3 + 2] = 999.0;
    x.obs[j].tmbr = chan[j*3 + 2];
  }

  //if (x.year != 2008) {
  //  printf("year = %d dst.0 = %e ",x.year, dst[0]);
  //}
  fwrite(&x, sizeof(avhrrpt), 1, fout);

  return;
}
