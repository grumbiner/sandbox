#include <cstdio>
#include "avhrr.h"
//Robert Grumbine
//11 Dec 2008

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
void avhrrout(double *idst1, double *idst2, double *chan) {
#elif LINUX
void avhrrout_(double *idst1, double *idst2, double *chan) {
#else
void avhrrout_(double *idst1, double *idst2, double *chan) {
#endif
  avhrrpt x;
  x.year   = (short int)     idst1[0];
  x.month  = (unsigned char) idst1[1];
  x.day    = (unsigned char) idst1[2];
  x.hour   = (unsigned char) idst1[3];
  x.minute = (unsigned char) idst1[4];
  x.second = (unsigned char) idst1[5];
  x.sstype = (unsigned char) idst1[6];
  x.sstsrc = (unsigned char) idst1[7];
  x.satid = (short int) idst1[8];
  
  x.clat = idst2[0];
  x.clon = idst2[1];
  x.sst  = idst2[2];
  x.irel = (unsigned char) idst2[3];
  x.rms  = (short int)     idst2[4];
  x.soza = idst2[5];
  x.saza = idst2[6];
  x.solazi = idst2[7];
  if (idst2[8] > 1e6) idst2[8] = 1e6;
  x.opth   = (int) idst2[8];

  for (int j = 0; j < 5; j++) {
    x.obs[j].channel = (unsigned char) chan[j*3 + 0];
    if (chan[j*3 + 1] > 1e6) chan[j*3 + 1] = 255;
    x.obs[j].albedo = (unsigned char) chan[j*3 + 1];
    if (chan[j*3 + 2] > 1e6) chan[j*3 + 2] = 999.0;
    x.obs[j].tmbr = chan[j*3 + 2];
  }

  fwrite(&x, sizeof(avhrrpt), 1, fout);

  return;
}
