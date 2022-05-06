#include "amsr2.h"
#include <stdio.h>
#include <stdlib.h>

void scanhead(amsr2pt *x, FILE *fin) ;

typedef struct {
  unsigned int year   : 12;
  unsigned int month  :  4;
  unsigned int day    :  6;
  unsigned int hour   :  5;
  unsigned int minute :  6;
  unsigned int second :  6;
} packed_bufr_date;

typedef struct {
  short int year;
  unsigned char month, day, hour, minute, second;
} bufr_date;

int main(void) {
  FILE *fin;
  amsr2pt x;
  amsr2_spot y;

  printf("size of bufrdate : %d\n", (int) sizeof(bufr_date));
  printf("size of packed : %d\n", (int) sizeof(packed_bufr_date));

  printf("size of amsr2 point: %d\n", (int) sizeof(x));

  printf("size of amsr2 satid: %d\n", (int) sizeof(x.satid));
  printf("size of amsr2 point: %d\n", (int) sizeof(x.year));
  printf("size of amsr2 point: %d\n", (int) sizeof(x.month));
  printf("size of amsr2 point: %d\n", (int) sizeof(x.day));
  printf("size of amsr2 point: %d\n", (int) sizeof(x.hour));
  printf("size of amsr2 point: %d\n", (int) sizeof(x.minute));
  printf("size of amsr2 point: %d\n", (int) sizeof(x.second));
  printf("size of amsr2 point: %d\n", (int) sizeof(x.clat));
  printf("size of amsr2 point: %d\n", (int) sizeof(x.clon));
  printf("size of amsr2 point: %d\n", (int) sizeof(x.nspots));
  printf("size of amsr2 obs:   %d\n", (int) sizeof(x.obs));

  printf("size of amsr2 spot: %d\n", (int) sizeof(amsr2_spot));

  return 0;
}
  

void scanhead(amsr2pt *x, FILE *fin) {
  fread(x, sizeof(amsr2pt), 1, fin);
  x->obs = malloc(x->nspots*sizeof(amsr2_spot));

  return;
}
