#include <stdio.h>

#include "ssmi85.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  bufr_line line;
  int i, k;

  fin = fopen(argv[1], "r");

  while (!feof(fin) ) {
    fread(&line, sizeof(bufr_line), 1, fin);
    printf("%3d %4d %2d %2d %2d %2d %2d %d\n",line.satno, line.year, line.month, 
       line.day, line.hour, line.mins, line.secs, line.scan_no);

    for (i = 0; i < NSCANS; i++) {
       printf("%d %6.2f %7.2f %d %d %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f\n",
         line.full[i].scan_counter, 
         line.full[i].latitude, line.full[i].longitude, 
         line.full[i].surface_type, line.full[i].position_num,
         line.full[i].t19v, line.full[i].t19h, line.full[i].t22v, 
         line.full[i].t37v, line.full[i].t37h,
         line.full[i].t85v, line.full[i].t85h);
       for (k = 0; k < 3; k++) {
         printf("%d %6.2f %7.2f %d %d %6.2f %6.2f\n",
           line.full[i].hires[k].kwrit,
           line.full[i].hires[k].latitude, 
           line.full[i].hires[k].longitude, 
           line.full[i].hires[k].sftg, 
           line.full[i].hires[k].posn, 
           line.full[i].hires[k].t85v, 
           line.full[i].hires[k].t85h);
       }
       printf("\n");
    }
    printf("\n");

  }

  return 0;
} 
