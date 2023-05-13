#include <stdio.h>
#include <stdlib.h>

#include "icegrids.h"
#include "icessmi.h"

float hires(oldssmi *map, int nx, int ny, int polei, int polej, unsigned char *mask, unsigned char *hires_conc) ;  

int main(int argc, char *argv[]) {
  FILE *fin;
  oldssmi *map;
  unsigned char *mask, *hires_conc;
  int i;

  map = malloc(NY_NORTH*NX_NORTH*sizeof(oldssmi) );
  mask = malloc(NY_NORTH*NX_NORTH*sizeof(unsigned char) );
  hires_conc = malloc(NY_NORTH*NX_NORTH*sizeof(unsigned char) );

  fin = fopen(argv[1], "r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open %s\n",argv[1]);
    return 1;
  }
  fread(map, sizeof(oldssmi), NX_NORTH*NY_NORTH, fin);
  fclose(fin);
  fin = fopen(argv[2], "r"); 
  if (fin == (FILE *) NULL) {
    printf("Failed to open %s\n",argv[2]);
    return 1;
  }
  fread(mask, sizeof(unsigned char),  NX_NORTH*NY_NORTH, fin);
  fclose(fin);

  hires(map, NX_NORTH, NY_NORTH,  polei_NORTH, polej_NORTH, mask,
        hires_conc);
  
  for (i = 0; i < NX_NORTH*NY_NORTH; i++) {
    if (map[i].conc_bar != LAND && map[i].conc_bar != COAST && map[i].conc_bar != NO_DATA &&
        mask[i] != LAND && mask[i] != COAST &&
        hires_conc[i] != LAND && hires_conc[i] != COAST ) {
    printf("%3d  %3d  %3d  %4d\n",map[i].conc_bar, hires_conc[i], mask[i], hires_conc[i] - map[i].conc_bar);
    }
  }

  return 0;
}
