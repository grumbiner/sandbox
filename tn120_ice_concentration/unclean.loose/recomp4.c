#include <stdio.h>
#include <math.h>
#include "icessmi.h"
#include "icegrids.h"

float gr37(const ssmi *map, const int i, const int j, 
           const int nx, const int ny, const int range);
float gr22(const ssmi *map, const int i, const int j, 
           const int nx, const int ny, const int range);
int newfilt(ssmi *nmap, ssmi *smap);


int main(int argc, char *argv[])
{
  FILE *nin, *nout, *sin, *sout;
  ssmi nmap[NY_NORTH][NX_NORTH], smap[NY_SOUTH][NX_SOUTH];
  unsigned char nconc[NY_NORTH][NX_NORTH], sconc[NY_SOUTH][NX_SOUTH];
  int i, j, gradn[NY_NORTH][NX_NORTH], lapln[NY_NORTH][NX_NORTH];
  float t19v, t19h, t22v, t37v, t37h, t85v, t85h;
  float g37[NY_NORTH][NX_NORTH];
  int debug;
  
  debug = (1 == 0);

  nin = fopen(argv[1], "r");
  sin = fopen(argv[2], "r");
  nout = fopen(argv[3], "w");
  sout = fopen(argv[4], "w");
  if (nin == NULL || sin == NULL) {
    printf("Failed to open an input file\n");
    return -1;
  }
  if (nout == NULL || sout == NULL) {
    printf("Failed to open an output file\n");
    return -1;
  }

  i = fread(&nmap[0][0], sizeof(ssmi), NX_NORTH*NY_NORTH, nin);
  if (i != NX_NORTH*NY_NORTH) {
    printf("Failed to read in full northern map!\n");
    return -2;
  }
  i = fread(&smap[0][0], sizeof(ssmi), NX_SOUTH*NY_SOUTH, sin);
  if (i != NX_SOUTH*NY_SOUTH) {
    printf("Failed to read in full southern map!\n");
    return -2;
  }

  for (j = 0; j < NY_NORTH; j++) {
    for (i = 0; i < NX_NORTH; i++) {
       t19v = nmap[j][i].t19v / 100.;
       t19h = nmap[j][i].t19h / 100.;
       t22v = nmap[j][i].t22v / 100.;
       t37v = nmap[j][i].t37v / 100.;
       t37h = nmap[j][i].t37h / 100.;
       t85v = nmap[j][i].t85v / 100.;
       t85h = nmap[j][i].t85h / 100.;

       nmap[j][i].conc_bar = (int) (0.5 + 
            nasa_team(t19v, t19h, t22v, t37v, t37h, t85v, t85h, 'n') );

    }
  }

  for (j = 0; j < NY_SOUTH; j++) {
    for (i = 0; i < NX_SOUTH; i++) {
       t19v = smap[j][i].t19v / 100.;
       t19h = smap[j][i].t19h / 100.;
       t22v = smap[j][i].t22v / 100.;
       t37v = smap[j][i].t37v / 100.;
       t37h = smap[j][i].t37h / 100.;
       t85v = smap[j][i].t85v / 100.;
       t85h = smap[j][i].t85h / 100.;
       smap[j][i].conc_bar = (int) (0.5 + 
            nasa_team(t19v, t19h, t22v, t37v, t37h, t85v, t85h, 's') );
    }
  }

  if (debug) printf("Calling newfilt\n");
  newfilt(&nmap[0][0], &smap[0][0]);
  if (debug) printf("Returned from newfilt\n");

  getfld(&nmap[0][0], NX_NORTH*NY_NORTH, &nconc[0][0], &g37[0][0], CONC_BAR);
  fwrite(&nconc[0][0], sizeof(unsigned char), NX_NORTH*NY_NORTH, nout);

  getfld(&smap[0][0], NX_SOUTH*NY_SOUTH, &sconc[0][0], &g37[0][0], CONC_BAR);
  fwrite(&sconc[0][0], sizeof(unsigned char), NX_SOUTH*NY_SOUTH, sout);
  
  return 0;
}
