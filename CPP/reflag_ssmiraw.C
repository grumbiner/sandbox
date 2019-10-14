#include <stdlib.h>
#include "icessmi.h"

#define NX (2*345)
#define NY (2*355)
//#define NX 770
//#define NY 930

int main(void) {
  ssmi  *team, *team2;
  FILE *fin;
  ijpt loc;
  int i;

  team  = (ssmi*) malloc (NX*NY*sizeof(ssmi));
  team2 = (ssmi*) malloc (NX*NY*sizeof(ssmi));

  fin = fopen("s3ssmi.20050518","r");
  fread(team2, NX*NY, sizeof(ssmi), fin);
  fclose(fin);

  fin = fopen("version_a/s3ssmi.20050518","r");
  fread(team, NX*NY, sizeof(ssmi), fin);
  fclose(fin);

  for (i = 0; i < NX*NY; i++) {
    if (team2[i].bar_conc < 128) {
      if (team2[i].bar_conc   > 100) team2[i].bar_conc   = 100;
      if (team2[i].conc_bar   > 100) team2[i].conc_bar   = 100;
      if (team2[i].hires_conc > 100) team2[i].hires_conc = 100;
      if (team2[i].old_conc   > 100) team2[i].old_conc   = 100;

      printf("%3d %3d %3d %3d  %4d\n",team2[i].bar_conc, team2[i].conc_bar, 
                               team2[i].hires_conc, team2[i].old_conc,
                   team2[i].bar_conc - team2[i].old_conc  ) ;
    }
  }

  return 0;
}
