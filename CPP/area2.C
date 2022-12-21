#include <stdlib.h>
#include "icessmi.h"
#include "ncepgrids.h"

float area(southhigh<unsigned char> &x) ;
float ice_extent(southhigh<unsigned char> &x) ;
void reflag(southhigh<unsigned char> &x) ;

int main(int argc, char *argv[]) {
  ssmi *team, *team2;
  southhigh<unsigned char> conc, conc2;
  FILE *fin;
  ijpt loc;
  int i;

  team  = (ssmi*) malloc(conc.xpoints()*conc.ypoints()*sizeof(ssmi));
  team2 = (ssmi*) malloc(conc.xpoints()*conc.ypoints()*sizeof(ssmi));
 
  fin = fopen(argv[1],"r");
  if (fin == (FILE *) NULL) {
    printf("couldn\'t open %s\n",argv[1]);
    return -1;
  }
  //fread(team2, conc.xpoints()*conc.ypoints(), sizeof(ssmi), fin);
  conc2.binin(fin);
  fclose(fin);
  printf("conc2 max, min average %d %d %d \n",conc2.gridmax(), conc2.gridmin(), conc2.average() );

  fin = fopen(argv[2],"r");
  //fread(team, conc.xpoints()*conc.ypoints(), sizeof(ssmi), fin);
  conc.binin(fin);
  fclose(fin);

  int count1 = 0, count2 = 0, count3 = 0, count4 = 0;

  for (i = 0; i < conc.xpoints()*conc.ypoints(); i++) {
    //conc2[i] = team2[i].hires_conc;
    //conc[i] = team[i].hires_conc;

    if ((int) conc2[i] < 128) {
      printf("conc2 %d %d\n",i,conc2[i]);
      if ((int) conc2[i] > 100) conc2[i] = 100;
    }
    if ((int) conc[i] < 128) {
      printf("concold %d %d\n",i,conc[i]);
      if ((int) conc[i]   > 100) conc[i]  = 100;
    }

    if ((int) conc[i] >= 128) conc[i] = 0;
    if ((int) conc2[i] >= 128) conc2[i] = 0;
  }

//  reflag(conc2);
//  reflag(conc);

  printf("areas %f and %f\n",    area(conc2)/1.e12,   area(conc)/1.e12);
  printf("extents %f and %f\n",ice_extent(conc2)/1.e12, ice_extent(conc)/1.e12);
  return 0;
}

void reflag(southhigh<unsigned char> &x) {
  ijpt loc;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if ((int) x[loc] > 128) x[loc] = 157;
  }
  }
  return;
}

float area(southhigh<unsigned char> &x) {
  ijpt loc;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x[loc] > 128) x[loc] = 0;
  }
  }

  return x.integrate()/100.;
}

float ice_extent(southhigh<unsigned char> &x) {
  ijpt loc;

  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x[loc] > 128) x[loc] = 0;
    if (x[loc] > 0) x[loc] = 1;
  }
  }

  return x.integrate();
}
