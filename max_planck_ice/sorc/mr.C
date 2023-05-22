#include <stdio.h>
#include <stdlib.h>
#include "ncepgrids.h"

int main(void) {
  FILE *fin1, *fin2;
  northgrid<float> ftn;
  northgrid<unsigned char> satland;
  char tstring[400], tmp[2];
  ijpt loc;

  fin1 = fopen("MASK","r");
  tmp[1] = '\0';
  for (loc.j = 0; loc.j < ftn.ypoints(); loc.j++) {
  //for (loc.j = ftn.ypoints()-1; loc.j >=0 ; loc.j-- ) {
    fgets(tstring, 400, fin1);
    //for (loc.i = 0; loc.i < ftn.xpoints()-1; loc.i++) {
    for (loc.i = 0; loc.i < ftn.xpoints(); loc.i++) {
      //fscanf(fin1, "%1d",&tmp);
      tmp[0] = tstring[loc.i];
      ftn[loc] = (float) atoi(tmp);
    }
    //loc.i = ftn.xpoints() - 1;
    //fscanf(fin1, "%1d\n",&tmp);
    //ftn[loc] = (float) tmp;
  }
  printf("max, min = %f %f\n",ftn.gridmax(), ftn.gridmin() );
  if (ftn.gridmax() != 1) {
    printf("gridmax not = 1, aborting\n");
    return 1;
  }
  
  fin2 = fopen("nland","r");
  satland.binin(fin2);
  fclose(fin2);

  for (loc.j = 0; loc.j < ftn.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ftn.xpoints(); loc.i++) {
    if ( (int) satland[loc] != 0 && ftn[loc] == 1 ||
         (int) satland[loc] == 0 && ftn[loc] == 0 ) {
      printf("%3d %3d  %3d %1.0f\n",
        loc.i, loc.j, (int) satland[loc], ftn[loc]);
    }
  }
  }
     
  return 0;
}
