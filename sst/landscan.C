#include "ncepgrids.h"

void land_update(global_quarter<short int> &fl, global_quarter<short int> &land, int step);

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  global_quarter<short int> fl, land;
  int i;
  int x[4];
  
  land.set(1);

  for (i = 1; i < argc; i++) {
    fin = fopen(argv[i],"r");
    fread(&x, sizeof(int), 4, fin);
    fl.binin(fin);
    fclose(fin);
    printf("i= %5d\n",i); fflush(stdout);
    land_update(fl, land, i); 

  }

// flip the land on output, to accord with my orientation
  ijpt loc, tloc;
  for (loc.j = 0; loc.j < fl.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < fl.xpoints(); loc.i++) {
    tloc.j = fl.ypoints() - loc.j - 1;
    tloc.i = loc.i;
    fl[tloc] = land[loc];
  }
  }

// 0 = land, 1 = water
  fout = fopen("unified.land","w");
  fl.binout(fout);
  fclose(fout);


  return 0;
}
void land_update(global_quarter<short int> &fl, global_quarter<short int> &land, int step) {
  int i;
  int reyflag = -999;
  for (i = 0; i < fl.xpoints()*fl.ypoints(); i++) {
    if (land[i] == 1) {
      if (fl[i] < -300) {
        if (fl[i] != reyflag) {
          printf("illegal sst step %5d  %d %d\n",step, i, fl[i]);
          fflush(stdout);
          land[i] = 0;
        }
        else {
          printf("new land step %5d  %d %d\n",step, i, fl[i]);
          fflush(stdout);
          land[i] = 0;
        }
      }
    }
  }

  return;
}
