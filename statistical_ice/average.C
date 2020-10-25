#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  northgrid<float> ins[10], avg;
  FILE *fin, *fout;
  ijpt loc;
  int i, years, count;

//  for (i = 0; i < argc; i++) {
//    printf("i %d argv %s\n",i,argv[i]);
//  }

  for (i = 2; i < argc; i++) {
    fin = fopen(argv[i],"r");
    ins[i-2].binin(fin);
    fclose(fin);
  }

  for (loc.j = 0; loc.j < avg.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < avg.xpoints(); loc.i++) {
     count = 0;
     for (i = 2; i < argc; i++) {
       //if (ins[i-2][loc] != 0. && ins[i-2][loc] <= 1.00) {
       if (ins[i-2][loc] <= 1.00) {
         count += 1;
         avg[loc] += ins[i-2][loc];
       }
     }  
     if (count != 0) {
       avg[loc] /= count;
     }
     else {
       avg[loc] = 2.24;
     }
  }
  }

  fout = fopen(argv[1],"w");
  avg.binout(fout);
  fclose(fout);

  return 0;
}
